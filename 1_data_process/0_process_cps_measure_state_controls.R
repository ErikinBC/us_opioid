#------------------------------------------------------------------------------
# measure state-level control variables from CPS data
#==============================================================================

library(ipumsr)
library(data.table)
library(fst)

# specify your project root directory here
here = getwd()

dir_data = file.path(here,'..','data')

# read cps data
# check _read_cps_data.R file to convert the rawdata to fst file format
sel_var = c('CPSIDP','YEAR','MONTH','STATEFIP','SEX','AGE','RACE','HISPAN','CITIZEN','MARST','EDUC','EMPSTAT','LABFORCE','FAMSIZE','FAMINC','ASECFLAG','WTFINL')
stopifnot(!any(duplicated(sel_var)))
path_cdata <- file.path(dir_data,'CPS','cps_2005_2019.fst')
cdata = read_fst(path_cdata,as.data.table=TRUE,columns=sel_var)
dim(cdata)

month_to_quarter = function(x) {
	q = ifelse(x %in% c(1,2,3), 1,
		ifelse(x %in% c(4,5,6), 2,
		ifelse(x %in% c(7,8,9), 3,
		ifelse(x %in% c(10,11,12), 4, NA))))
	return(q)
}

setnames(cdata,'YEAR','year')
setnames(cdata,'MONTH','month')
# identify weight variable
# cdata[, wt_cps := WTFINL]
setnames(cdata,'WTFINL','wt_cps')

# sample selection
cdata[is.na(ASECFLAG), ASECFLAG := 0L]
cdata = cdata[ASECFLAG != 1,]

# identify quarter
cdata[,quarter := month_to_quarter(month)]

# select relevant variables
# cdata = cdata[, ..sel_var]

# measure mean age, over age > 50, % female, % white, % black, % hispanic, % asian, % other race,
# % unemployed, % married, 
cdata[LABFORCE == 1 | LABFORCE == 2, unemployed := ifelse(EMPSTAT %in% c(21,22), 1L, 0L)]

# sex
cdata[SEX != 0, female := ifelse(SEX==2, 1, 0)]

# hispanic 
cdata[, hispanic := ifelse(HISPAN > 0 & HISPAN < 900, 1, 0)]

# race
cdata[RACE != 999, race := car::recode(RACE, "'100'='white'; '200'='black'; '651'='asian'; else='other'")]
cdata[, white := ifelse(hispanic == 0 & race == 'white', 1L, 0L)]
cdata[, black := ifelse(hispanic == 0 & race == 'black', 1L, 0L)]
cdata[, asian := ifelse(hispanic == 0 & race == 'asian', 1L, 0L)]
cdata[, other := ifelse(hispanic == 0 & race == 'other', 1L, 0L)]

# citizenship-related information
cdata[, bornUS := ifelse(CITIZEN == 1, 1,0)]
cdata[, noncitizen := ifelse(CITIZEN == 5, 1, 0)]

# years of education
cdata[EDUC == 1,educ_year := 0]
cdata[EDUC == 2,educ_year := 0]
cdata[EDUC == 3,educ_year := 0]
cdata[EDUC == 10,educ_year := 4]
cdata[EDUC == 20,educ_year := 6]
cdata[EDUC == 30,educ_year := 8]
cdata[EDUC == 40,educ_year := 9]
cdata[EDUC == 50,educ_year := 10]
cdata[EDUC == 60,educ_year := 11]
cdata[EDUC == 70,educ_year := 12]
cdata[EDUC == 71,educ_year := 12]
cdata[EDUC == 73,educ_year := 12]
cdata[EDUC == 81,educ_year := 14]
cdata[EDUC == 91,educ_year := 15]
cdata[EDUC == 92,educ_year := 15]
cdata[EDUC == 111,educ_year := 16]
cdata[EDUC == 123,educ_year := 18]
cdata[EDUC == 124,educ_year := 18]
cdata[EDUC == 125,educ_year := 20]

# define poverty based on income levels; depending on the household size
cdata[FAMINC < 990, poverty := 0]
cdata[FAMSIZE == 1 & FAMINC < 430, poverty := 1]
cdata[FAMSIZE == 2 & FAMINC < 540, poverty := 1]
cdata[FAMSIZE == 3 & FAMINC < 700, poverty := 1]
cdata[FAMSIZE == 4 & FAMINC < 700, poverty := 1]

cdata[, age_4060 := ifelse(40 < AGE & AGE <= 60, 1L, 0L)]
cdata[, age_60 := ifelse(AGE > 60, 1L, 0L)]

# married 
cdata[, married := ifelse(MARST %in% c(1,2), 1L, 0L)]

# half-year
cdata[,halfyear := ifelse(quarter %in% c(1,2), 1, 2)]

# aggregate measures by state + time
time_unit = 'quarter'
	
if (time_unit == 'quarter') agg_time = c('year','quarter')
if (time_unit == 'quarter') n_month = 3

sdata = cdata[, .(
	n_respondent = .N,
	n_resident = sum(wt_cps, na.rm=TRUE) / n_month,
	p_female = weighted.mean(female, wt_cps, na.rm=TRUE),
	p_white = weighted.mean(white, wt_cps, na.rm=TRUE),
	p_black = weighted.mean(black, wt_cps, na.rm=TRUE),
	p_asian = weighted.mean(asian, wt_cps, na.rm=TRUE),
	p_other = weighted.mean(other, wt_cps, na.rm=TRUE),
	p_hispanic = weighted.mean(hispanic, wt_cps, na.rm=TRUE),
	p_married = weighted.mean(married, wt_cps, na.rm=TRUE),
	p_bornUS = weighted.mean(bornUS, wt_cps, na.rm=TRUE),
	p_noncitizen = weighted.mean(noncitizen, wt_cps, na.rm=TRUE),
	mean_educ = weighted.mean(educ_year, wt_cps, na.rm=TRUE),
	p_college = weighted.mean(educ_year > 15, wt_cps, na.rm=TRUE),
	mean_age = weighted.mean(AGE, wt_cps, na.rm=TRUE),
	p_age40_60 = weighted.mean(age_4060, wt_cps, na.rm=TRUE),
	p_age60up = weighted.mean(age_60, wt_cps, na.rm=TRUE),
	p_poverty = weighted.mean(poverty, wt_cps, na.rm=TRUE),
	p_unemployed=weighted.mean(unemployed, wt_cps,na.rm=TRUE)), by=c(agg_time,'STATEFIP')]

fwrite(sdata, file.path(dir_data,'reg-data',paste0('cps_state_control_',time_unit,'.csv')))
# Compare to previous
old_sdata = fread(file.path(dir_data,'reg-data',paste0('old_cps_state_control_',time_unit,'.csv')))
stopifnot(dim(old_sdata) == dim(sdata))
stopifnot(all(colnames(old_sdata) == colnames(sdata)))
for (cn in colnames(sdata)) {
	print(cn)
	stopifnot(all.equal(sdata[[cn]], old_sdata[[cn]]))
}

