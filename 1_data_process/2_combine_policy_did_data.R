# ----------------------------------------------------------------------
# ' this file use cleaned policy timing table to identify treatment indicators
# ' treatment indicator (after certain policy change=1, otherwise 0)
# ----------------------------------------------------------------------
library(data.table)
library(zoo)

here = getwd()
dir_olu = file.path(here,'..')
dir_data = file.path(dir_olu,'data')
dir_figures = file.path(dir_olu,'figures')

# read fips code data
fips_codes = fread(file.path(dir_data, 'tigris_fips_codes.csv'))

# load pdmp policy data
pdmp_group = fread(file.path(dir_data,'policy-data','pdmp_group.csv'))
pdmp_group = merge(pdmp_group, unique(fips_codes[,c('state','state_name')]),by.x='state',by.y='state_name',all.x=TRUE)
setnames(pdmp_group,'state','state_name')
setnames(pdmp_group,'state.y','state')

pdmp_group = dcast(pdmp_group, state ~ type, fun=function(x) min(x), value.var = 'pdmp_date')
pdmp_group[,enactment := as.Date(enactment)]
pdmp_group[,access := as.Date(access)]
pdmp_group[,must := as.Date(must)]

# load other state law database
state_law = fread(file.path(dir_data,'policy-data','state_drug_policy.csv'))

# add medicaid laws 
medicaid_law = fread(file.path(dir_data,'policy-data','medicaid expansion dates.csv'))
medicaid_law[,notes_kff := NULL]
medicaid_law[, medicaid_date := as.Date(kff_implentation_date,format='%m/%d/%y')]
medicaid_law = medicaid_law[,c('state_abb','medicaid_date')]
setnames(medicaid_law,'state_abb','state')

# combine different policy data sets
policy = merge(x=pdmp_group, y=state_law, by='state',all.x=TRUE)
policy = merge(x=policy,y=medicaid_law,by.x="state",by.y="state",all.x=TRUE)
names(policy) = gsub('_date','',names(policy))

# adjust date format
list_laws = c('access','must','enactment','dayslimit','goodsam','pillmill','naloxone','medicaid')

for (laws in list_laws) {
	policy[, (laws) := as.Date(get(laws), format='%Y-%m-%d')]
	policy[get(laws) < as.Date('1999-01-01'), (laws) := as.Date('1999-01-01')]
}

# -- expand data sets into year-quarter 2007 to 2017 
create_target_period_dt = function(unit, from_year, to_year, type='quarter'){

	from_date = as.Date(paste0(from_year,'-01-01'))
	to_date = as.Date(paste0(to_year,'-12-31'))

	date_list = seq.Date(from = from_date, to = to_date, by = 1)
	unit = na.omit(unique(unit))

	target = expand.grid(unit, date_list)
	target = data.table(target)
	names(target) = c('unit','date')
	target[,year := year(date)]

	if (type == 'quarter'){
		target[,quarter := quarter(date)]
		target = unique(target[, c('year','quarter','unit')])
	} else if (type == 'month') {
		target[,month := month(date)]
		target = unique(target[, c('year','month','unit')])
	} else if (type == 'week'){
		target[,week := week(date)]
		target = unique(target[, c('year','week','unit')])		
	}
	return(target)
}


target_yq = create_target_period_dt(unit=policy$state, from_year=1999, to_year=2018)
setnames(target_yq,'unit','state')
target_yq = merge(x = target_yq, y = policy, by.x = "state", by.y = "state", all.x = TRUE)

# create treatment indicators
setorder(target_yq,state,year,quarter)
target_yq[,t0_date := zoo::as.yearqtr(paste0(year, ' Q',quarter), format = "%Y Q%q")]

for (law in list_laws) {
	target_yq[, (law) := zoo::as.yearqtr(get(law))]
	target_yq[[paste0(law,'_time')]] = (target_yq$t0_date - target_yq[[paste0(law)]]) * 4
	target_yq[[paste0(law,'_tr')]] = as.integer(target_yq$t0_date >= target_yq[[paste0(law)]])
}

# export data to outcome file
outfile_yq = file.path(dir_data,'reg-data','did_policy_quarter.csv')
fwrite(target_yq, outfile_yq)

old_yq = fread(file.path(dir_data,'reg-data','old_did_policy_quarter.csv'))

stopifnot(all(dim(old_yq) == dim(target_yq)))
for (cn in colnames(old_yq)) {
	print(cn)
	if (class(target_yq[[cn]]) == 'yearqtr') {
		stopifnot(all.equal(old_yq[[cn]], as.numeric(target_yq[[cn]])))
	} else {
		stopifnot(all.equal(old_yq[[cn]], target_yq[[cn]]))
	}
}

