# ----------------------------------------------------------------------
# ' this file reads regression tables and produces effects of policies on various outcomes
# ' using event-study designs
# ----------------------------------------------------------------------

library(parallel)
library(data.table)
library(wfe)
library(PanelMatch)
library(survey)
library(lfe)
library(ggplot2)
library(ggpubr)

here = getwd()

extract.coef <- function(model, type=NULL) {
  if (is.null(type)){
    s <- summary(model)
    names <- rownames(s$coef)
    co <- s$coef[, 1]
    se <- s$coef[, 2]
    pval <- s$coef[, 4]
  } else if (type=="bootstrap") {
    names = ""
    co = model$o.coef 
    se = sd(model$boots)
    pval = NA
  }

  tr = data.table(
    coef.names = names,
    coef = co,
    se = se,
    pvalues = pval)  
  return(tr)
}

# variables of interest
target_depvars = matrix(c(
    'opioid_mortality_per100k','All overdose',
    'mortality_opioids','Natural opioid',
    'mortality_methadone','Methadone',
    'mortality_heroin','Heroin',
    'mortality_synth','Synthetic opioid',
    'mortality_cocaine','Cocaine',

    'total_opioid_patients_per100k','P(taking opioid)',
    'total_overlapping_per100k','P(overlapping claims)',
    'prop_high_mme','P(daily MME > 90)',
    'n_plusplus4_per100k','P(4+4 within 90 days)',
    'overdosed_patients_per100k','P(OUD and Overdose)',
    'total_MAT_patients_per100k_addnaltrexon', 'P(taking MAT drug)'
    ), 
ncol=2, byrow=TRUE)

depvars_label = target_depvars[,2]
depvars = target_depvars[,1]


list_treatment = paste0(c('access','must','naloxone','pillmill','goodsam','dayslimit'),'_tr')

time_unit = 'quarter'
from_to = c(-4*3,4*3) # three years before and two years after

# load data 
data = fread(file.path(here,'data','reg-data',paste0('did_',time_unit,'_addmeth.csv')))
for (DV in depvars[1:6]) {data[,(DV) := get(DV)*10^5]}

# load state-level controls 
scontrols = fread(file.path(here,'data','reg-data',paste0('cps_state_control_',time_unit,'.csv')))
fips_codes = fread(file.path(here, 'data', 'tigris_fips_codes.csv'))
scontrols = merge(scontrols,unique(fips_codes[,c('state_code','state')]),all.x=TRUE, by.x='STATEFIP',by.y='state_code')

# re-specification of time-indicators
	agg_time_unit = c('year','quarter')
	data = merge(data, scontrols, by=c('state',agg_time_unit),all.x=TRUE,all.y=TRUE)
	data[,time_id := as.factor(year * 4 + quarter)]

# re-specification of statecode indicators 
	data[, state_code := as.integer(as.factor(state))]

data[, person_wt :=  1 / n_people]

# run weighted fixed-effects estimators
covariate = c(
    'p_female','p_age40_60','p_age60up','p_white','p_black','p_asian','p_hispanic',
    'p_bornUS','mean_educ','p_unemployed','p_college', 'p_married','p_poverty',
    'n_resident')

list_coef = list(); n = 1
for (treatment in list_treatment){
	
	# adjust non-adoption as -1
	time_var = gsub('_tr','_time',treatment)
	
	min = from_to[1];max = from_to[2]
		
	treated_state = unique(data[get(treatment)==1 ,state])
	data[, (paste0('ever_',treatment)) := ifelse(state %in% treated_state, 1, 0)]
	
	# adjust min/max depending on from-to specifications
	data[, (paste0('modified_',time_var)) := get(time_var)]
	data[get(time_var) < min,(paste0('modified_',time_var)) := min]
	data[get(time_var) > max,(paste0('modified_',time_var)) := max]
	data[get(paste0('ever_',treatment))==0,(paste0('modified_',time_var)) := -1]
		
	for (t in min:max) {
		label_t = ifelse (t < 0, 'L','F')
		data[, (paste0(time_var,'_',label_t,abs(t))) := ifelse(get(paste0('modified_',time_var)) == t, 1, 0)]	
	}
	
	time_dummies = c()
	for (t in min:max) {
		label_t = ifelse (t < 0, 'L','F')
		time_dummies = c(time_dummies,paste0(time_var,'_',label_t,abs(t)))
	}
	
	time_dummies = grep(paste0(time_var,'_','L1$'),time_dummies,value=TRUE,invert=TRUE)

	coef = mclapply(depvars,
		function(DV) {
	
			# run the model 
			out = felm(as.formula(paste0(DV,"~",paste0(c(covariate,time_dummies),collapse = '+'),
				'| state_code + time_id | 0 | state_code')),data=as.data.frame(data))
			coef = extract.coef(out)
			base_coef = data.table(coef.names=paste0(time_var,'_L1'),coef=0,se=0,pvalues=1)
			coef = rbind(coef,base_coef)
			coef[,treatment := treatment]
			coef[,outcome := DV]
			coef[,time_unit := time_unit]
			return(coef)
		}, mc.cores = 6)		
			list_coef[[n]] = rbindlist(coef); n = n + 1
}

# combine output
	tab = rbindlist(list_coef)

# extract date info
	extract_date = function(x) {
		y = unlist(lapply(strsplit(x,'_'),function(x) x[length(x)]))
		return(y)
	}
	tab[,date := readr::parse_number(extract_date(coef.names))]

	tab[grepl('L',coef.names),date := date * (-1)]
	tab = tab[!grepl('p_|^n_',coef.names),]
	
	tab[,effects := ifelse(coef>0, 'positive','negative')]
	tab[date == -1, effects := 'reference']
	tab[, lci := coef - 1.96*se]
	tab[, uci := coef + 1.96*se]
	tab[,sig := ifelse(lci > 0 | uci < 0,'sig','insig')]

fwrite(tab,file.path(here,'results','coef_event_study.csv'))
