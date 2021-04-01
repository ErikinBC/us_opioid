# ----------------------------------------------------------------------
# ' combines mortality + claims data + policy data + cps state-level controls
# ' generate regression table for did analysis (panel matching)
# ----------------------------------------------------------------------

library(data.table)

here = getwd()
dir_data = file.path(here,'..','data')

fips_codes = fread(file.path(dir_data, 'tigris_fips_codes.csv'))

# read policy data
time_unit = 'quarter'
agg_time_unit = c('year','quarter')	
policy = fread(file.path(dir_data,'reg-data',paste0('did_policy_',time_unit,'.csv')))

# read mortality data
mortality = fread(file.path(dir_data,'cdc-data',paste0('CDC_MCD_state_',time_unit,'_opioid_mortality_1999_2018_suppressed.csv')))
mortality[,state := NULL]
mortality = merge(mortality, unique(fips_codes[,c('state_code','state')]), by.x='statecode',by.y='state_code',all.x=TRUE)

cdata = dcast(mortality, state + year + quarter ~ icd_code, value.var = "crude_rate_measure")
setnames(cdata,c(paste0('T40.',1:6),'all'),paste0('death_',c(paste0('T40_',1:6),'all')))

mortality[death_count < 10, crude_rate_measure := 0]
cdata_imp = dcast(mortality, state + year + quarter ~ icd_code, value.var = "crude_rate_measure")
setnames(cdata_imp,c(paste0('T40.',1:6),'all'),paste0('death_censor_',c(paste0('T40_',1:6),'all')))

# read optum claims data
zip5_claims = fread(file.path(dir_data,'reg-data',paste0('claims_state_','zip5','_',time_unit,'.csv')))	

# merge different data
ddata = merge(policy, zip5_claims, by=c('state',agg_time_unit), all.x=TRUE,all.y=TRUE)
ddata = merge(ddata, cdata[,c('state',agg_time_unit,paste0('death_',c(paste0('T40_',1:6),'all'))),with=FALSE], by=c('state',agg_time_unit), all.x=TRUE,all.y=TRUE)
ddata = merge(ddata, cdata_imp[,c('state',agg_time_unit,paste0('death_censor_',c(paste0('T40_',1:6),'all'))),with=FALSE], by=c('state',agg_time_unit), all.x=TRUE,all.y=TRUE)

# rename variables
setnames(ddata,'death_all','opioid_mortality_per100k')
setnames(ddata,'death_T40_1','mortality_heroin')
setnames(ddata,'death_T40_2','mortality_opioids')
setnames(ddata,'death_T40_3','mortality_methadone')
setnames(ddata,'death_T40_4','mortality_synth')
setnames(ddata,'death_T40_5','mortality_cocaine')
setnames(ddata,'death_T40_6','mortality_other')

setnames(ddata,'death_censor_T40_1','censored_mortality_heroin')
setnames(ddata,'death_censor_T40_2','censored_mortality_opioids')
setnames(ddata,'death_censor_T40_3','censored_mortality_methadone')
setnames(ddata,'death_censor_T40_4','censored_mortality_synth')
setnames(ddata,'death_censor_T40_5','censored_mortality_cocaine')
setnames(ddata,'death_censor_T40_6','censored_mortality_other')

path_ddata = file.path(dir_data,'reg-data',paste0('did_',time_unit,'_addmeth.csv'))
fwrite(ddata, path_ddata)

old_ddata = fread(file.path(dir_data,'reg-data',paste0('old_did_',time_unit,'_addmeth.csv')))

setdiff(colnames(old_ddata),colnames(ddata))
setdiff(colnames(ddata),colnames(old_ddata))
