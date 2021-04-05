# Load packages
pckgs <- c('data.table','cowplot','magrittr','stringr','zoo','ggplot2','data.table','parallel','wfe','PanelMatch','survey','purrr')
for (pckg in pckgs) { library(pckg, character.only=TRUE) }

source('funs_support.R')
source('funs_matching.R')
# specify your project root directory here
here = getwd()
dir_olu = file.path(here,'..')
dir_data = file.path(dir_olu,'data')
dir_regdata = file.path(dir_data,'reg-data')

##################################
# --- (1) LOAD AND PREP DATA --- #

# Policies of intereet
cn_policy_lvl <- c('goodsam','naloxone') #'access','must','pillmill','dayslimit','medicaid'
cn_policy_lbl <- c('Good Samaritan Law','Naloxone Law') #'PDMP access','Mandatory PDMP','Pain Clinic Law','Prescription Limit Law','Medicaid Expansion'

# variables of interest
target_depvars = matrix(c(
    'opioid_mortality_per100k','All Opioid Mortality',
    'mortality_heroin','Heroin Mortality',
    'mortality_opioids','Natural Opioid Mortality',
    'mortality_methadone','Methadone Mortality',
    'mortality_synth','Synthetic Mortality',
    'mortality_cocaine','Cocaine Mortality'), ncol=2, byrow=TRUE)

depvars_label = target_depvars[,2]
depvars = target_depvars[,1]

list_treatment = paste0(c('access','must','naloxone','pillmill','goodsam','dayslimit'),'_tr')
time_unit = 'quarter'

# load data 
data = fread(file.path(dir_regdata,paste0('did_',time_unit,'_addmeth.csv')))
# adjust mortality data 
for (DV in depvars[1:6]) { data[,(DV) := get(DV)*1e5] }

# load state-level controls 
scontrols = fread(file.path(dir_regdata,paste0('cps_state_control_',time_unit,'.csv')))
fips_codes = fread(file.path(dir_data, 'tigris_fips_codes.csv'))
scontrols = merge(scontrols,unique(fips_codes[,c('state_code','state')]),all.x=TRUE, by.x='STATEFIP',by.y='state_code')

# re-specification of time-indicators to integer sequence
agg_time_unit = c('year','quarter')
data = merge(data, scontrols, by=c('state',agg_time_unit), all.x=TRUE, all.y=TRUE)
data[,time_id := year * 4 + quarter]
data[, time_id := as.integer(time_id - min(time_id)+1 )]

# sample selection based on the period
data = data[year >= 2007 & year <= 2018 & state != 'PR',]
# re-specification of state-indicators
data[, state_code := as.integer(as.factor(state))]
di_state = data[,list(.N),by=list(state,state_code)][,-'N']
di_time_id = data[,list(.N),by=list(t0_date,time_id)][,-'N']

# n_resident lines up with state population
data[,list(pop=max(n_resident)),by=list(state)][order(-pop)]

# Get the unique time of the outcomes
di_policy = melt(data,c('state'),cn_policy_lvl,'policy')
di_policy = di_policy[,list(t0_date=mean(value)),by=list(state,policy)]
di_policy[,max(t0_date,na.rm=T),by=policy]

########################################
# --- (2) RUN NON-PARAMERIC TRENDS --- #

# See equation (18) for the DiD estimator

# Covariates to determine "weighting" on
covariates = c('p_female','p_age40_60','p_age60up',
    'p_white','p_black','p_asian','p_hispanic',
    'p_unemployed','p_poverty','n_resident')
head(data[,c('state','year',covariates[1:3]),with=F])

# Use overall opioid death rate
depvar = "opioid_mortality_per100k"

lst_w = list()
lst_m = list() 
for (policy in cn_policy_lvl) {
    policy_tr = str_c(policy,'_tr')
    tmp_lst <- get_np_panel(depvar, policy_tr, covariates, treatment_lag=8)
    lst_w[[policy]] = tmp_lst$w
    lst_m[[policy]] = tmp_lst$m
    rm(tmp_lst)
}
# Combine weights
df_weight = rbindlist(lst_w)
df_weight[, `:=` (DV = factor(DV,depvars,depvars_label),treatment=factor(treatment,str_c(cn_policy_lvl,'_tr'),cn_policy_lbl))]
df_weight = merge(merge(df_weight,di_state,by.x='state_code_tr',by.y='state_code'),di_state,by.x='state_code_ctr',by.y='state_code')
setnames(df_weight,c('DV','treatment','state.x','state.y'),c('depvar','policy','state_tr','state_ctr'))
df_weight = df_weight[,-c('state_code_ctr','state_code_tr')]
# df_weight[,list(n=.N),by=list(policy,state_ctr)][order(-policy,-n)]

# Compare outcomes
df_np = rbindlist(lst_m)
df_np[, `:=` (DV = factor(DV,depvars,depvars_label),treatment=factor(treatment,str_c(cn_policy_lvl,'_tr'),cn_policy_lbl))]
df_np = merge(df_np,di_state,by.x='state_code_tr',by.y='state_code')
setnames(df_np,c('DV','treatment','state'),c('depvar','policy','state_tr'))
df_np[, state_code_tr := NULL]
df_np[1:3] %>% t
rm(list=c('lst_w','lst_m'))

df_weight[,list(n=length(unique(state_tr))),by=policy]
df_np[,list(n=length(unique(state_tr))),by=policy]

#################$$$$#######################
# --- (3) ANALYZE NON-PARAMERIC TRENDS --- #




##########################################
# --- (4) ONLINE LOGISTIC REGRESSION --- #

# How accurate is predicting Y_(it) given the lags of X_it-L, Z_it-L and Y_it-L? How much of this is because of knowing Y? Do the Z's help at all?
# See equation (13)
# "To estimate the propensity score, we first create a subset of the data, consisting of all treated observations and their matched control observations from the same year.""



