pckgs <- c('data.table','cowplot','magrittr','stringr','zoo','ggplot2','data.table','parallel','wfe','PanelMatch','survey','purrr')
for (pckg in pckgs) { library(pckg, character.only=TRUE) }

source('funs_support.R')
# specify your project root directory here
here = getwd()
dir_olu = file.path(here,'..')
dir_data = file.path(dir_olu,'data')
dir_regdata = file.path(dir_data,'reg-data')

##################################
# --- (1) LOAD AND PREP DATA --- #

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

# run weighted fixed-effects estimators
covariate = c('p_female','p_age40_60','p_age60up',
    'p_white','p_black','p_asian','p_hispanic',
    'p_unemployed','p_poverty','n_resident')

data[,c('state','year',covariate[1:3]),with=F]
# n_resident lines up with state population
data[,list(pop=max(n_resident)),by=list(state)][order(-pop)]
data[1] %>% t



###########################
# --- (2) RUN "MODEL" --- #

# DV=depvars[1];treatment=list_treatment[3]
# outcome_lead=0:12;treatment_lag=8;balance_check_lag = 4

# get_np_panel <- function(DV, treatment,covariate,
#     outcome_lead=0:12, treatment_lag=8, balance_check_lag = 4) {}
covs.formula = as.formula(paste0('~ ',
        'I(lag(',DV,', 1:12))',
        '+ medicaid_tr',
        '+ ',paste0('lag(',covariate,', 1)',collapse = '+')
        ))

# run PM matching
pmatch <- PanelMatch(
    outcome.var = DV,
    covs.formula = covs.formula, 
    treatment = treatment,time.id = 'time_id', unit.id = 'state_code', 
    refinement.method = 'CBPS.weight',match.missing = TRUE, 
    verbose=TRUE,
    forbid.treatment.reversal = TRUE, matching=TRUE, lag=treatment_lag,
    qoi='att',  lead = 0, data=as.data.frame(data))

# access mean balance over time on DV
all_i = 1:length(names(pmatch$att))
list_long_data = list(); n = 1
i=1
for (i in all_i) {
    # Get the treatment state and time
    treated_st_time = names(pmatch$att)[i]
    treated_st = as.integer(unlist(strsplit(treated_st_time,'\\.'))[1])
    treated_time = as.integer(unlist(strsplit(treated_st_time,'\\.'))[2])
    # Find out who the "controls" are
    control_st_weight = attributes(pmatch$att[[i]])$weights
    control_st = data.table(state_code=as.integer(names(control_st_weight)),weight=control_st_weight)
    ctr_data = as.data.table(merge(data[,c('state_code','time_id',DV),with=FALSE],control_st,by='state_code'))
    control_st[, `:=` (state_code_tr=treated_st, state_code_ctr=state_code, state_code=NULL )]
    # OUTCOME BALANCE
    # COVARIATE BALANCE

    if (nrow(ctr_data) > 0) {
        # (i) Calculates weighted 
        ctr_data = ctr_data[, .(
            weighted_outcome = weighted.mean(get(DV), weight,na.rm=TRUE)
            ),by='time_id']
        setnames(ctr_data, 'weighted_outcome',DV)
        ctr_data[,type := 'matched']
        ctr_data[, time_id := time_id - treated_time]
        
        # (ii) Is is the data 
        tr_data = as.data.table(data[state_code==treated_st,c('time_id',DV),with=FALSE])
        tr_data[,type := 'treated']
        tr_data[, time_id := time_id - treated_time]

        # (iii) 
        all_ctr_data = data[get(treatment) == 0 & state_code != treated_st,]
        all_ctr_data = all_ctr_data[, .(
            unweighted_outcome = mean(get(DV),na.rm=TRUE)
            ),by='time_id']
        setnames(all_ctr_data, 'unweighted_outcome',DV)
        all_ctr_data[,type := 'unmatched']
        all_ctr_data[, time_id := time_id - treated_time]

        # (iv) Save the data
        control_st
    }
    
}