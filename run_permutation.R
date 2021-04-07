args = commandArgs(trailingOnly=TRUE)
seed = as.integer(args[1])
print(sprintf('Seed: %i',seed))

stime <- Sys.time()
pckgs <- c('data.table','parallel','wfe','PanelMatch','survey','purrr','metafor')
for (pckg in pckgs) { library(pckg, character.only=TRUE) }

source('funs_support.R')
source('funs_matching.R')

here = getwd()
dir_olu = file.path(here,'..')
dir_output = file.path(dir_olu,'output')
dir_permute = file.path(dir_output,'permute')
dir_data = file.path(dir_olu,'data')
dir_regdata = file.path(dir_data,'reg-data')

######################################
# ----- 1_model_panel_matching ----- #

# variables of interest
target_depvars = matrix(c('opioid_mortality_per100k','All Opioid Mortality'), 
                        ncol=2, byrow=TRUE)
# 'total_opioid_patients_per100k','P(taking opioid)'

depvars_label = target_depvars[,2]
depvars = target_depvars[,1]

list_treatment = paste0(c('naloxone','goodsam'),'_tr')
list_treatment_label = c('Naloxone Law','Good Samaritan')

time_unit = 'quarter'

# load data 
data = fread(file.path(dir_regdata,paste0('did_',time_unit,'_addmeth.csv')))
# adjust mortality data 
for (depvar in depvars) { data[,(depvar) := get(depvar)*1e5] }

# load state-level controls 
scontrols = fread(file.path(dir_regdata,paste0('cps_state_control_',time_unit,'.csv')))
fips_codes = fread(file.path(dir_data, 'tigris_fips_codes.csv'))
scontrols = merge(scontrols,unique(fips_codes[,c('state_code','state')]),all.x=TRUE, by.x='STATEFIP',by.y='state_code')

# re-specification of time-indicators to integer sequence
agg_time_unit = c('year','quarter')
data = merge(data, scontrols, by=c('state',agg_time_unit), all.x=TRUE, all.y=TRUE)
data[,time_id := year * 4 + quarter]
data[, time_id := as.integer(time_id - min(time_id)+1 )]

# re-specification of state-indicators
data[, state_code := as.integer(as.factor(state))]

# sample selection based on the period
data = data[year >= 2007 & year <= 2018,]

# Scramble the dates if seed > 0
set.seed(seed)
if (seed > 0) {
    print('Scrambling policy adoption date')
    alt_lst_tr = gsub('_tr','',list_treatment)
    dates_tr = data[,lapply(.SD,function(x) mean(x)),by=state,.SDcols=alt_lst_tr]
    dates_tr = cbind(data.table(state=dates_tr$state),dates_tr[,lapply(.SD,function(x) sample(x)),.SDcols=alt_lst_tr])
    data = merge(data[,-alt_lst_tr,with=F],dates_tr,by='state')
    for (tr in list_treatment) {
        tr2 = gsub('_tr','',tr)
        data[[tr]] = ifelse(data$t0_date >= data[[tr2]], 1, 0)
    }
} else {
    print('Policy adoption will not be scrambled; seed==0')
}


# run weighted fixed-effects estimators
covariates = c('p_female','p_age40_60','p_age60up','p_white',
                'p_black','p_asian','p_hispanic',
                'p_unemployed','p_poverty','n_resident')


# run models across all DV + treatments
list_out = list()
i = 1
for (depvar in depvars) {
    for (treatment in list_treatment) {
        print(sprintf('outcome: %s',depvar))
        output = run_pmatch(DV=depvar, treatment=treatment, covariate=covariates, df=data.table::copy(data))
        output[, `:=` (outcome=depvar, treatment=treatment)]
        list_out[[i]] = output
        i = i + 1
    }
}
coef_pmatch = rbindlist(list_out)
coef_pmatch[, seed_idx := seed]
print(coef_pmatch)
fwrite(coef_pmatch,file.path(dir_permute,paste0('coef_pmatch_',seed,'.csv')))

#########################################
# ----- 2_model/2_meta_analysis.R ----- #

# run meta analysis
list_out = list(); n = 1 
for (depvar in depvars) {
    for (tr in list_treatment) {
        dat = coef_pmatch[outcome == depvar & treatment == tr,]
        res.RE <- rma(yi=coef, sei=se, data=dat, method="REML")
        out = data.table(coef=coef(res.RE), se=res.RE$se,pval=res.RE$pval, 
                         lci=res.RE$ci.lb, uci=res.RE$ci.ub, treatment=tr, outcome=depvar)
        list_out[[n]] = out
        n = n + 1
    }
}
tab = rbindlist(list_out)
tab[, seed_idx := seed]
print(tab)
fwrite(tab,file.path(dir_permute,paste0('tab_',seed,'.csv')))

# # readjust digits (per 300 million)
# tab[grepl('mortality',outcome), coef := coef * 3000]
# tab[grepl('mortality',outcome), lci := lci * 3000]
# tab[grepl('mortality',outcome), uci := uci * 3000]
# tab[,star := ifelse(pval < 0.05, '*','')]

etime = Sys.time()
nsec = round(as.numeric(difftime(etime,stime,units='secs')))
print(sprintf('--------- SCRIPT TOOK %i SECONDS TO RUN -----------',nsec))