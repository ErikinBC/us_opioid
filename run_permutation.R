pckgs <- c('data.table','parallel','wfe','PanelMatch','survey','purrr',
           'gghighlight', 'ggpubr', 'ggsci', 'cowplot','viridis','colorspace',
           'metafor', 'hrbrthemes', 'rio')
for (pckg in pckgs) { library(pckg, character.only=TRUE) }

stime <- Sys.time()

source('funs_support.R')
source('funs_matching.R')

here = getwd()
dir_olu = file.path(here,'..')
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

# run weighted fixed-effects estimators
covariates = c('p_female','p_age40_60','p_age60up','p_white',
                'p_black','p_asian','p_hispanic',
                'p_unemployed','p_poverty','n_resident')


# run models across all DV + treatments
list_out = list(); n = 1
for (depvar in depvars) {
    print(sprintf('Running model for depvar: %s',depvar))
    output = mclapply(list_treatment,function(treatment) {
       message('=== now running for ',depvar,' ',treatment,' ===')
        output = run_pmatch(DV=depvar, treatment=treatment, covariate=covariates, df=data.table::copy(data))
        output$outcome = depvar
        output$treatment = treatment
        return(output)
    }, mc.cores = parallel::detectCores() - 2)
    list_out[[n]] = output
    n = n + 1
}

# combine output
all_list_out = list(); n = 1 
for (k in 1:length(list_out)){
    tmp = list_out[[k]]
    for (j in 1:length(tmp)){
        all_list_out[[n]] = tmp[[j]]
        n = n + 1
    }
}

# export policy effects from panel matching
coef_pmatch = rbindlist(lapply(all_list_out, function(x) {
    out=x$coef_fit
    out[,outcome := x$outcome]
    out[,treatment := x$treatment]
    }))

# fwrite(coef_pmatch, file.path(here,'results','coef_pmatch.csv'))

#########################################
# ----- 2_model/2_meta_analysis.R ----- #

# run meta analysis
list_out = list(); n = 1 
for (tr in list_treatment){
    for (dv in depvars){
        dat = coef_pmatch[outcome == dv & treatment == tr,]
        res.RE <- rma(yi=coef, sei=se, data=dat, method="REML")
        out = data.table(
                 coef=coef(res.RE) 
                ,se=res.RE$se
                ,pval=res.RE$pval
                ,lci=res.RE$ci.lb
                ,uci=res.RE$ci.ub
                ,treatment=tr
                ,outcome=dv)
        list_out[[n]] = out
        n = n + 1
    }
}
tab = rbindlist(list_out)
# # readjust digits (per 300 million)
# tab[grepl('mortality',outcome), coef := coef * 3000]
# tab[grepl('mortality',outcome), lci := lci * 3000]
# tab[grepl('mortality',outcome), uci := uci * 3000]
# tab[,star := ifelse(pval < 0.05, '*','')]

etime = Sys.time()
nsec = round(as.numeric(difftime(etime,stime,units='secs')))
print(sprintf('--------- SCRIPT TOOK %i SECONDS TO RUN -----------',nsec))









