# ----------------------------------------------------------------------
# ' run panel matching across all treatments + all outcomes
# ----------------------------------------------------------------------

library(parallel)
library(data.table)
library(wfe)
library(PanelMatch)
library(survey)
library(purrr)

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

here = getwd()

# variables of interest
target_depvars = matrix(c(
    'opioid_mortality_per100k','All Opioid Mortality',
    'mortality_heroin','Heroin Mortality',
    'mortality_opioids','Natural Opioid Mortality',
    'mortality_methadone','Methadone Mortality',
    'mortality_synth','Synthetic Mortality',
    'mortality_cocaine','Cocaine Mortality',

    'overdosed_patients_per100k','P(Opioid Use Disorder and Overdose)',
    'total_opioid_patients_per100k','P(taking opioid)',
    'total_overlapping_per100k','P(overlapping claims)',
    'prop_high_mme','P(daily MME > 90)',
    'n_plusplus4_per100k','P(4+4 within 90 days)',
    'total_MAT_patients_per100k_addnaltrexon', 'P(taking MAT drug)'
    ), 
ncol=2, byrow=TRUE)

depvars_label = target_depvars[,2]
depvars = target_depvars[,1]

list_treatment = paste0(c('access','must','naloxone','pillmill','goodsam','dayslimit'),'_tr')
time_unit = 'quarter'

# load data 
data = fread(file.path(here,'data','reg-data',paste0('did_',time_unit,'_addmeth.csv')))

# adjust mortality data 
for (DV in depvars[1:6]) {
    data[,(DV) := get(DV)*10^5]
}

# load state-level controls 
scontrols = fread(file.path(here,'data','did-data',paste0('cps_state_control_',time_unit,'.csv')))
fips_codes = fread(file.path(here, 'data', 'tigris_fips_codes.csv'))
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
covariate = c(
    'p_female','p_age40_60','p_age60up','p_white','p_black','p_asian','p_hispanic',
    'p_unemployed','p_poverty','n_resident')
treatment = list_treatment[1]

run_pmatch = function(DV, treatment,covariate,
    outcome_lead=0:12,
    treatment_lag=8,
    balance_check_lag = 4) {

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

    for (i in all_i){

        treated_st_time = names(pmatch$att)[i]
        
        treated_st = as.integer(unlist(strsplit(treated_st_time,'\\.'))[1])
        treated_time = as.integer(unlist(strsplit(treated_st_time,'\\.'))[2])
        
        control_st_weight = attributes(pmatch$att[[i]])$weights
        control_st = data.table(state_code=as.integer(names(control_st_weight)),weight=control_st_weight)
    
        ctr_data = as.data.table(merge(data[,c('state_code','time_id',DV),with=FALSE],control_st,by='state_code'))

        if (nrow(ctr_data) > 0){

            ctr_data = ctr_data[, .(
                weighted_outcome = weighted.mean(get(DV), weight,na.rm=TRUE)
                ),by='time_id']
            setnames(ctr_data, 'weighted_outcome',DV)
            ctr_data[,type := 'matched']
            ctr_data[, time_id := time_id - treated_time]
        
            tr_data = as.data.table(data[state_code==treated_st,c('time_id',DV),with=FALSE])
            tr_data[,type := 'treated']
            tr_data[, time_id := time_id - treated_time]
            
            all_ctr_data = data[get(treatment) == 0 & state_code != treated_st,]
            all_ctr_data = all_ctr_data[, .(
                unweighted_outcome = mean(get(DV),na.rm=TRUE)
                ),by='time_id']
            setnames(all_ctr_data, 'unweighted_outcome',DV)
            all_ctr_data[,type := 'unmatched']
            all_ctr_data[, time_id := time_id - treated_time]
        
            long_data = rbind(tr_data,ctr_data, all_ctr_data)
            long_data[,treated_state := treated_st]
            list_long_data[[n]] = long_data
            n = n + 1
    
        }
    }

    list_long_data = rbindlist(list_long_data)
    outcome_balance = list_long_data[, .(mean_outcome = mean(get(DV),na.rm=TRUE)), by=c('type','time_id')]
    outcome_balance[, pre_post := ifelse(time_id < 0, 'pre','post')]
    
    # use safely for error controls
    safe_pmatch = safely(PanelMatch)

    list_coef_fit = list(); k = 1; list_error = list(); e = 1
    for (ll in outcome_lead){
        pmatch <- safe_pmatch(
            outcome.var = DV,
            covs.formula = covs.formula, 
            treatment = treatment,time.id = 'time_id', unit.id = 'state_code', 
            refinement.method = 'CBPS.weight',match.missing = TRUE, 
            verbose=TRUE,
            forbid.treatment.reversal = TRUE, matching=TRUE, lag=treatment_lag,
            qoi='att',  lead = ll, data=as.data.frame(data))
        
        if (is.null(pmatch$error)){

            pmatch.fit <- PanelEstimate(sets = pmatch$result,number.iterations = 1000, confidence.level = 0.95,
                    df.adjustment = FALSE,data=as.data.frame(data))
            coef = data.table(time_id = rownames(summary(pmatch.fit)$summary),summary(pmatch.fit)$summary)
            names(coef) = c('time_id','coef','se','lci','uci')

            coef$N_treated = summary(pmatch.fit$matched.sets)$number.of.treated.units
            coef$mean_matched_control = as.numeric(summary(pmatch.fit$matched.sets)$set.size.summary['Mean'])
            coef$total_control = sum(summary(pmatch.fit$matched.sets)$overview$matched.set.size)
            
            list_coef_fit[[k]] = coef
            k = k + 1

        } else {
            coef = data.table(time_id = paste0('t+',ll), coef=NA, se=NA, lci=NA, uci=NA,
                N_treated=NA_integer_,mean_matched_control=NA,total_control=NA_integer_)
            list_coef_fit[[k]] = coef
            k = k + 1        
            list_error[[e]] = list(DV=DV, treatment=treatment, lead=ll, error=pmatch$error)
            e = e + 1
        }
    }

    coef_fit = rbindlist(list_coef_fit)
    
    return(list(coef_fit=coef_fit, outcome_balance=outcome_balance,error=list_error))
}

# run models across all DV + treatments
list_out = list(); n = 1
for (DV in depvars){
    output = mclapply(list_treatment,function(treatment){
        message('=== now running for ',DV,' ',treatment,' ===')
        output = run_pmatch(DV,treatment, covariate=covariate)
        output$outcome = DV
        output$treatment = treatment
        return(output)
    }, mc.cores = 14)
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

# export balance output
outcome_balance = rbindlist(lapply(all_list_out, function(x) {
    out=x$outcome_balance
    out[,outcome := x$outcome]
    out[,treatment := x$treatment]
    }))

# export policy effects from panel matching
coef_fit = rbindlist(lapply(all_list_out, function(x) {
    out=x$coef_fit
    out[,outcome := x$outcome]
    out[,treatment := x$treatment]
    }))

fwrite(outcome_balance, file.path(here,'results','outcome_balance.csv'))
fwrite(coef_fit, file.path(here,'results','coef_pmatch.csv'))

