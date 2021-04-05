#------------------------------------------------------------------------------
# meta analysis for summarizing all temporal and lagged effects
#==============================================================================
library(gghighlight)
library(ggpubr)
library(ggsci)
library(data.table)
library(metafor)
library(hrbrthemes)
library(rio)

here = getwd()

# target outcomes
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

# target treatment
list_treatment = paste0(c('access','must','pillmill','dayslimit','goodsam','naloxone'),'_tr')
list_treatment_label = c('PDMP access','mandatory PDMP','Pain Clinic','Days Limit','Good Samaritan','Naloxone Law')

# read pmatch results
coef_pmatch = fread(file.path(here,'results','coef_pmatch.csv'))

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

# readjust digits
tab[grepl('mortality',outcome), coef := coef * 3000]
tab[grepl('mortality',outcome), lci := lci * 3000]
tab[grepl('mortality',outcome), uci := uci * 3000]

tab[grepl('mortality',outcome) == FALSE, coef := coef * 100]
tab[grepl('mortality',outcome) == FALSE, lci := lci * 100]
tab[grepl('mortality',outcome) == FALSE, uci := uci * 100]
tab[,star := ifelse(pval < 0.05, '*','')]

fwrite(tab,file.path(here,'results','pmatch_meta_analysis.csv'))

