#------------------------------------------------------------------------------
# export results from meta analysis
#==============================================================================
library(gghighlight)
library(ggpubr)
library(ggsci)
library(data.table)
library(metafor)
library(hrbrthemes)
library(rio)

here = getwd()

tab = fread(file.path(here,'results','pmatch_meta_analysis.csv'))

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

# create tables for appendix
tab[grepl('mortality',outcome),present := paste0(round(coef,1),' [',round(lci,1),',',round(uci,1),']',star)]
tab[grepl('mortality',outcome) == FALSE,present := paste0(round(coef,3),' [',round(lci,3),',',round(uci,3),']',star)]

tab_coef = dcast(tab, outcome ~ treatment,value.var ='present')

# change the order + label
tab_coef = tab_coef[order(match(outcome,depvars)),]
tab_coef[, outcome := depvars_label[match(outcome,depvars)]]
colnames(tab_coef) = list_treatment_label[match(colnames(tab_coef),list_treatment_label)]
tab_coef = tab_coef[, c('outcome','access_tr',   'must_tr', 'pillmill_tr', 'dayslimit_tr',    'goodsam_tr', 'naloxone_tr')]

export(tab_coef, file.path(here,'results','table_s8-s9_meta_effects.xlsx'))

