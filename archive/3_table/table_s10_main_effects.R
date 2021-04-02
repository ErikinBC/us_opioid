# ----------------------------------------------------------------------
# ' this file produces regression tables in appendix
# ----------------------------------------------------------------------

library(here)

# ---- load library 
library(ggridges)
library(ggpubr)
library(viridis)
library(huxtable)

here = getwd()

list_treatment = paste0(c('access','must','pillmill','dayslimit','goodsam','naloxone'),'_tr')
list_treatment_label = c('PDMP access','mandatory PDMP','Pain Clinic','Days Limit','Good Samaritan','Naloxone Law')

# variables of interest
target_depvars = matrix(c(
    'total_opioid_patients_per100k','P(taking opioid)',
    'total_overlapping_per100k','P(overlapping claims)',
    'prop_high_mme','P(daily MME > 90)',
    'n_plusplus4_per100k','P(4+4 within 90 days)',
    'overdosed_patients_per100k','P(Opioid Use Disorder and Overdose)',
    'total_MAT_patients_per100k_addnaltrexon', 'P(taking MAT drug)',

    'opioid_mortality_per100k','All Opioid Mortality',
    'mortality_opioids','Natural Opioid Mortality',
    'mortality_methadone','Methadone Mortality',
    'mortality_heroin','Heroin Mortality',
    'mortality_synth','Synthetic Mortality',
    'mortality_cocaine','Cocaine Mortality'
    ), 
ncol=2, byrow=TRUE)

depvars_label = target_depvars[1:12,2]
depvars = target_depvars[1:12,1]

coef_fit = fread(file.path(here,'results','coef_pmatch.csv'))

# drop se info here
coef_fit[,se := NULL]

list_huxtab = list(); n = 1
for (DV in depvars){    

    tab = coef_fit[outcome==DV,]
    tab[,outcome := NULL]
    if (DV %in% depvars[1:6]){
        tab[, coef := coef*3000]
        tab[, lci := lci*3000]
        tab[, uci := uci*3000]
    } else {
        tab[, coef := coef*100]
        tab[, lci := lci*100]
        tab[, uci := uci*100]
    }

    for (tr in list_treatment){
        tr_tab = tab[treatment == tr,]
        tr_tab[,treatment := NULL]
        tr_tab[,mean_matched_control := NULL]
        tr_tab = as_hux(tr_tab)
        
        tr_tab = insert_row(tr_tab, paste0(
            'Outcome ',depvars_label[match(DV,depvars)],', Policy ',list_treatment_label[match(tr,list_treatment)]),fill='',colspan=6)
        colnames(tr_tab)= c('time after policy change','Coef.','95% LCI','95% UCI','N treated','N total matched')
        tr_tab = tr_tab %>% add_colnames()
        for (j in 2:4) number_format(tr_tab)[2:nrow(tr_tab), j] =  '%9.2g'

        list_huxtab[[n]] = tr_tab
        n = n + 1
    }    
}

hux_tab = do.call(rbind, list_huxtab)

outfile = file.path(here,'results', paste0('table_s10_pmatch_all_mat.xlsx'))
quick_xlsx(hux_tab, file=outfile, open=FALSE)






