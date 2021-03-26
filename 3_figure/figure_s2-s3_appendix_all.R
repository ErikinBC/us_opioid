#------------------------------------------------------------------------------
# plots in appendix : for each outcome 
# general patterns (outcome balancing check)
#==============================================================================
library(gghighlight)
library(ggpubr)
library(ggsci)
library(data.table)

here = getwd()

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


depvars_label = target_depvars[1:12,2]
depvars = target_depvars[1:12,1]

list_treatment = paste0(c('access','must','pillmill','dayslimit','goodsam','naloxone'),'_tr')
list_treatment_label = c('PDMP access','mandatory PDMP','Pain Clinic','Prescription Limit','Good Samaritan','Naloxone Law')

outcome_balance = fread(file.path(here,'results','outcome_balance.csv'))

tab = outcome_balance
tab[, pre_post := ifelse(time_id < 0, 'pre','post')]
tab[,outcome := factor(outcome,levels=depvars,labels=depvars_label)]
tab[,treatment := factor(treatment, levels=list_treatment,labels=list_treatment_label)]

tab[outcome %in% depvars_label[1:6],outcome_type := 'cdc data']
tab[outcome %in% depvars_label[7:12],outcome_type := 'claims data']

fig2 = ggplot(tab[-12 <= time_id & time_id <= 12 & outcome_type == 'claims data' & !is.na(treatment),], 
	aes(x=time_id,y=mean_outcome,alpha=pre_post,shape=type,color=type)) +
    geom_point()+
    geom_line() + 
    geom_vline(xintercept=0,linetype='dotted')+
    #scale_color_nejm()+
    scale_alpha_manual(values=c('pre'=0.5,'post'=1))+
    scale_color_manual(values=c('treated'='red','matched'='blue','unmatched'='black'))+
    theme_pubr() + 
	guides(alpha=FALSE, 
		fill=guide_legend(title=NULL),
		color=guide_legend(title=NULL),
		shape=FALSE,
		linetype=FALSE)+ 
    facet_wrap(treatment~outcome,scale='free', nrow=6)+
    labs(y='Opioid indicators from claims data',x='Quarters before and after policy change',subtitle='')

ggsave(file.path(here,'results','figure_s2_outcome_balance_claims.png'),fig2,width=15,height=15)

fig1 = ggplot(tab[-12 <= time_id & time_id <= 12 & outcome_type == 'cdc data' & !is.na(treatment),], 
	aes(x=time_id,y=mean_outcome,alpha=pre_post,shape=type,color=type)) +
    geom_point()+
    geom_line() + 
    geom_vline(xintercept=0,linetype='dotted')+
    #scale_color_nejm()+
    scale_alpha_manual(values=c('pre'=0.5,'post'=1))+
    scale_color_manual(values=c('treated'='red','matched'='blue','unmatched'='black'))+
    theme_pubr() + 
	guides(alpha=FALSE, 
		fill=guide_legend(title=NULL),
		color=guide_legend(title=NULL),
		shape=FALSE,
		linetype=FALSE)+ 
    facet_wrap(treatment~outcome,scale='free', nrow=6)+
    labs(y='Overdose deaths per 100k',x='Quarters before and after policy change',subtitle='')

ggsave(file.path(here,'results','figure_s3_outcome_balance_mortality.png'),fig1,width=15,height=15)

