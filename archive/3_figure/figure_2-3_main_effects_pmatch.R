#------------------------------------------------------------------------------
# figure 2 and 3
#==============================================================================

library(gghighlight)
library(ggpubr)
library(ggsci)
library(data.table)
library(viridis)
library(colorspace)

here = getwd()
time_unit = 'quarter'

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
list_treatment_label = c('PDMP access','mandatory PDMP','Pill Mill','Prescription Limit','Good Samaritan','Naloxone Law')

# load outcome data for plotting
outcome_balance = fread(file.path(here,'results','outcome_balance.csv'))
coef_pmatch = fread(file.path(here,'results','coef_pmatch.csv'))

# read state-level data as well.
data = fread(file.path(here,'data','reg-data',paste0('did_',time_unit,'_addmeth.csv')))
for (DV in depvars[1:6]) {data[,(DV) := get(DV)*10^5]}

# read pmatch results
coef_pmatch = fread(file.path(here,'results','coef_pmatch.csv'))

tab = copy(coef_pmatch)

tab[,effects := ifelse(coef>0, 'positive','negative')]
tab[,date := as.numeric(gsub('t+','',time_id))]
tab[,sig := ifelse(lci > 0 | uci < 0,'sig','insig')]
tab[,outcome := factor(outcome,levels=depvars,labels=depvars_label)]
tab[,treatment := factor(treatment, levels=list_treatment,labels=list_treatment_label)]

tab[outcome %in% depvars_label[1:6],outcome_type := 'cdc data']
tab[outcome %in% depvars_label[7:12],outcome_type := 'claims data']
tab[outcome_type == 'cdc data' & (coef * 3000) < -2000, coef := -(2000/3000)]

# read meta analysis rsults
mean_effect = fread(file.path(here,'results','pmatch_meta_analysis.csv'))
mean_effect[,outcome := factor(outcome,levels=depvars,labels=depvars_label)]
mean_effect[,treatment := factor(treatment, levels=list_treatment,labels=list_treatment_label)]
mean_effect[outcome %in% depvars_label[1:6],outcome_type := 'cdc data']
mean_effect[outcome %in% depvars_label[7:12],outcome_type := 'claims data']

setnames(mean_effect,'coef','coef_meta')
setnames(mean_effect,'se','se_meta')
setnames(mean_effect,'lci','lci_meta')
setnames(mean_effect,'uci','uci_meta')
mean_effect[,effects_meta := ifelse(coef_meta > 0, 'positive','negative')]

tab1 = merge(tab, mean_effect, by=c('outcome','treatment','outcome_type'))

tab1[, mean_effect :=  ifelse(outcome_type=='cdc data',
    paste0('Avg.= ',round(coef_meta,1)),
    paste0('Avg.= ',round(coef_meta,3)))]

tenth_max = coef_pmatch[, .(tenth_max=max(coef, na.rm=TRUE)), by=c('outcome')]

tenth_max[outcome %in% depvars_label[1:6],tenth_max := tenth_max * 3000]
tenth_max[outcome %in% depvars_label[7:12],tenth_max := tenth_max * 100]

tab1 = merge(tab1, tenth_max, by='outcome',all.x=TRUE)
tab1[, sig_meta := 'insig']
tab1[pval < 0.05 & coef_meta > 0, sig_meta := 'sig_positive']
tab1[pval < 0.05 & coef_meta < 0, sig_meta := 'sig_negative']

tab_short = tab1[treatment %in% c('mandatory PDMP', 'Naloxone Law') & outcome %in% c('All overdose', 'Natural opioid','Synthetic opioid'), ]
tab_short[, treatment := droplevels(treatment)]

p2 = ggplot(tab_short, 
    aes(x=date,y=coef*3000,ymin=lci*3000,ymax=uci*3000,color=effects, shape=sig)) + 
    geom_rect(aes(fill = sig_meta),xmin = -Inf,xmax = Inf,ymin = -Inf,ymax = Inf,alpha = 0.01) +
    geom_hline(aes(yintercept=0),linetype='solid',color='black',size=0.2)+
    geom_hline(data=mean_effect[treatment %in% c('mandatory PDMP', 'Naloxone Law') & outcome %in% c('All overdose', 'Natural opioid','Synthetic opioid'),],aes(color=effects_meta,yintercept=coef_meta),linetype='solid',size=1)+
    geom_pointrange(aes(linetype=sig))+
    scale_shape_manual(values = c('sig'=19,'insig'=1)) +
    scale_fill_manual(values = c('sig_positive'='firebrick1','sig_negative'='royalblue1','insig'='white'))+
    scale_linetype_manual(values = c('sig'='solid','insig'='dotted'))+
    scale_color_manual(values = c('positive'='red','negative'='blue'))+
    theme_light()+
    facet_grid(outcome~treatment,scale='free_y',
        labeller = label_value,switch = "y")+
    theme(strip.text.x = element_text(size = 6),strip.text.y = element_text(size = 7))+
    theme(legend.position='none')+
    scale_x_continuous(n.breaks = 3) +
    labs(y='Effects of policy on overdose deaths (per 300 million)',
        x='Quarters elapsed after policy enactment')
p2 = p2 + geom_text(aes(x=0,y=tenth_max,label=mean_effect), color='black', size=3,hjust="left",fontface='bold')

ggsave(file.path(here,'results','figure_3_pmatch_mortality_short.svg'),p2, width=5,height=6,dpi=500)

tab_short = tab1[treatment %in% c('mandatory PDMP', 'Naloxone Law') & outcome %in% c('P(taking opioid)', 'P(OUD and Overdose)','P(taking MAT drug)'), ]
tab_short[, treatment := droplevels(treatment)]

p1 = ggplot(tab_short, 
    aes(x=date,y=coef*100,ymin=lci*100, ymax=uci*100, color=effects, shape=sig)) + 
    geom_rect(aes(fill = sig_meta),xmin = -Inf,xmax = Inf,ymin = -Inf,ymax = Inf,alpha = 0.01) +
    geom_hline(aes(yintercept=0),linetype='solid',color='black',size=0.2)+
    geom_hline(data=mean_effect[treatment %in% c('mandatory PDMP', 'Naloxone Law') & outcome %in% c('P(taking opioid)', 'P(OUD and Overdose)','P(taking MAT drug)'),],aes(color=effects_meta,yintercept=coef_meta),linetype='solid',size=1)+
    geom_pointrange(aes(linetype=sig))+
    scale_shape_manual(values = c('sig'=19,'insig'=1)) +
    scale_fill_manual(values = c('sig_positive'='firebrick1','sig_negative'='royalblue1','insig'='white'))+
    scale_linetype_manual(values = c('sig'='solid','insig'='dotted'))+
    scale_color_manual(values = c('positive'='red','negative'='blue'))+
    theme_light()+
    facet_grid(outcome~treatment,scale='free_y',
        labeller = label_value,switch = "y")+
    theme(strip.text.x = element_text(size = 6),strip.text.y = element_text(size = 7))+
    theme(legend.position='none')+
    scale_x_continuous(n.breaks = 3) +
    labs(y='Marginal effects of policy on opioid indicators (%)',
        x='Quarters elapsed after policy enactment')
p1 = p1 + geom_text(aes(x=0,y=tenth_max,label=mean_effect), color='black', size=3,hjust="left",fontface='bold')

ggsave(file.path(here,'results','figure_2_pmatch_claims_short.svg'),p1, width=5,height=6, dpi=500)

