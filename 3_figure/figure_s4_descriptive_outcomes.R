# ----------------------------------------------------------------------
# ' this file plots trends of outcomes across time
# ----------------------------------------------------------------------
library(tidyverse)
library(skimr)
library(hrbrthemes) # for ggplot
require(mgcv)
library(ggpubr)
library(zoo)
library(gridExtra)

here = getwd()

# variables of interest
target_depvars = matrix(c(
    'opioid_mortality_per100k','All overdose deaths',
    'mortality_heroin','Heroin',
    'mortality_opioids','Natural Opioid',
    'mortality_methadone','Methadone',
    'mortality_synth','Synthetic',
    'mortality_cocaine','Cocaine',

    'total_opioid_patients_per100k','P(taking opioid)',
    'prop_high_mme','P(daily MME > 90)',
    'total_overlapping_per100k','P(Overlapping)',
    'n_plusplus4_per100k','P(4+4-90)',
    'overdosed_patients_per100k','P(OUD + OD)',
    'total_MAT_patients_per100k_addnaltrexon', 'P(taking MAT)'
    ), 
ncol=2, byrow=TRUE)

depvars_label = target_depvars[,2]
depvars = target_depvars[,1]

time_unit = 'year'
geo_unit = 'state'
time_unit = 'quarter'

mdata = fread(file.path(here,'data','reg-data',paste0('did_',time_unit,'_addmeth.csv')))
pdmp_timing = fread(file.path(here,'data','reg-data','did_policy_quarter.csv'))
list_treatment = paste0(c('access','must','naloxone','pillmill','goodsam','dayslimit'),'_tr')
pdmp_timing[, all_laws_tr := rowSums(.SD,na.rm=TRUE), .SDcols=list_treatment]

all_mdata = melt(mdata, id.var = c('state','year','quarter'), measure.vars = depvars)
all_mdata = merge(x = all_mdata, y = pdmp_timing[,c('state','year','quarter','all_laws_tr')], by = c('state','year','quarter'), all.x = TRUE)

all_mdata[,outcome_group := ifelse(grepl('mortality',variable),'mortality','prescription')]

all_mdata[, variable := factor(variable, levels=depvars, labels=depvars_label)]
all_mdata[,date := as.Date(as.yearqtr(paste0(year,' Q',quarter)))]

# across different number of adoptions
mean_mdata = all_mdata[,.(
    mean_value = mean(value,na.rm=TRUE), 
    sd_value = sd(value,na.rm = TRUE), 
    mean_lci = quantile(value, 0.05, na.rm=TRUE),
    mean_uci = quantile(value, 0.95, na.rm=TRUE),
    n_obs=.N),by = c('variable','all_laws_tr')]

mean_mdata[,mean_lci := mean_value - 1.96*sd_value / sqrt(n_obs)]
mean_mdata[,mean_uci := mean_value + 1.96*sd_value / sqrt(n_obs)]
mean_mdata[,outcome_group := ifelse(grepl('Mortality|Heroin|Natural|Synthetic|Cocaine|Methadone',variable),'mortality','prescription')]

mean_mdata_sub = mean_mdata[variable %in% c('All overdose deaths','P(OUD + OD)','Natural Opioid','Synthetic',
    'P(taking opioid)','P(4+4-90)','P(daily MME > 90)','P(taking MAT)'),]
mean_mdata_sub[,variable := factor(variable, levels=c('All overdose deaths','P(OUD + OD)','Natural Opioid','Synthetic',
    'P(taking opioid)','P(4+4-90)','P(daily MME > 90)','P(taking MAT)'))]
mean_tab = dcast(mean_mdata_sub, variable~all_laws_tr, value.var = c('mean_value'))
export(mean_tab, file.path(here,'results','figure_s4_trends.xlsx'))

p_all1 <- ggplot(mean_mdata_sub[variable %in% c('All overdose deaths','P(OUD + OD)','P(taking MAT)'),], 
    aes(x=all_laws_tr,y=mean_value,ymin=mean_lci,ymax=mean_uci,color=all_laws_tr)) +
    stat_smooth(method = "lm", formula = y ~ x + I(x^2), size = 0.4,se=TRUE, col='black')+
    geom_pointrange(size=0.2)+
    scale_color_gradient(low='pink',high='red3') +
    facet_wrap(~variable,scale='free',nrow=1) +
    theme_minimal()+
    scale_x_continuous(breaks=c(0,1,2,3,4,5,6))+
    theme(legend.position = 'none',
        panel.grid.minor = element_blank(),panel.grid.major=element_blank(),
        axis.line = element_line(colour = "black"))+
    labs(title = NULL,subtitle=NULL,
    #subtitle = paste0("Measures: ",varlabel),
    y='Outcome rate per 100,000',
    x='Cumulative Count of Policies Enacted') 

p_all2 <- ggplot(mean_mdata_sub[variable %in% c('P(taking opioid)','P(4+4-90)','P(daily MME > 90)'),], 
    aes(x=all_laws_tr,y=mean_value,ymin=mean_lci,ymax=mean_uci,color=all_laws_tr)) +
    stat_smooth(method = "lm", formula = y ~ x + I(x^2), size = 0.4,se=TRUE, col='black')+
    geom_pointrange(size=0.2)+
    scale_color_gradient(low='pink',high='red3') +
    facet_wrap(~variable,scale='free',nrow=1) +
    theme_minimal()+
    scale_x_continuous(breaks=c(0,1,2,3,4,5,6))+
    theme(legend.position = 'none',
        panel.grid.minor = element_blank(),panel.grid.major=element_blank(),
        axis.line = element_line(colour = "black"))+
    labs(title = NULL,subtitle=NULL,
    #subtitle = paste0("Measures: ",varlabel),
    y='Outcome rate per 100,000',
    x='Cumulative Count of Policies Enacted') 

p_all = ggarrange(p_all1,p_all2,nrow=2)

ggsave(p_all,
    filename=file.path(here,'results','figure_s4_trends.png'),
    width=6,height=5)
