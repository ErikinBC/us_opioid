# ----------------------------------------------------------------------
# ' figure 1
# ----------------------------------------------------------------------

library(tidyverse)
library(skimr)
library(hrbrthemes) 
require(mgcv)
library(ggpubr)
library(zoo)
library(gridExtra)
library(ggrepel)
library(ggsci)
library(ggalluvial)

here = getwd()

# load pdmp data 
pdmp_group = fread(file.path(here,'data','policy-data','pdmp_group.csv'))
pdmp_group[,pdmp_date := as.Date(pdmp_date,format='%Y-%m-%d')]

pdmp_group[year(pdmp_date) < 2007, pdmp_date := as.Date('2007-01-01')]
pdmp_group = pdmp_group[!is.na(pdmp_date),]
setnames(pdmp_group,'pdmp_date','date')

# load other state law data 
state_law = fread(file.path(here,'data','policy-data','state_drug_policy.csv'))
state_law = melt(state_law, id.vars='state')
state_law[,state := state.name[match(state,state.abb)]]

setnames(state_law,'value','date')
setnames(state_law, 'variable','type')

state_law[,type := gsub('_date','',type)]
state_law[,date := as.Date(date)]
state_law = state_law[!is.na(date),]
state_law[date < as.Date('2007-01-01'), date := as.Date('2007-01-01')]

medicaid_law = fread(file.path(here,'data','policy-data','medicaid expansion dates.csv'))
medicaid_law[,notes_kff := NULL]
medicaid_law[, kff_implentation_date := as.Date(kff_implentation_date,format='%m/%d/%y')]

medicaid_law[,type := 'medicaid']
setnames(medicaid_law,'kff_implentation_date','date')
medicaid_law[as.Date('2007-01-01') >= date, date := as.Date('2007-01-01')]

# combine all these three laws
state_law = rbind(state_law,pdmp_group[type!='enactment',],medicaid_law[!is.na(date),c('state','date','type')])

state_law[,type := factor(type, 
  levels=c('access','must','pillmill','dayslimit','goodsam','naloxone','medicaid'),
  labels=c('PDMP access','mandatory PDMP','Pain Clinic Law','Prescription Limit Law','Good Samaritan Law','Naloxone Law','Medicaid Expansion'))]

state_law[,year := year(date)]
state_law[, date := as.Date(date)]

fig_individual = ggplot(state_law[date <= as.Date('2018-12-31'),] %>% group_by(type) %>% arrange(date) %>% mutate(rn = row_number())) + 
    geom_vline(xintercept = as.Date('2013-01-01'), linetype="dotted", color = "gray", size=1)+
  geom_step(aes(x=date, y=rn, color=type,linetype=type,size=0.8)) + 
    scale_linetype_manual(values=c(1,1,1,1,1,1,1)) +
    annotate(geom="text", x=as.Date('2009-01-01'), y=29, label="PDMP access",color="pink")+
    annotate(geom="text", x=as.Date('2017-01-01'), y=30, label="mandatory PDMP",color="red")+
    annotate(geom="text", x=as.Date('2017-01-01'), y=13, label="Pain Clinic",color="brown")+
    annotate(geom="text", x=as.Date('2015-01-01'), y=0, label="Prescription Limit",color="orange")+
    annotate(geom="text", x=as.Date('2017-01-01'), y=45, label="Good Samaritan",color="blue")+
    annotate(geom="text", x=as.Date('2014-01-01'), y=37, label="Naloxone",color="skyblue")+    
    annotate(geom="text", x=as.Date('2013-01-01'), y=20, label="Medicaid Expansion",color="purple")+    
    scale_color_manual(values=c(
      'PDMP access'='pink',
      'Good Samaritan Law'='blue',
      'mandatory PDMP'='red',
      'Naloxone Law'='skyblue',
      'Pain Clinic Law'='brown'  ,
      'Prescription Limit Law'='orange' ,
      'Medicaid Expansion'='purple'
      ))+
    scale_size_identity() +
    scale_x_date(date_breaks='2 years', date_labels='%Y',limits=c(as.Date('2006-06-01'),as.Date('2018-12-31')))+
    labs(x='State Policy Implementation Year', y='Cumulative Number of Effective State-Policies')+
    theme_pubr() +
    theme(panel.grid = element_blank(), axis.ticks.y = element_line()) +
    theme(legend.position = 'none')

pdmp_timing = fread(file.path(here,'data','reg-data','did_policy_quarter.csv'))

list_treatment = paste0(c('access','must','naloxone','pillmill','goodsam','dayslimit'),'_tr')
pdmp_timing[, all_laws_tr := rowSums(.SD,na.rm=TRUE), .SDcols=list_treatment]
pdmp_timing[, one := 1]
last_year = na.omit(pdmp_timing[year==2018&quarter==1,c('all_laws_tr','state','one')])
last_year[, pos_y := cumsum(one), by=c('all_laws_tr')]

fig_histogram = ggplot(last_year,
  aes(x=all_laws_tr, y=pos_y,label=state)) +
  geom_text()+
  xlim(c(0,7))+
  ylim(c(0.5,19))+
  theme_pubr() +
  labs(x='Number of Enacted State Policies',y='Number of States')

state_law = state_law[!is.na(state),]
state_law = state_law[!is.na(date),]
setorder(state_law, state,date)
state_law[,order := 1:.N, by='state']

state_law[,law := type]

fig_sequence = ggplot(state_law,
       aes(x = order, stratum = law, alluvium = state,
           fill = law, label = law)) +
  scale_x_continuous(breaks=c(1:7))+
  scale_fill_manual(values=c(
      'PDMP access'='pink',
      'Good Samaritan Law'='blue',
      'mandatory PDMP'='red',
      'Naloxone Law'='skyblue',
      'Pain Clinic Law'='brown'  ,
      'Prescription Limit Law'='orange' ,
      'Medicaid Expansion'='purple'
      ))+
  geom_flow(stat = "flow", lode.guidance = "frontback",
          na.rm=TRUE) +
  geom_stratum() +
  guides(fill=guide_legend(title=''))+
  theme_pubr()+
  labs(x='Policy Adoption Sequence Order', y='Number of States')

fig_multi <- ggarrange(
  ggarrange(fig_individual,fig_histogram,ncol = 2, vjust=1.7, hjust=c(-0.44,-0.26),
    font.label=list(size=12),
    labels = c("A. Monthly Count","B. Current Policy Adoption (2018)")),
    ggarrange(fig_sequence,
        font.label=list(size=12),
        vjust= 0.5,hjust=-0.4,
        labels = "C. Transition Plot"                                     
      ),
    nrow = 2
          ) 
ggsave(file.path(here,'results','figure1_state_policy_count_for_presentation_new.png'),fig_multi,width=8,height=8)

