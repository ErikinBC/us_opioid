#------------------------------------------------------------------------------
# plots in appendix : figure illustration
# 1. general patterns (outcome balancing check)
# 2. covariate balancing
# 3. event-study design 
# 4. panel matching outcomes
#==============================================================================

pckgs <- c('data.table','ggplot2','cowplot',
           'gghighlight','ggpubr','ggsci')
for (pckg in pckgs) { library(pckg, character.only=TRUE) }

here = getwd()
dir_olu = file.path(here,'..')
dir_results = file.path(dir_olu, 'results')
dir_figures = file.path(dir_olu, 'figures')
dir_data = file.path(dir_olu,'data')

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
    'total_MAT_patients_per100k', 'P(taking MAT drug)'
    ), 
ncol=2, byrow=TRUE)


depvars_label = target_depvars[1:12,2]
depvars = target_depvars[1:12,1]

list_treatment = paste0(c('access','must','naloxone','pillmill','goodsam','dayslimit'),'_tr')
list_treatment_label = c('PDMP access','mandatory PDMP','Naloxone Law','Pain Clinic','Good Samaritan','Days Limit')

outcome_balance = fread(file.path(dir_results,'outcome_balance.csv'))
coef_event = fread(file.path(dir_results,'coef_event_study.csv'))
coef_pmatch = fread(file.path(dir_results,'coef_pmatch.csv'))

# for each outcome + treatment combinations
DV = depvars_label[1]
tr = list_treatment_label[2]

tab1 = outcome_balance
tab1[, pre_post := ifelse(time_id < 0, 'pre','post')]
tab1[,outcome := factor(outcome,levels=depvars,labels=depvars_label)]
tab1[,treatment := factor(treatment, levels=list_treatment,labels=list_treatment_label)]

fig2 = ggplot(tab1[-12 <= time_id & time_id <= 12 & treatment == tr & outcome == DV,], 
	aes(x=time_id,y=mean_outcome,alpha=pre_post,shape=type,color=type)) +
    geom_point()+
    geom_line() + 
    geom_vline(xintercept=0,linetype='dotted')+
    scale_alpha_manual(values=c('pre'=0.5,'post'=1))+
    scale_color_manual(values=c('treated'='red','matched'='blue','unmatched'='black'))+
    theme_pubr() +
    theme(axis.title.y = element_text(size=10))+
    guides(alpha=FALSE, shape=guide_legend(title=NULL),color=guide_legend(title=NULL))+
    labs(y=paste0(DV,' deaths per 100k'),x='Quarters before and after policy change',subtitle='B. Pre-post trends')
save_plot(file.path(dir_figures,'eFigure1.png'),fig2,base_height=4,base_width=5)

tab2 = coef_event
tab2[,outcome := factor(outcome,levels=depvars,labels=depvars_label)]
tab2[,treatment := factor(treatment, levels=list_treatment,labels=list_treatment_label)]

fig3 = ggplot(tab2[-12 <= date & date < 12 & treatment == tr & outcome == DV,], 
	aes(x=date,y=coef, ymin=coef - 1.96*se, ymax=coef + 1.96*se, color=effects, linetype=sig)) + 
	geom_pointrange(aes(alpha=sig))+
	scale_alpha_manual(values=c('sig'=1,'insig'=0.5)) +
	scale_color_manual(values=c('positive'='red','negative'='blue','reference'='black'))+
	geom_ribbon(color='gray',fill='gray',alpha=0.2,linetype='solid')+	
	scale_linetype_manual(values=c('sig'='solid','insig'='dotted'))+
	geom_hline(yintercept = 0, lty=2) + 
	geom_vline(xintercept=0,linetype='dotted')+
	theme_pubr() + 
	theme(axis.title.y = element_text(size=9))+
	guides(alpha=FALSE, 
		fill=guide_legend(title=NULL),
		color=guide_legend(title=NULL),
		linetype=FALSE)+ 
	labs(y=paste0('Marginal effects on ',tolower(DV),' deaths per 100k'),x='Quarters before and after policy change',subtitle='C. Event study frameworks')

tab3 = coef_pmatch
tab3[,effects := ifelse(coef>0, 'positive','negative')]
tab3[,date := as.numeric(gsub('t+','',time_id))]
tab3[,sig := ifelse(lci > 0 | uci < 0,'sig','insig')]
tab3[,outcome := factor(outcome,levels=depvars,labels=depvars_label)]
tab3[,treatment := factor(treatment, levels=list_treatment,labels=list_treatment_label)]

fig4 = ggplot(tab3[!is.na(coef) & treatment == tr & outcome == DV,], 
	aes(x=date,y=coef, ymin=lci, ymax=uci, color=effects, linetype=sig)) + 
	geom_pointrange(aes(alpha=sig))+
	scale_alpha_manual(values=c('sig'=1,'insig'=0.5)) +
	scale_color_manual(values=c('positive'='red','negative'='blue'))+
	geom_ribbon(color='gray',fill='gray',alpha=0.2,linetype='solid')+	
	scale_linetype_manual(values=c('sig'='solid','insig'='dotted'))+
	geom_hline(yintercept = 0, lty=2) +
	guides(alpha=FALSE, 
		fill=guide_legend(title=NULL),
		color=guide_legend(title=NULL),
		#linetype=guide_legend(title=NULL)
		linetype=FALSE
		)+ 
	theme_pubr() + 
	theme(axis.title.y = element_text(size=9))+
	labs(y=paste0('Marginal effects on ',tolower(DV),' deaths per 100k'),x='Quarters after policy change',subtitle='D. Panel matching')

hdata = rbind(
	data.table(x=c(-1,1),y=c(10, 20),type='control'),
	data.table(x=c(-1,1),y=c(15, 25),type='counterfactual'),
	data.table(x=c(-1,0),y=c(15, 20),type='treated'),
	data.table(x=c(0,1),y=c(20, 30),type='treated'))

hdata[,type := factor(type, 
	levels=c('treated','counterfactual','control'),
	labels=c('treated','counterfactual','control'))]

fig1 = ggplot(data=hdata, aes(x=x,y=y, linetype=type,color=type)) +
	geom_line() +
	ylim(c(0,35))+ xlim(c(-1.2,1.8)) + 
	scale_linetype_manual(values=c('control'='solid','counterfactual'='dotted','treated'='solid'))+
	scale_color_manual(values=c('control'='blue','counterfactual'='red','treated'='red'))+
	theme_light() + 
	guides(color=guide_legend(title=NULL),linetype=guide_legend(title=NULL))+ 
	theme(legend.position='top')+
	theme(axis.text.x = element_blank())+
	labs(y='hypothetical outcome',x='time',subtitle='A. Difference-in-difference')+
	annotate(
    	geom = "curve", x = 1, y = 25, xend = 1, yend = 30, 
    	curvature = .3, arrow = arrow(length = unit(2, "mm"))
  	) +
  	annotate(geom = "text", x = 1.2, y = 28, label = "treatment\neffects", hjust = "left")

fig = ggarrange(
  fig1, fig2, fig3, fig4,
  nrow = 2, ncol = 2) 

ggsave(file.path(here,'results','figure_s1_illustration_pmatch.png'),fig,width=7,height=7)

