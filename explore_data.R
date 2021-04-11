# SCRIPT TO FAMILIZIZE DATA

pckgs <- c('data.table','cowplot','magrittr','stringr','zoo','ggplot2')
for (pckg in pckgs) { library(pckg, character.only=TRUE) }

# specify your project root directory here
here = getwd()
dir_olu = file.path(here,'..')
dir_figures = file.path(dir_olu, 'figures')
dir_output = file.path(dir_olu, 'output')
dir_data = file.path(dir_olu,'data')
dir_regdata = file.path(dir_data,'reg-data')
dir_cdc = file.path(dir_data,'cdc-data')

#########################
# --- (1) LOAD DATA --- #

# (i) Load regression data
ddata = fread(file.path(dir_regdata,'did_quarter_addmeth.csv'))
ddata %>% dim
ddata[1,] %>% t

# (ii) Load state lookup
di_state = fread(file.path(dir_data,'tigris_fips_codes.csv'))
di_state = di_state[,(count=.N),by=list(state,state_code,state_name)]
di_state[,V1:=NULL]

# (iii) Load CDC mortality
path_cdc = file.path(dir_cdc,'CDC_MCD_state_quarter_opioid_mortality_1999_2018_suppressed.csv')
dat_cdc = fread(path_cdc)
setnames(dat_cdc,old=c('statecode','death_count','crude_rate_measure','icd_code','population'),new=c('state_code','deaths','deaths_pc','icd10','pop'))
dat_cdc = merge(di_state[,c('state_code','state')],dat_cdc[,-c('state')],by='state_code')
# Replace year/quarter with yqtr
dat_cdc[,yqtr := as.yearqtr(str_c(year,quarter,sep='-'))]
dat_cdc[, `:=` (state_code=NULL,year=NULL,quarter=NULL)]

# We don't have CDC data on Puerto Rico's population
setdiff(unique(ddata$state),unique(dat_cdc$state))


###################################
# --- (2) TALLY OPIOID DEATHS --- #

vv_from = c('all',str_c('T40.',1:6))
vv_to = c('all','heroin','opioids','methadone','synth','cocaine','other')

# Rename death codes
dat_cdc[,icd10 := factor(icd10,levels=vv_from,labels=vv_to)]
# Does all equal the other six?
tmp1 = dat_cdc[icd10 != 'all',list(agg=sum(deaths),pop=mean(pop)),by=list(state,yqtr)]
tmp2 = data.table::copy(dat_cdc[icd10 == 'all'])
setnames(tmp2,'deaths','all')
tmp3 = merge(tmp2[,c('yqtr','state','all')],tmp1,by=c('state','yqtr'))
tmp3[,ddeaths := all - agg]
tmp3[,list(mx=max(ddeaths),mi=min(ddeaths)),by=state]
tmp3[state == 'AK' & ddeaths == -5]

gg_check1 = ggplot(tmp3,aes(x=agg,y=all)) + geom_point(size=0.5) + scale_x_continuous(limits=c(0,2100)) + scale_y_continuous(limits=c(0,2100)) + geom_abline(slope=1,intercept=0,color='red') + theme_bw()
save_plot(file.path(dir_figures,'gg_check1.png'),gg_check1,base_height=4,base_width=5)

tmp3[,c('all','agg')] %>% apply(2,sum)  # "all" appears to have more, especially on the lower end

#################################
# --- (3) PER CAPITA DEATHS --- #

cn_icd10_lvl <- c('All overdose','Opioids','Cocaine','Synthetic','Heroin','Methadone','Other')
names(cn_icd10_lvl) <- c('all','opioids','cocaine','synth','heroin','methadone','other')

# This aligns with eTable. 5
eTab5 <- dat_cdc[,list(deaths=sum(deaths)),by=icd10][order(-deaths)]
icd10_deaths <- sum(eTab5[2:nrow(eTab5)]$deaths)
all_deaths <- eTab5$deaths[1]
extra_deaths <- all_deaths - icd10_deaths
print(sprintf('all deaths: %i, icd10 deaths: %i, difference: %i',all_deaths,icd10_deaths,extra_deaths))

# crude_rate_measure is basically identical to deaths / pop
dat_cdc[,err := (deaths - round(deaths_pc*pop))/deaths]
summary(100*dat_cdc[err != 0]$err)  # No more than 1% difference

# Compare this to the regression data
cn_ddata = colnames(ddata)
cn_mort_lvl = c(str_subset(cn_ddata,'^mortality'),'opioid_mortality_per100k')
cn_mort_lbl = str_replace(str_replace(cn_mort_lvl,'mortality_',''),'opioid_per100k','all')
df_mort = melt(ddata,id.vars=c('state','year','quarter'),measure.vars=cn_mort_lvl,variable.name='mortality',value.name='deaths')
df_mort[, mortality := factor(mortality,cn_mort_lvl,cn_mort_lbl)]
setnames(df_mort,'mortality','icd10')
df_mort[,yqtr := as.yearqtr(str_c(year,quarter,sep='-'))]
df_mort[, `:=` (year=NULL,quarter=NULL)]
df_mort = df_mort[order(state,icd10,yqtr)]
# They are identical
tmp_merge = merge(df_mort,dat_cdc[,c('yqtr','state','icd10','deaths_pc')],by=c('state','yqtr','icd10'),all.y=TRUE,all.x=FALSE)
stopifnot(with(tmp_merge,all.equal(deaths,deaths_pc)))
xx = max(apply(tmp_merge[,c('deaths','deaths_pc')],2,max))
gg_check2 = ggplot(tmp_merge,aes(x=deaths,y=deaths_pc)) + labs(x='Regression',y='CDC') + geom_point(size=0.5) + geom_abline(slope=1,intercept=0,color='red') + theme_bw() + scale_x_continuous(limits=c(0,xx)) + scale_y_continuous(limits=c(0,xx))
save_plot(file.path(dir_figures,'gg_check2.png'),gg_check2,base_height=4,base_width=5)


# The Q/Q growth will be skewed downwards from Q4 to Q1 because population is not being linearly interpolated
dat_deaths <- dat_cdc[,list(deaths=sum(deaths),pop=sum(pop)),by=list(icd10,yqtr)][order(icd10,yqtr)]
dat_deaths[, `:=` (qtr = as.integer(format(yqtr,'%q')), year=as.integer(format(yqtr,'%Y')))]
# Get linearly interpolated pop
dat_pop_yy = dat_deaths[icd10=='all',list(pop=mean(pop)),by=list(year)]
dat_pop_yy[,lpop := shift(pop)]
dat_pop_qq = dat_pop_yy[!is.na(lpop),approx(c(lpop,pop),n=5)$y,by=list(year)]
dat_pop_qq[, year := year - 1]
setnames(dat_pop_qq,'V1','pop')
dat_pop_qq = cbind(dat_pop_qq,dat_pop_qq[ , list( qtr = seq(.N) ) , by = year][,-'year'])
dat_pop_qq = dat_pop_qq[!duplicated(pop)]
dat_pop_qq[, `:=` (year=ifelse(qtr==5,year+1, year), qtr=ifelse(qtr == 5, 1, qtr) )]
dat_deaths = merge(dat_deaths,dat_pop_qq,by=c('year','qtr'),suffixes=c('','_interp'))
dat_deaths = dat_deaths[order(icd10,yqtr)]
# Calculate different PC deaths
dat_deaths[,`:=` (dpc = 1e5 * deaths / pop, dpc_interp=1e5 * deaths / pop_interp)]

dat_deaths[,c('icd10','yqtr','deaths','dpc','dpc_interp')] %>% head(20)
dcast(dat_deaths_long,'icd10+yqtr ~ method',value.var='pct')[dpc > deaths] %>% head
dat_deaths_long[,list(mu=median(pct,na.rm=T)),by=list(method,icd10)][order(icd10,method)]

dat_deaths_long = melt(dat_deaths,id.vars=c('icd10','yqtr','qtr'),measure.vars=c('dpc','dpc_interp','deaths'),variable.name='method')
dat_deaths_long[, qtr := str_c('Q',qtr)]
dat_deaths_long[,pct:=value/shift(value)-1,by=list(method, icd10)]
dat_deaths_long[,method := factor(method,levels=c('deaths','dpc','dpc_interp'),labels=c('Deaths','Per capita','Per capita (interp)'))]

posd = position_dodge(0.5)
gg_qq_deaths = ggplot(dat_deaths_long,aes(x=qtr,y=pct,color=method)) + 
    geom_jitter(size=0.5,alpha=0.5,position=posd) + 
    geom_boxplot(position=posd) + 
    geom_hline(yintercept=0) + 
    facet_wrap(~icd10,labeller=labeller(icd10=cn_icd10_lvl)) + 
    labs(x='Quarter',y='% change in per capita deaths') + 
    theme_bw() + 
    scale_color_discrete(name='Method')
save_plot(file.path(dir_figures,'gg_qq_deaths.png'),gg_qq_deaths,base_height=8,base_width=10)

dat_deaths$yqtr %>% head %>% as.Date
# Time series
gg_dpc = ggplot(dat_deaths[icd10!='all'],aes(x=as.Date(yqtr),y=4*dpc)) + geom_line() + 
    theme_bw() + 
    labs(y='Annualized deaths per 100K') +   #,title='Quarterly US Overdose deaths'
    theme(axis.title.x=element_blank(),axis.text.x=element_text(angle=90)) + 
    facet_wrap(~icd10,labeller=labeller(icd10=cn_icd10_lvl),scales='free_y',nrow=2) + 
    scale_x_date(date_breaks='2 years',date_labels='%Y')
save_plot(file.path(dir_figures,'gg_dpc.png'),gg_dpc,base_height=5,base_width=9)

# The reason that the growth rate for per capita deaths (whether constant or interpolated) are (basically) the same is that both annual and quarterly population growth rates are paltry:
growth_qq = melt(dat_deaths[icd10=='all'],id.vars=c('yqtr','qtr'),measure.vars=c('pop','pop_interp','deaths'),variable.name='msr')
growth_qq[, qtr := str_c('Q',qtr)]
growth_qq[,pct := value/shift(value)-1,by=msr]
cn_msr = c(pop='Pop',pop_interp='Pop (interp)',deaths='Deaths')

gg_growth_pct = ggplot(growth_qq,aes(x=pct,fill=qtr)) + 
    theme_bw() + labs(y='Frequency',x='Quarterly (%) change') + 
    geom_histogram(position='identity',alpha=0.5,bins=20) + 
    scale_fill_discrete(name='Quarter') + 
    facet_wrap(~msr,labeller=labeller(msr=cn_msr),scales='free_x') + 
    theme(panel.spacing.x=unit(2, 'lines'),legend.position='bottom')
save_plot(file.path(dir_figures,'gg_growth_pct.png'), gg_growth_pct, base_height=3,base_width=8)

#################################
# --- (4) COMPARE TO CANADA --- #

# https://health-infobase.canada.ca/substance-related-harms/opioids-stimulants/graphs?index=395
dat_cad = fread(file.path(dir_data,'cad-data','SubstanceHarmsData.csv'))
dat_cad = dat_cad[!(Time_Period %in% c('By quarter','By year'))]
dat_cad = dat_cad[Substance == 'Opioids' & Source == 'Deaths' & Unit=='Number']
dat_cad = dat_cad[!(Value %in% c('Suppressed','Not applicable'))]
dat_cad_deaths = dat_cad[,list(deaths=sum(as.integer(Value))),by=Time_Period]
# Annualize deaths
dat_cad_deaths[,`:=` (deaths = ifelse(Time_Period=='2020 (Jan to Sep)',deaths*(4/3),deaths),
                        Time_Period = as.integer(str_replace_all(Time_Period,'[^0-9]','')) )]
setnames(dat_cad_deaths,'Time_Period','year')
# Pop from quarterly estimates  Table: 17-10-0009-01 (formerly CANSIM 051-0005) 
dat_cad_pop = data.frame(year=c(2018,2019,2020),pop=c(37249240,37802043,38008005))
dat_cad_deaths = merge(dat_cad_deaths,dat_cad_pop,by='year')
dat_cad_deaths[,`:=` (dpc = deaths/(pop/1e5), region='CAD')]

# US data: https://www.drugabuse.gov/drug-topics/trends-statistics/overdose-death-rates
# Estimate that this year will be 27% higher: https://www.cdc.gov/nchs/nvss/vsrr/drug-overdose-data.htm
dat_us_deaths = data.table(year=seq(2018,2020),deaths=c(46802,49860,49860*1.27),pop=c(327096265,329064917,331002651))
dat_us_deaths[,`:=` (dpc = deaths/(pop/1e5), region='US')]
dat_both_deaths = rbind(dat_cad_deaths, dat_us_deaths)

# (i) Compare CAD & US
gg_cad_comp = ggplot(dat_both_deaths,aes(x=as.character(year),y=dpc,fill=region)) + 
    theme_bw() + 
    geom_bar(color='black',stat='identity',position='dodge',width=0.65) + 
    scale_fill_discrete(name='Country') + 
    labs(y='Opioid deaths per 100K') + 
    theme(axis.title.x=element_blank(),axis.text.x=element_text(angle=90))
# (ii) Annual US Deaths
gg_us_qtr = ggplot(dat_deaths[icd10=='all'],aes(x=as.Date(yqtr),y=4*dpc)) + geom_line() + 
    theme_bw() + 
    labs(y='Annualized deaths per 100K',subtitle='Quarterly US overdose deaths') + 
    theme(axis.title.x=element_blank(),axis.text.x=element_text(angle=90)) + 
    scale_x_date(date_breaks='2 years',date_labels='%Y')
gg_deaths_comp = plot_grid(gg_cad_comp,gg_us_qtr,nrow=1,align='hv',axis='tb')
save_plot(file.path(dir_figures,'gg_deaths_comp.png'), gg_deaths_comp, base_height=4,base_width=10)


###################################
# --- (5) DEATHS AND POLICIES --- #

# Calculate state-level pop
dat_pop_state = dat_cdc[,list(pop=mean(pop)),by=list(state,yqtr)]
dat_pop_state[, yqtr := as.integer(format(yqtr,'%Y'))+(as.integer(format(yqtr,'%q'))-1)/4]

# How does the distribution of (per capita) deaths compare to the treated vs untreated states?
cn_idx = c('state','year','t0_date') #'quarter'
cn_policy_lvl <- c('access','must','pillmill','dayslimit','goodsam','naloxone','medicaid')
cn_policy_lbl <- c('PDMP access','Mandatory PDMP','Pain Clinic Law','Prescription Limit Law','Good Samaritan Law','Naloxone Law','Medicaid Expansion')
df_policy = ddata[,c(cn_idx,cn_policy_lvl,cn_mort_lvl),with=F]
setnames(df_policy,c('t0_date',cn_mort_lvl),c('yqtr',cn_mort_lbl))
# Melt over mortality types
df_policy = melt(df_policy,measure.vars=cn_mort_lbl,variable.name='mort',value.name='deaths_pc')
# Melt over policy types
df_policy = melt(df_policy,measure.vars=cn_policy_lvl,variable.name='policy',value.name='ptime')
# Add state-level population to back out the raw deaths
df_policy = merge(df_policy,dat_pop_state,by=c('state','yqtr'),all.x=T)
df_policy[, `:=` (deaths = round(deaths_pc * pop), deaths_pc=NULL)]
df_policy = df_policy[order(state,mort,policy,yqtr)]
# Puerto rico has missing data
df_policy = df_policy[state != 'PR']
# Assign weather policy is active
df_policy[, p_active := ifelse(is.na(ptime),F, ifelse(yqtr >= ptime,T,F))]
df_policy[state == 'TX' & yqtr == 2017.25 & mort == 'all']
# Fancy labels
df_policy[, `:=` (mort=factor(mort, names(cn_icd10_lvl), as.character(cn_icd10_lvl)), 
                  policy=factor(policy,cn_policy_lvl,cn_policy_lbl))]

# --- (i) PERCENTAGE OF STATES WITH A POLICY --- #
df_pct_policy = df_policy[mort=='Heroin' & policy!='Medicaid Expansion',list(pct=mean(p_active)),by=list(policy,yqtr)]
gg_pct_policy = ggplot(df_pct_policy,aes(x=yqtr,y=pct,color=policy,shape=policy)) + 
    theme_bw() + geom_line() + geom_point() + 
    labs(y='% of states with policy') + 
    theme(axis.title.x=element_blank()) + 
    scale_color_discrete(name='Policy')
save_plot(file.path(dir_figures,'gg_pct_policy.png'), gg_pct_policy, base_height=4,base_width=6)


# --- (ii) DEATHS RATE THOSE WITH AND WITHOUT A POLICY --- #
df_comp_policy <- df_policy[policy!='Medicaid Expansion',list(deaths_pc=1e5*sum(deaths)/sum(pop)),by=list(policy,mort,yqtr,p_active)]
# Get 1000 bootstrap
tmp_df1 = data.table::copy(df_policy[policy!='Medicaid Expansion'])
n = nrow(tmp_df1)
nsim = 1000
alpha = 0.05
path_bs_comp = file.path(dir_output,'bs_comp_policy.csv')
cn_old = c('FALSE','TRUE')
cn_new = c('p_F','p_T')
if (!file.exists(path_bs_comp)) {
    print('Running bootstrap iterations')
    set.seed(nsim)
    holder = list()
    for (i in seq(nsim)) {
        if ((i %% 25) == 0) { print(i) }
        tmp_df2 = tmp_df1[,.SD[sample(.N,replace=TRUE)],by = list(policy,mort,yqtr,p_active)]
        tmp_df2 = tmp_df2[,list(deaths_pc=1e5*sum(deaths)/sum(pop),idx=i),by=list(policy,mort,yqtr,p_active)]
        holder[[i]] = tmp_df2
    }
    tmp_df2 = rbindlist(holder)
    tmp_df3 = dcast(tmp_df2,'policy+mort+yqtr+idx~p_active',value.var='deaths_pc')
    setnames(tmp_df3,cn_old,cn_new)
    tmp_df3[, `:=` (deaths_pc = p_T - p_F, p_active='diff')]
    tmp_df4 = rbind(tmp_df2, tmp_df3[!is.na(deaths_pc),-cn_new,with=F])
    tmp_df = tmp_df4[,list(lb=quantile(deaths_pc,alpha/2), ub=quantile(deaths_pc,1-alpha/2)),by=list(policy,mort,yqtr,p_active)]
    fwrite(tmp_df,path_bs_comp)
    rm(holder)
} else {
    print('Bootstraps already exists')
    tmp_df = fread(path_bs_comp)
}
# Get the "diff"
tmp_comp = dcast(df_comp_policy,'policy+mort+yqtr~p_active',value.var='deaths_pc')
setnames(tmp_comp, cn_old, cn_new)
tmp_comp[, `:=` (deaths_pc = p_T - p_F, p_active='diff')]
tmp_comp = tmp_comp[!is.na(deaths_pc),-cn_new,with=F]
df_comp_policy = rbind(df_comp_policy, tmp_comp)
df_comp_policy = merge(df_comp_policy,tmp_df)
df_comp_policy[, `:=` ( p_active=factor(p_active,c('TRUE','FALSE','diff'),c('Yes','No','Diff')) )]
gg_comp_policy = ggplot(df_comp_policy[p_active != 'Diff'],aes(x=yqtr,y=deaths_pc,color=p_active)) + 
    theme_bw() + geom_line() + 
    geom_ribbon(aes(ymin=lb,ymax=ub,fill=p_active),alpha=0.25) + 
    labs(y='Deaths per 100k',subtitle='Area shows 95% CI') + 
    theme(axis.title.x=element_blank()) + 
    facet_grid(mort~policy,scales='free_y') + 
    scale_color_discrete(name='Has policy') + 
    guides(fill=FALSE) + 
    scale_x_continuous(limits=c(2005,2019),breaks=seq(2006,2020,3))
save_plot(file.path(dir_figures,'gg_comp_policy.png'), gg_comp_policy, base_height=12,base_width=15)


gg_diff_policy = ggplot(df_comp_policy[p_active == 'Diff'],aes(x=yqtr,y=deaths_pc)) + 
    theme_bw() + geom_line() + 
    geom_ribbon(aes(ymin=lb,ymax=ub),fill='black',alpha=0.25) + 
    labs(y='Difference in deaths per 100k',subtitle='Area shows 95% CI') + 
    theme(axis.title.x=element_blank()) + 
    facet_grid(mort~policy,scales='free_y') + 
    guides(fill=FALSE) + 
    geom_hline(yintercept=0,color='blue') + 
    scale_x_continuous(limits=c(2005,2019),breaks=seq(2006,2020,3))
save_plot(file.path(dir_figures,'gg_diff_policy.png'), gg_diff_policy, base_height=12,base_width=15)
