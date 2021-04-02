# SCRIPT TO FAMILIZIZE DATA

pckgs <- c('data.table','cowplot','magrittr','stringr','zoo','ggplot2')
for (pckg in pckgs) { library(pckg, character.only=TRUE) }

# specify your project root directory here
here = getwd()
dir_olu = file.path(here,'..')
dir_figures = file.path(dir_olu, 'figures')
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
cn_ddata <- colnames(ddata)
cn_mort <- c(str_subset(cn_ddata,'^mortality'),'opioid_mortality_per100k')

df_mort = melt(ddata,id.vars=c('state','year','quarter'),measure.vars=cn_mort,variable.name='mortality',value.name='deaths')
df_mort[,mortality := str_replace(mortality,'mortality_','')]
df_mort[,mortality := ifelse(mortality == 'opioid_per100k','all',mortality)]
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
dat_deaths[,`:=` (dpc = 100000 * deaths / pop, dpc_interp=100000 * deaths / pop_interp)]

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
gg_dpc = ggplot(dat_deaths,aes(x=as.Date(yqtr),y=dpc)) + geom_line() + 
    theme_bw() + 
    labs(y='Deaths per 100K',title='Total US Overdose deaths') + 
    theme(axis.title.x=element_blank(),axis.text.x=element_text(angle=90)) + 
    facet_wrap(~icd10,labeller=labeller(icd10=cn_icd10_lvl),scales='free_y') + 
    scale_x_date(date_breaks='2 years',date_labels='%Y')
save_plot(file.path(dir_figures,'gg_dpc.png'),gg_dpc,base_height=8,base_width=10)

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

###################################
# --- (4) DEATHS AND POLICIES --- #

# How does the distribution of (per capita) deaths compare to the treated vs untreated states?

cn_idx = c('state','year','qtr','t0_date')
cn_policy <- c('access', 'enactment','dayslimit','goodsam','pillmill','naloxone','medicaid')
# Append to population to back out the raw deaths


ddata[1:2,] %>% t

dat_deaths %>% head
