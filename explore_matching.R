# Load packages
pckgs <- c('data.table','magrittr','stringr','purrr','forcats','tidytext',
           'ggplot2','cowplot',
           'parallel','zoo',
           'wfe','PanelMatch','survey')
for (pckg in pckgs) { library(pckg, character.only=TRUE) }

source('funs_support.R')
source('funs_matching.R')
# specify your project root directory here
here = getwd()
dir_olu = file.path(here,'..')
dir_results = file.path(dir_olu, 'results')
dir_data = file.path(dir_olu,'data')
dir_figures = file.path(dir_olu, 'figures')
dir_regdata = file.path(dir_data,'reg-data')

alph = 0.05
critv = qnorm(1-alph/2)

##################################
# --- (1) LOAD AND PREP DATA --- #

# Policies of intereet
cn_policy_lvl <- c('goodsam','naloxone') #'access','must','pillmill','dayslimit','medicaid'
cn_policy_lbl <- c('Good Samaritan Law','Naloxone Law') #'PDMP access','Mandatory PDMP','Pain Clinic Law','Prescription Limit Law','Medicaid Expansion'

# variables of interest
target_depvars = matrix(c(
    'opioid_mortality_per100k','All Opioid Mortality',
    'mortality_heroin','Heroin Mortality',
    'mortality_opioids','Natural Opioid Mortality',
    'mortality_methadone','Methadone Mortality',
    'mortality_synth','Synthetic Mortality',
    'mortality_cocaine','Cocaine Mortality'), ncol=2, byrow=TRUE)

depvars_label = target_depvars[,2]
depvars = target_depvars[,1]

list_treatment = paste0(c('access','must','naloxone','pillmill','goodsam','dayslimit'),'_tr')
time_unit = 'quarter'

# load data 
data = fread(file.path(dir_regdata,paste0('did_',time_unit,'_addmeth.csv')))
# adjust mortality data 
for (DV in depvars[1:6]) { data[,(DV) := get(DV)*1e5] }

# load state-level controls 
scontrols = fread(file.path(dir_regdata,paste0('cps_state_control_',time_unit,'.csv')))
fips_codes = fread(file.path(dir_data, 'tigris_fips_codes.csv'))
scontrols = merge(scontrols,unique(fips_codes[,c('state_code','state')]),all.x=TRUE, by.x='STATEFIP',by.y='state_code')

# re-specification of time-indicators to integer sequence
agg_time_unit = c('year','quarter')
data = merge(data, scontrols, by=c('state',agg_time_unit), all.x=TRUE, all.y=TRUE)
data[,time_id := year * 4 + quarter]
data[, time_id := as.integer(time_id - min(time_id)+1 )]

# sample selection based on the period
data = data[year >= 2007 & year <= 2018 & state != 'PR',]
# re-specification of state-indicators
data[, state_code := as.integer(as.factor(state))]
di_state = data[,list(.N),by=list(state,state_code)][,-'N']
di_time_id = data[,list(.N),by=list(t0_date,time_id)][,-'N']

# n_resident lines up with state population
data[,list(pop=max(n_resident)),by=list(state)][order(-pop)]

# Get the unique time of the outcomes
di_policy = melt(data,c('state'),cn_policy_lvl,'policy')
di_policy = di_policy[,list(t0_date=mean(value)),by=list(state,policy)]
di_policy[,policy_lbl := factor(policy,cn_policy_lvl,cn_policy_lbl)]
# di_policy[,max(t0_date,na.rm=T),by=policy]

# Load in the author calculated results
outcome_balance = fread(file.path(dir_results,'outcome_balance.csv'))

########################################
# --- (2) RUN NON-PARAMERIC TRENDS --- #

# See equation (18) for the DiD estimator

# Covariates to determine "weighting" on
covariates = c('p_female','p_age40_60','p_age60up',
    'p_white','p_black','p_asian','p_hispanic',
    'p_unemployed','p_poverty','n_resident')
head(data[,c('state','year',covariates[1:3]),with=F])

# Use overall opioid death rate
depvar = "opioid_mortality_per100k"
tlag = 8
flead = 8

lst_w = list()
lst_m = list() 
for (policy in cn_policy_lvl) {
    policy_tr = str_c(policy,'_tr')
    tmp_lst <- get_np_panel(depvar, policy_tr, covariates, treatment_lag=tlag)
    lst_w[[policy]] = tmp_lst$w
    lst_m[[policy]] = tmp_lst$m
    rm(tmp_lst)
}
# Combine weights
df_weight = rbindlist(lst_w)
df_weight[, `:=` (DV = factor(DV,depvars,depvars_label),treatment=factor(treatment,str_c(cn_policy_lvl,'_tr'),cn_policy_lbl))]
df_weight = merge(merge(df_weight,di_state,by.x='state_code_tr',by.y='state_code'),di_state,by.x='state_code_ctr',by.y='state_code')
setnames(df_weight,c('DV','treatment','state.x','state.y'),c('depvar','policy','state_tr','state_ctr'))
df_weight = df_weight[,-c('state_code_ctr','state_code_tr')]
# df_weight[,list(n=.N),by=list(policy,state_ctr)][order(-policy,-n)]

# Compare outcomes
df_np = rbindlist(lst_m)
df_np[, `:=` (DV = factor(DV,depvars,depvars_label),treatment=factor(treatment,str_c(cn_policy_lvl,'_tr'),cn_policy_lbl))]
df_np = merge(df_np,di_state,by.x='state_code_tr',by.y='state_code')
setnames(df_np,c('DV','treatment','state'),c('depvar','policy','state_tr'))
df_np[, state_code_tr := NULL]
df_np = melt(df_np,c('depvar','policy','time_id','state_tr'),NULL,'grp')
# Get the growth rate
df_np[,pct := value/shift(value)-1,by=list(depvar,policy,state_tr,grp)]
df_np[, pre_post := ifelse(time_id < 0, 'pre', 'post')]
# df_np[,list(.N),by=list(depvar,policy,state_tr,grp,is.na(pct))][`is.na`==T][,2:6][order(-N)]
# df_np[policy=='Naloxone Law' & state_tr=='ND'][,2:7]

rm(list=c('lst_w','lst_m'))
df_weight[,list(n=length(unique(state_tr))),by=policy]
df_np[,list(n=length(unique(state_tr))),by=policy]

# Compare to author calculated values
lee_np = outcome_balance[type != 'unmatched' & outcome == depvar & treatment %in% str_c(cn_policy_lvl,'_tr')]
setnames(lee_np,c('outcome','treatment','type','mean_outcome'),c('depvar','policy','grp','value'))
lee_np[, `:=` (depvar = factor(depvar,depvars,depvars_label),policy=factor(policy,str_c(cn_policy_lvl,'_tr'),cn_policy_lbl))]

setdiff(colnames(df_np),colnames(lee_np))

########################################
# --- (3) ANALYZE MATCHING WEIGHTS --- #

cn_gg_weight = c('depvar','policy','state_tr') #'state_ctr'

# --- (i) LOOK AT THE WEIGHT SUPREMUM --- #
# Get the "highest" word for each one
top_weight = df_weight[df_weight[,.I[weight == max(weight)],by=cn_gg_weight,with=T]$V1]
# Get the order of adoption
top_weight = merge(top_weight,di_policy[,-'policy'],by.x=c('state_tr','policy'),by.y=c('state','policy_lbl'))
top_weight[,yqtr := as.yearqtr(t0_date)]
top_weight[, xlbl := str_c(state_tr,' (',str_replace(yqtr,'20',''),')')]

gg_top_weight = ggplot(top_weight,aes(x=reorder_within(xlbl,t0_date,policy),y=weight,fill=policy)) + 
    theme_bw() + 
    geom_bar(stat='identity',color='black') + 
    scale_fill_discrete(name='Policy') + 
    labs(y='Control weight (max)',x='Treatment state') + 
    ggtitle('Largest weight in control set') + 
    geom_text(aes(label=state_ctr,y=weight+0.05),size=3,angle=90) +
    facet_wrap(~policy,scales='free_x') + 
    scale_x_reordered() + 
    theme(legend.position='bottom',axis.title.x=element_blank(),
          axis.text.x=element_text(angle=90,size=8))
save_plot(file.path(dir_figures,'gg_top_weight.png'),gg_top_weight,base_height=6,base_width=16)

# --- (ii) DISTRIBUTION OF WEIGHTS --- #
mu_weight = dcast(df_weight,'depvar+policy+state_tr~state_ctr',value.var='weight')
mu_weight = melt(mu_weight,cn_gg_weight,NULL,'state_ctr','weight')
mu_weight[, weight := ifelse(is.na(weight),0,weight)]
mu_weight = mu_weight[order(depvar,policy,-weight)]
mu_weight = mu_weight[,list(weight=mean(weight)),by=list(depvar,policy,state_ctr)]
mu_weight[,cweight := cumsum(weight),by=list(depvar,policy)]

# mu_weight[,list(max(cweight)),by=list(depvar,policy)]

posd = position_dodge(0.5)
gg_csum_weights = ggplot(mu_weight,aes(x=reorder_within(state_ctr,cweight,policy),y=cweight,color=policy,group=policy)) +
    geom_point(position=posd) + geom_line(position=posd) + 
    labs(y='Cumulative average control weight',x='Control state') + 
    theme_bw() + 
    theme(legend.position='bottom',axis.text.x=element_text(size=7,angle=90)) + 
    scale_x_reordered() + 
    scale_color_discrete(name='Policy') + 
    facet_wrap(~policy,scales='free_x')
save_plot(file.path(dir_figures,'gg_csum_weights.png'),gg_csum_weights,
          base_height=5,base_width=12)

############################################
# --- (4) ANALYZE NON-PARAMERIC TRENDS --- #

# --- (i) CHECK THAT I MY NON-PARA VERY CLOSE TO EFIGURE 1 --- #

erik_np = df_np[,list(value=mean(value),n=.N),by=list(depvar,policy,grp,pre_post,time_id)]
comp_np = merge(erik_np,lee_np,by=c('depvar','policy','grp','pre_post','time_id'),suffixes=c('_erik','_lee'))[order(depvar,policy,grp,pre_post,time_id)]
comp_np = melt(comp_np,measure.vars=c('value_erik','value_lee'),variable.name='msr')
comp_np %>% head
# Data lines up very closely
gg_comp_np = ggplot(comp_np,aes(x=time_id,y=value,color=msr,shape=pre_post)) + 
    theme_bw() + geom_line() + 
    facet_grid(policy ~ grp)
save_plot(file.path(dir_figures,'gg_comp_np.png'),gg_comp_np,
          base_height=8,base_width=10)
# What is driving the small difference? Puerto Rico? Other subsets?

# On average, the parallel trends assumption does not "seem" to be violated
tmp_fig1 = erik_np[time_id >= -12 & time_id<=12]
gg_erik_fig1 = ggplot(tmp_fig1,aes(x=time_id,y=value,color=grp)) + 
    theme_bw() + geom_point() + geom_line() + 
    labs(y='Average opioid mortality death (per 100K)',x='Quarters before and after policy') + 
    facet_wrap(~policy) + 
    geom_vline(xintercept=0) + 
    scale_color_discrete(name='Group',labels=c('Control','Treatment'))
save_plot(file.path(dir_figures,'gg_erik_fig1.png'),gg_erik_fig1,
          base_height=5,base_width=10)

# Do only for naloxone laws
gg_naloxone_pt = ggplot(tmp_fig1[str_detect(policy,'Naloxone')],aes(x=time_id,y=value,color=grp)) + 
    theme_bw() + geom_point() + geom_line() + 
    labs(y='Average opioid mortality death (per 100K)',x='Quarters before and after policy') + 
    geom_vline(xintercept=0) + facet_wrap(~policy) + 
    scale_color_discrete(name='Group',labels=c('Control','Treatment')) + 
    theme(legend.position=c(0.25,0.75))
save_plot(file.path(dir_figures,'gg_naloxone_pt.png'),gg_naloxone_pt,
          base_height=4,base_width=5)


# --- (ii) COMPARE THE DISTRIBUTION OF GROWTH RATES --- #

df_no_deaths = df_np[,list(n=.N),by=list(depvar,policy,state_tr,grp,value==0)]
states_no_deaths = unique(df_no_deaths[value == TRUE][,-c('value')]$state_tr)
print(sprintf('There are %i states that have no deaths in some quarters: %s',length(states_no_deaths),str_c(states_no_deaths,collapse=', ')))

df_pct = df_np[!(state_tr %in% states_no_deaths) & !is.na(pct)] # & (time_id >= -tlag) & (time_id <= flead)
stopifnot(any(!is.na(df_pct$pct)))

df_pct_mu = df_pct[,list(mu=mean(pct),se=sd(pct),n=.N,lb=quantile(pct,0.25),ub=quantile(pct,0.75)),by=list(depvar,policy,grp,time_id<0)]
df_pct_mu[, `:=` ( time_id = ifelse(time_id, 'Before', 'After'))] #, lb=mu-critv*se, ub=mu+critv*se 
df_pct_wide = dcast(df_pct_mu,'depvar+policy+time_id~grp',value.var=c('mu','se','n'))
df_pct_wide[,z_score := (mu_treated-mu_matched)/sqrt(se_treated**2/n_treated + se_matched**2/n_matched)]
df_pct_wide$z_score

n_sim = 1000
holder_sim = list()
set.seed(n_sim)
for (i in seq(n_sim)) { 
    holder_sim[[i]] = df_pct_wide[,list(idx=i,pct=mean(rnorm(n_treated,mu_treated,se_treated)-mean(rnorm(n_matched,mu_matched,se_matched)))),by=list(depvar,policy,time_id)]
}
df_pct_sim = rbindlist(holder_sim)
df_pct_sim_bounds = df_pct_sim[,list(lb=quantile(pct,alph/2),ub=quantile(pct,1-alph/2)),by=list(depvar,policy,time_id)]

# --- (i) Distribution of growth rates --- #
gg_pct_lag = ggplot(df_pct_mu,aes(x=time_id,y=mu,color=grp)) + 
    theme_bw() + geom_point(position=posd) +
    geom_linerange(aes(ymin=lb,ymax=ub),position=posd) +  
    facet_wrap(~policy) + 
    scale_color_discrete(name='Group',labels=c('Control','Treatment')) + 
    labs(x='Treatment time',y='Quarterly growth rates',subtitle='Vertical lines show IQR')
# Save
save_plot(file.path(dir_figures,'gg_pct_lag.png'),gg_pct_lag,
          base_height=5,base_width=10)

gg_pct_dist = ggplot(df_pct_sim,aes(x=pct,fill=time_id)) + 
    theme_bw() + 
    geom_histogram(position='identity',alpha=0.5,bins=25) + 
    facet_wrap(~policy) + 
    geom_vline(aes(xintercept=lb,color=time_id),data=df_pct_sim_bounds,linetype=2) + 
    geom_vline(aes(xintercept=ub,color=time_id),data=df_pct_sim_bounds,linetype=2) + 
    scale_fill_discrete(name='Treatment time') + 
    guides(color=FALSE) + 
    geom_vline(xintercept=0) + 
    # scale_x_continuous(limits=c(-0.025,0.05)) + 
    labs(y='Frequency',x='Average quarterly growth diff. (treated - control)',subtitle='Vertical lines show 95% CI')
save_plot(file.path(dir_figures,'gg_pct_dist.png'),gg_pct_dist,
          base_height=3.5,base_width=10)

###############################
# --- (5) IMPLETEMENT ATT --- #

# See equation (18)
df_att = data.table::copy(df_np[time_id>=-1 & time_id<=12,-'pct'])
tmp = df_att[df_att[,.I[time_id == -1],by=list(depvar,policy,state_tr,grp)]$V1,-c('time_id','pre_post')]
setnames(tmp,'value','v0')
df_att = merge(df_att,tmp,by=c('depvar','policy','state_tr','grp'))
df_att[,dval := value - v0]
df_att = dcast(df_att,'depvar+policy+state_tr+time_id~grp',value.var='dval')
df_att[, dy := treated - matched]
df_att_mu = df_att[time_id>=0,list(mu=mean(dy),lb=quantile(dy,0.25),ub=quantile(dy,0.75),n=.N),by=list(depvar,policy,time_id)]

gg_att_np = ggplot(df_att_mu,aes(x=time_id,y=mu,color=policy)) + 
    theme_bw() + geom_point(position=posd) + 
    labs(x='Quarters from policy',y='Average change in total opioid deaths',subtitle='Vertical lines show IQR') + 
    geom_linerange(aes(ymin=lb,ymax=ub),position=posd) + 
    scale_color_discrete(name='Policy: ') + 
    geom_hline(yintercept=0,linetype=2) + 
    theme(legend.position='bottom')
save_plot(file.path(dir_figures,'gg_att_np.png'),gg_att_np,
          base_height=4.5,base_width=5)


##########################################
# --- (6) ONLINE LOGISTIC REGRESSION --- #

# "To estimate the propensity score, we first create a subset of the data, consisting of all treated observations and their matched control observations from the same year.""
# See equation (13)

bl_data = tmp_data = data[,c('state','year','quarter',depvar,covariates,'medicaid_tr'),with=F]
bl_data[, medicaid_tr := ifelse(is.na(medicaid_tr),0,medicaid_tr)]

cn_gg_idx <- c('policy','depvar','state')
state_tr_lst = unique(df_weight$state_tr)
holder = list()
for (ss in state_tr_lst) {
    print(ss)
    # Temporary list of control states
    tmp1 = df_weight[state_tr == ss,-c('state_tr','weight')]
    tmp2 = df_weight[state_tr == ss,list(.N),by=list(depvar,policy,state_tr)][,-'N']
    setnames(tmp1,'state_ctr','state')
    setnames(tmp2,'state_tr','state')
    state_ctr = rbind(tmp1, tmp2)
    state_ctr[, is_tr := ifelse(state == ss, 1, 0)]
    # year of policy change
    t0_policy = di_policy[state == ss,-c('state','policy')]
    setnames(t0_policy,'policy_lbl','policy')
    tmp_data = merge(state_ctr,bl_data,by='state',allow.cartesian=TRUE)
    tmp_data = merge(t0_policy,tmp_data,by='policy',allow.cartesian=TRUE)
    tmp_data[, `:=` (t0 = year + (quarter-1)/4,year=NULL,quarter=NULL)]
    tmp_data[, `:=` (time_id=4*(t0-t0_date), t0_date=NULL, t0=NULL ) ]
    setnames(tmp_data, c(depvar), c('y'))
    # The X data is the 12 lags of the y, medicaid, and 1 lag of the covariates
    Z1 = tmp_data[time_id == -1,c(cn_gg_idx,covariates),with=F]
    Y1 = tmp_data[time_id >= -12 & time_id <= -1,c(cn_gg_idx,'time_id','y'),with=F]
    Y1 = dcast(Y1, str_c(str_c(cn_gg_idx,collapse='+'),'~time_id'),value.var='y')
    X1 = tmp_data[time_id == 0,c(cn_gg_idx,'is_tr'),with=F]  
    X1 = cbind(data.table(state_tr=ss),X1)
    regdata = merge(merge(X1,Y1,by=cn_gg_idx),Z1,by=cn_gg_idx)
    holder[[ss]] = regdata
}
dat_yX = rbindlist(holder)
cn_y = c(cn_gg_idx,'state_tr','is_tr')

holder = list()
for (ss in state_tr_lst) {
    tmp_ss = dat_yX[state_tr != ss]
    y_ss = tmp_ss[['is_tr']]
    X_ss = tmp_ss[,-cn_y,with=F]
    X_test = dat_yX[state_tr == ss,-cn_y,with=F]
    mdl_ss = glm(y_ss ~ ., data=X_ss,family=binomial)
    tmp_y = dat_yX[state_tr == ss,cn_y,with=F]
    tmp_y[, phat := predict(mdl_ss,X_test)]
    holder[[ss]] = tmp_y
}
dat_y = rbindlist(holder)

auroc <- function(y, score) {
  n1 <- sum(y==1)
  n0 <- length(y) - n1
  num <- sum(rank(score)[y==1]) - n1*(n1+1)/2
  den <- n1*n0
  return(num/den)
}

auroc(c(1,1,0,0,0),c(5,4,6,4,1))

# Get the AUROC and CI
cn_gg_auc = c('depvar','policy')
df_auc = dat_y[,list(auc=auroc(is_tr,phat)),by=cn_gg_auc,with=T]
# Repeated with simulation
nsim = 1000
holder = list()
set.seed(1234)
for (i in seq(nsim)) {
    tmp_i = dat_y[dat_y[, sample(.N, replace=TRUE), by=list(policy,depvar)]$V1,list(auc=auroc(is_tr,phat)),by=cn_gg_auc,with=T]
    holder[[i]] = tmp_i
}
df_auc_bs = rbindlist(holder)
df_auc_bs_bounds = df_auc_bs[,list(lb=quantile(auc,alph/2),ub=quantile(auc,1-alph/2)),by=cn_gg_auc,with=T]


gg_auc_ps = ggplot(df_auc_bs, aes(x=auc,fill=policy)) + 
    theme_bw() + theme(legend.position='bottom') + 
    scale_fill_discrete(name='Policy: ') + 
    labs(x='AUROC (bootstrap distribution)',y='Frequency',subtitle='Dashed lines show 95% CI\nSolid line shows point estimate') + 
    geom_histogram(position='identity',alpha=0.5,bins=25) + 
    geom_vline(aes(xintercept=auc,color=policy),data=df_auc,linetype=1) + 
    geom_vline(aes(xintercept=lb,color=policy),data=df_auc_bs_bounds,linetype=2) + 
    geom_vline(aes(xintercept=ub,color=policy),data=df_auc_bs_bounds,linetype=2) + 
    guides(color=FALSE)
save_plot(file.path(dir_figures,'gg_auc_ps.png'),gg_auc_ps,
          base_height=4.5,base_width=5)

