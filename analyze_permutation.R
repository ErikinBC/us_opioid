pckgs <- c('data.table','ggplot2','cowplot','forcats','stringr')
for (pckg in pckgs) { library(pckg, character.only=TRUE) }

source('funs_support.R')

here = getwd()
dir_olu = file.path(here,'..')
dir_results = file.path(dir_olu,'results') 
dir_output = file.path(dir_olu,'output')
dir_permute = file.path(dir_output,'permute')
dir_figures = file.path(dir_olu,'figures')

#############################
# ----- (1) LOAD DATA ----- #

fn_permute = list.files(dir_permute)
nsim = max(as.integer(str_replace_all(fn_permute,'\\.csv|coef_pmatch_|tab_','')))
cn_coef <- c('time_id','coef','se','lci','uci','outcome','treatment','seed_idx')
holder_coef = list()
holder_tab = list()
for (i in as.character(seq(0,nsim))) {
    fn_tab_i = file.path(dir_permute,str_c('tab_',i,'.csv'))
    fn_coef_i = file.path(dir_permute,str_c('coef_pmatch_',i,'.csv'))
    if (file.exists(fn_tab_i)) { holder_tab[[i]] = fread(fn_tab_i) }
    if (file.exists(fn_coef_i)) { holder_coef[[i]] = fread(fn_coef_i,select=cn_coef) }
}
df_tab = rbindlist(holder_tab)
df_coef = rbindlist(holder_coef)
rm(list=c('holder_tab','holder_coef'))
# assign z-score
df_tab[, zscore := coef/se]
df_coef[, zscore := coef/se]
# Use only 12 leads
df_coef = df_coef[time_id!='t+0']
df_coef[, time_id := as.integer(str_replace(time_id,'t\\+',''))]
setnames(df_coef,'time_id','Lead')

depvars = unique(df_tab$outcome)
treatment_list = unique(df_tab$treatment)

# Load in the point estimates
coef_pmatch = fread(file.path(dir_results,'coef_pmatch.csv'))
coef_pmatch = coef_pmatch[treatment %in% treatment_list & outcome %in% depvars]
coef_pmatch = coef_pmatch[time_id!='t+0']
coef_pmatch[, time_id := as.integer(str_replace(time_id,'t\\+',''))]
setnames(coef_pmatch,'time_id','Lead')
coef_pmatch[, zscore := coef/se]

meta_pmatch = fread(file.path(dir_results,'pmatch_meta_analysis.csv'))
meta_pmatch = meta_pmatch[treatment %in% treatment_list & outcome %in% depvars]
meta_pmatch[, `:=` (coef=coef/3000, lci=lci/3000, uci=uci/3000)]
meta_pmatch[, zscore := coef/se]

# Estimates are close enough for seed 0 lines up with author's estimate
cn1 = c('coef','treatment')
merge(meta_pmatch[,cn1,with=F],df_tab[seed_idx == 0,cn1,with=F],by='treatment')
cn2 = c('coef','treatment','Lead')
merge(coef_pmatch[,cn2,with=F],df_coef[seed_idx == 0,cn2,with=F],by=c('treatment','Lead'))
df_tab = df_tab[seed_idx>0]
df_coef = df_coef[seed_idx>0]

############################################
# ----- (2) ZSCORE SHOWS RIGHT SHIFT ----- #

cn_lvl = c(naloxone_tr='Naloxone',goodsam_tr='Good Samaritan')

# Even the coefficient is better behaved, it still is not that significant
cn_match = c('outcome','treatment','coef')
pval_tab = merge(df_tab[,cn_match,with=F],meta_pmatch[,cn_match,with=F],by=c(cn_match[1:2]))
pval_tab_sum = pval_tab[,list(pos=mean(coef.x>0)-0.5,pval=(sum(coef.x > coef.y)+1)/(length(coef.x)+1) ),by=treatment]
cn_match = c('Lead',cn_match)
pval_coef = merge(df_coef[,cn_match,with=F],coef_pmatch[,cn_match,with=F],by=c(cn_match[1:3]))
pval_coef_sum = pval_coef[,list(pos=mean(coef.x>0)-0.5,pval=(sum(coef.x > coef.y)+1)/(length(coef.x)+1)),by=list(Lead,treatment)][order(treatment)]
pval_perm = rbind(cbind(Lead='REMA',pval_tab_sum),pval_coef_sum)
pval_perm[,Lead := factor(Lead,c('REMA',1:12),c('REMA',str_c('Q',1:12)))]
pval_perm[, fdr := p.adjust(pval,method='fdr')]
pval_perm = melt(pval_perm,c('Lead','treatment'),c('pval','fdr'),'adj','pval')
t1 = str_c(rep(names(cn_lvl),2),rep(c('pval','fdr'),each=2),sep='_')
t2 = str_c(rep(cn_lvl,2),rep(c('(raw)','(FDR)'),each=2),sep=' ')
pval_perm[, cc := factor(str_c(treatment,'_',adj),t1,t2)]
pval_perm[2:3,] %>% t
colz = rep(gg_color_hue(2),2)
shz = rep(c(19,8),each=2)

stit = 'P-value is number of randomized coefficient greater than observed coefficient'
gg_pval_perm = ggplot(pval_perm,aes(x=Lead,y=pval,color=cc,shape=cc)) + 
    theme_bw() + geom_point(position=position_dodge(width=0.5)) + 
    labs(y='P-value',x='Estimator',subtitle=stit) + 
    scale_color_manual(name='Policy (p-value): ',values=colz) + 
    scale_shape_manual(name='Policy (p-value): ',values=shz) + 
    theme(legend.position='bottom') + 
    scale_y_continuous(limits=c(0,1)) + 
    geom_hline(yintercept=0.05,linetype=2) + 
    guides(color=guide_legend(nrow=2,byrow=TRUE))
save_plot(file.path(dir_figures,'gg_pval_perm.png'),gg_pval_perm,base_height=4,base_width=7)


df_tab[,list(coef=mean(coef),zscore=mean(zscore)),by=list(outcome,treatment)]
df_coef[,list(coef=mean(coef),zscore=mean(zscore)),by=list(outcome,treatment,Lead)]

se_tab_z = df_tab[,list(mu=mean(zscore),se=mean(se)),by=list(outcome,treatment,coef>0)]
se_tab_z = se_tab_z[order(treatment,coef)]

df_tab[,coef0 := ifelse(coef<0,'Coef<0','Coef>=0')]
df_coef[,coef0 := ifelse(coef<0,'Coef<0','Coef>=0')]

# Make figures showing the distribution of the standard error
gg_se_tab = ggplot(df_tab,aes(x=se,fill=coef0)) + 
    theme_bw() + 
    stat_bin(aes(y=..density..), position='identity',alpha=0.4,bins=30,color='black') + 
    scale_fill_manual(name='Coefficient',values=c('green','purple'),labels=c('<0','>0')) + 
    facet_wrap('~treatment',labeller=labeller(treatment=cn_lvl)) + 
    labs(x='Standard error',y='Permutation frequency')
save_plot(file.path(dir_figures,'gg_se_tab.png'),gg_se_tab,base_height=3,base_width=6)

fun_lead = function(x) { str_c('Lead: ',x) }
gg_se_coef = ggplot(df_coef,aes(x=se,fill=coef0)) + 
    theme_bw() + 
    facet_wrap(~treatment+Lead,labeller=labeller(treatment=cn_lvl,Lead=fun_lead)) + 
    stat_bin(aes(y=..density..), position='identity',alpha=0.4,bins=30,color='black') + 
    scale_fill_manual(name='Coefficient',values=c('green','purple'),labels=c('<0','>0')) + 
    labs(x='Standard error',y='Permutation frequency')
save_plot(file.path(dir_figures,'gg_se_coef.png'),gg_se_coef,base_height=8,base_width=12)


################################
# ----- (3) DO INFERENCE ----- #

# We reject the null 37% of the time
df_tab[,list(reject=mean(zscore > qnorm(0.975)),z=mean(zscore),coef=mean(coef)),by=treatment]

ttest_tab_zscore = df_tab[,list(mu=mean(zscore),lb=t.test(zscore)$conf.int[1],
                ub=t.test(zscore)$conf.int[2]),by=list(outcome,treatment)]

ttest_df_zscore = df_coef[,list(mu=mean(zscore),lb=t.test(zscore)$conf.int[1],
                ub=t.test(zscore)$conf.int[2]),by=list(outcome,treatment,Lead)]

xx = ceiling(max(abs(df_tab$zscore)))
gg_tab_zscore = ggplot(df_tab, aes(x=zscore,fill=treatment)) + 
    theme_bw() + guides(color=FALSE) + 
    geom_histogram(position='identity',alpha=0.4,bins=25) + 
    scale_fill_discrete(name='Policy',labels=c('Good Samaritan','Naloxone Law')) + 
    # geom_vline(aes(xintercept=mu,color=treatment),data=ttest_tab_zscore) +
    geom_vline(aes(xintercept=lb,color=treatment),data=ttest_tab_zscore,linetype=1) + 
    geom_vline(aes(xintercept=ub,color=treatment),data=ttest_tab_zscore,linetype=1) + 
    geom_vline(xintercept=0) + 
    labs(y='Permutation frequency',x='Z-score') + 
    labs(subtitle='Solid lines show 95% CI of z-score mean\nDashed lines show Lee et. al estimate') + 
    # ggtitle('Meta-analysis distribution (randomized policy dates)') + 
    scale_x_continuous(limits=c(-xx,xx)) + 
    geom_vline(aes(xintercept=zscore,color=treatment),data=meta_pmatch,linetype=2)
save_plot(file.path(dir_figures,'gg_tab_zscore.png'),gg_tab_zscore,base_height=4,base_width=6)

xx = ceiling(quantile(abs(df_coef$zscore),0.999))
gg_coef_zscore = ggplot(df_coef, aes(x=zscore,fill=treatment)) + 
    theme_bw() + guides(color=FALSE) + 
    facet_wrap('~Lead',labeller=label_both,ncol=4) + 
    geom_histogram(position='identity',alpha=0.4,bins=25) + 
    scale_fill_discrete(name='Policy',labels=c('Good Samaritan','Naloxone Law')) + 
    # geom_vline(aes(xintercept=mu,color=treatment),data=ttest_df_zscore) +
    geom_vline(aes(xintercept=lb,color=treatment),data=ttest_df_zscore,linetype=1) + 
    geom_vline(aes(xintercept=ub,color=treatment),data=ttest_df_zscore,linetype=1) + 
    geom_vline(xintercept=0) + 
    labs(y='Permutation frequency',x='Z-score') + 
    labs(subtitle='Solid lines show 95% CI of z-score mean\nDashed lines show Lee et. al estimate') + 
    scale_x_continuous(limits=c(-xx,xx)) + 
    geom_vline(aes(xintercept=zscore,color=treatment),data=coef_pmatch,linetype=2)
save_plot(file.path(dir_figures,'gg_coef_zscore.png'),gg_coef_zscore,base_height=8,base_width=16)

