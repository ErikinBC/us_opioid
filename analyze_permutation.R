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
for (i in as.character(seq(nsim))) {
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

#############################################
# ----- (2) COMPARE TO POINT ESTIMATE ----- #

df_tab[,list(coef=mean(coef),zscore=mean(zscore)),by=list(outcome,treatment)]
df_coef[,list(coef=mean(coef),zscore=mean(zscore)),by=list(outcome,treatment,Lead)]

################################
# ----- (3) DO INFERENCE ----- #

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
    ggtitle('Meta-analysis distribution (randomized policy dates)') + 
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
    labs(subtitle='Dashed lines show 95% CI of z-score mean') + 
    ggtitle('Solid lines show 95% CI of z-score mean\nDashed lines show Lee et. al estimate') + 
    scale_x_continuous(limits=c(-xx,xx)) + 
    geom_vline(aes(xintercept=zscore,color=treatment),data=coef_pmatch,linetype=2)
save_plot(file.path(dir_figures,'gg_coef_zscore.png'),gg_coef_zscore,base_height=8,base_width=16)
