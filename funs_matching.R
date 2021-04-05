
# ---- FUNCTION TO GET NON-PARA ESTIMAT ----- # 
get_np_panel <- function(DV, treatment, covariate, treatment_lag=8) {
    # DV=depvar;treatment=policy_tr;covariate=covariates;treatment_lag=8
    covs.formula = as.formula(paste0('~ ',
            'I(lag(',DV,', 1:12))',
            '+ medicaid_tr',
            '+ ',paste0('lag(',covariate,', 1)',collapse = '+')
            ))  # this is the "Z"

    # run PM matching
    pmatch <- PanelMatch(
        outcome.var = DV,
        covs.formula = covs.formula, 
        treatment = treatment,time.id = 'time_id', unit.id = 'state_code', 
        refinement.method = 'CBPS.weight',match.missing = TRUE, 
        verbose=TRUE,
        forbid.treatment.reversal = TRUE, matching=TRUE, lag=treatment_lag,
        qoi='att',  lead = 0, data=as.data.frame(data))

    # access mean balance over time on DV
    all_i = 1:length(names(pmatch$att))
    holder_m = list()
    holder_w = list()
    for (i in all_i) {
        print(sprintf('%i of %i',i,length(all_i)))
        # Get the treatment state and time
        treated_st_time = names(pmatch$att)[i]
        treated_st = as.integer(unlist(strsplit(treated_st_time,'\\.'))[1])
        treated_time = as.integer(unlist(strsplit(treated_st_time,'\\.'))[2])
        # Find out who the "controls" are
        control_st_weight = attributes(pmatch$att[[i]])$weights
        control_st = data.table(DV=DV, treatment=treatment, state_code=as.integer(names(control_st_weight)),weight=control_st_weight)
        ctr_data = as.data.table(merge(data[,c('state_code','time_id',DV),with=FALSE],control_st,by='state_code'))
        control_st[, `:=` (state_code_tr=treated_st, state_code_ctr=state_code, state_code=NULL)]

        if (nrow(ctr_data) > 0) {
            # (i) Calculates weighted 
            ctr_data = ctr_data[, .(
                weighted_outcome = weighted.mean(get(DV), weight,na.rm=TRUE)
                ),by='time_id']
            setnames(ctr_data, 'weighted_outcome',DV)
            ctr_data[,type := 'matched']
            ctr_data[, time_id := time_id - treated_time]
            
            # (ii) Is is the data 
            tr_data = as.data.table(data[state_code==treated_st,c('time_id',DV),with=FALSE])
            tr_data[,type := 'treated']
            tr_data[, time_id := time_id - treated_time]

            # (iii) Save the data
            m_data = dcast(rbind(ctr_data,tr_data),'time_id~type',value.var=DV)        
            m_data[, `:=` (state_code_tr = treated_st, DV=DV, treatment=treatment)]
            holder_m[[i]] = m_data
            holder_w[[i]] = control_st
        }
    }
    holder_w = rbindlist(holder_w)
    holder_m = rbindlist(holder_m)    
    return(list(w=holder_w, m=holder_m))
}