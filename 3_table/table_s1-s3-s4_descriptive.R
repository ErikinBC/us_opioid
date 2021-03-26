#------------------------------------------------------------------------------
# generate descriptive tables for optum data
#==============================================================================

library(fst)
library(data.table)

here = getwd()

time_unit = 'quarter'
universe = 'ZIP5'

zip5_mbr = read_fst(file.path(here,'data','claims-individual',paste0('pdmp_individual_',time_unit,'.fst')),as.data.table=TRUE)
zip5_mbr = zip5_mbr[state != 'PR',]

# adjust medicare status
zip5_mbr[is.na(lowinc)|lowinc == 9, medicare := 0]
zip5_mbr[lowinc == 0 | lowinc == 1, medicare := 1]
zip5_mbr[lowinc == 9, lowinc := 0]

# create opioid indicator (whether patients receive opioids)
zip5_mbr[,is_opioid := ifelse(sum_opioid > 0, 1L, 0L)]
zip5_mbr[,is_MAT := ifelse(sum_MAT > 0, 1L, 0L)]

#------------------------------------------------------------------------------
# table s1 : general representation of optum data by states
#==============================================================================
state_zip5_mbr = zip5_mbr[, .(
	age = mean(age, na.rm=TRUE), 
	female=mean(female, na.rm=TRUE),
	lowinc = mean(lowinc,na.rm=TRUE),
	medicare = mean(medicare,na.rm=TRUE),
	cancer = mean(with_cancer,na.rm=TRUE),
	palliative = mean(with_palliative,na.rm=TRUE),
	overdose = mean(with_overdosed,na.rm=TRUE),
	sum_rx = sum(sum_prescription,na.rm=TRUE),
	sum_op_rx = sum(sum_opioid,na.rm=TRUE),
	sum_mat = sum(sum_MAT, na.rm=TRUE)
	), by=c('PATID','state')]

tab = state_zip5_mbr[,.(
	total_patient = .N,
	total_medicare = sum(medicare > 0, na.rm=TRUE),
	total_lowinc = sum(lowinc > 0, na.rm=TRUE),
	total_prescription = sum(sum_rx, na.rm=TRUE),
	total_overdosed = sum(overdose > 0, na.rm=TRUE)), by='state']

write.csv(tab, file.path(here,'results','table_s1_state.csv'))

#------------------------------------------------------------------------------
# table s3; general characteristics
#==============================================================================

# make a unique patient files
pat_zip5_mbr = zip5_mbr[, .(
	age = mean(age, na.rm=TRUE), 
	female=mean(female, na.rm=TRUE),
	lowinc = mean(lowinc,na.rm=TRUE),
	medicare = mean(medicare,na.rm=TRUE),
	cancer = mean(with_cancer,na.rm=TRUE),
	palliative = mean(with_palliative,na.rm=TRUE),
	overdose = mean(with_overdosed,na.rm=TRUE),
	sum_rx = sum(sum_prescription,na.rm=TRUE),
	sum_op_rx = sum(sum_opioid,na.rm=TRUE),
	sum_mat = sum(sum_MAT, na.rm=TRUE)
	), by='PATID']

# create an indicator
pat_zip5_mbr[,with_cancer := ifelse(cancer > 0, 1, 0)]
pat_zip5_mbr[,with_palliative := ifelse(palliative > 0, 1, 0)]
pat_zip5_mbr[,with_overdosed := ifelse(overdose > 0, 1, 0)]

sel_vars = c('age','female','lowinc','medicare',
	'with_cancer','with_palliative','with_overdosed',
	'sum_rx','sum_op_rx','sum_mat')

mean = pat_zip5_mbr[,lapply(.SD, mean, na.rm=TRUE),.SDcols=sel_vars]
SD = pat_zip5_mbr[,lapply(.SD, sd, na.rm=TRUE),.SDcols=sel_vars]

tab1 = as.data.frame(round(cbind(t(mean), t(SD)),3))
colnames(tab1) = paste0('all_',c('mean','SD'))

mean = pat_zip5_mbr[with_cancer == 0 & with_palliative == 0,lapply(.SD, mean, na.rm=TRUE),.SDcols=sel_vars]
SD = pat_zip5_mbr[with_cancer == 0 & with_palliative == 0,lapply(.SD, sd, na.rm=TRUE),.SDcols=sel_vars]
tab2 = round(cbind(t(mean), t(SD)),3)
colnames(tab2) = paste0('anal_',c('mean','SD'))

mean = pat_zip5_mbr[with_cancer == 1 | with_palliative == 1,lapply(.SD, mean, na.rm=TRUE),.SDcols=sel_vars]
SD = pat_zip5_mbr[with_cancer == 1 | with_palliative == 1,lapply(.SD, sd, na.rm=TRUE),.SDcols=sel_vars]
tab3 = round(cbind(t(mean), t(SD)),3)
colnames(tab3) = paste0('cancer_',c('mean','SD'))

# total N 
print(nrow(pat_zip5_mbr))
print(nrow(pat_zip5_mbr[with_cancer == 0 & with_palliative == 0,]))
print(nrow(pat_zip5_mbr[with_cancer == 1 | with_palliative == 1,]))

write.csv(data.frame(cbind(tab1,tab2,tab3)), file.path(here,'results','table_s3_all.csv'))

#------------------------------------------------------------------------------
# table : basic characteristics across medicare types
#==============================================================================
# filter cancer/palliative care
zip5_mbr[, medicare_type := medicare]
zip5_mbr[lowinc==1, medicare_type := 2]

list_var = c('sum_prescription','sum_MAT','sum_opioid','max_daily_mme','sum_overlapping',
	'with_overdosed','with_IO','with_ORD','nplusplus')
for (var in list_var) zip5_mbr[is.na(get(var)),(var) := 0]

agg_data = zip5_mbr[with_cancer == 0 & with_palliative == 0,.(
	n_people = .N,
	age = mean(age, na.rm=TRUE), 
	female=mean(female, na.rm=TRUE),	
	n_prescription = sum(sum_prescription),
	n_opioid_prescription = sum(sum_opioid),
	total_MAT_patients_per100k = mean(sum_MAT > 0),
	overdosed_patients_per100k = mean(with_overdosed > 0),
	ORD_patients_per100k = mean(with_ORD > 0),
	IO_patients_per100k = mean(with_IO > 0),
	total_opioid_patients_per100k = mean(sum_opioid > 0),
	prop_high_mme = mean(max_daily_mme > 90),
	total_overlapping_per100k = mean(sum_overlapping > 0),
	n_plusplus4_per100k = mean(nplusplus > 4)
	), by=c('medicare_type')]

agg_data_all = zip5_mbr[with_cancer == 0 & with_palliative == 0,.(
	n_people = .N,
	age = mean(age, na.rm=TRUE), 
	female=mean(female, na.rm=TRUE),	
	n_prescription = sum(sum_prescription),
	n_opioid_prescription = sum(sum_opioid),
	total_MAT_patients_per100k = mean(sum_MAT > 0),
	overdosed_patients_per100k = mean(with_overdosed > 0),
	ORD_patients_per100k = mean(with_ORD > 0),
	IO_patients_per100k = mean(with_IO > 0),
	total_opioid_patients_per100k = mean(sum_opioid > 0),
	prop_high_mme = mean(max_daily_mme > 90),
	total_overlapping_per100k = mean(sum_overlapping > 0),
	n_plusplus4_per100k = mean(nplusplus > 4)
	)]
		
agg_data_all[,medicare_type := 3]

tab3 = rbind(agg_data,agg_data_all)
tab3[, medicare_type := factor(medicare_type, levels=0:3,labels=c('non-medicare','medicare','medicare_with_list','all'))]

write.csv(tab3, file.path(here,'results','table_s4_summary_by_medicare.csv'))


