#------------------------------------------------------------------------------
# This file reads individual level data, and aggregate them to state levels
#==============================================================================

library(fst)
library(data.table)

here = getwd()

time_unit = 'quarter'
universe = 'ZIP5'

data = read_fst(file.path(here,'data','claims-individual',paste0('pdmp_individual_',time_unit,'.fst')),as.data.table=TRUE)

agg_time_unit = c('year','quarter')

# consider missing
list_var = c('sum_prescription','sum_MAT','sum_opioid','max_daily_mme','sum_overlapping',
	'with_overdosed','with_IO','with_ORD','nplusplus')
for (var in list_var) data[is.na(get(var)),(var) := 0]

# filter cancer/palliative care
agg_data = data[with_cancer == 0 & with_palliative == 0,.(
	n_people = .N,
	n_prescription = sum(sum_prescription),
	n_opioid_prescription = sum(sum_opioid),
	total_MAT_patients_per100k = mean(sum_MAT > 0),
	total_MAT_patients_per100k_addnaltrexon = mean(sum_MAT > 0 | naltrexon > 0, na.rm=TRUE),
	total_MAT_patients_per100k_addmeth = mean(sum_MAT > 0 | naltrexon > 0 | N_MAT_meth_inc, na.rm=TRUE),
	overdosed_patients_per100k = mean(with_overdosed > 0),
	ORD_patients_per100k = mean(with_ORD > 0),
	IO_patients_per100k = mean(with_IO > 0),
	total_opioid_patients_per100k = mean(sum_opioid > 0),
	prop_high_mme = mean(max_daily_mme > 90),
	total_overlapping_per100k = mean(sum_overlapping > 0),
	n_plusplus4_per100k = mean(nplusplus > 4)
	), by=c('state',agg_time_unit)]

fwrite(agg_data, file.path(here,'data','reg-data',paste0('claims_state_',tolower(universe),'_',time_unit,'.csv')))
