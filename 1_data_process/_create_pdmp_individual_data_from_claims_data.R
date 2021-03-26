#------------------------------------------------------------------------------
# This file reads individual level data, and aggregate them to state levels
# we are not able to share the raw data, but contact Brea Perry (blperry@iu.edu)
# or BK Lee (bl11@iu.edu) to get access to these data
#==============================================================================

library(fst)
library(data.table)
library(docshop)

bucket = 's3://iuni-doctorshopping'

time_unit = 'quarter'
universe = 'zip5'

sel_var = c('PATID','state','age','female','lowinc',
	'with_cancer','with_palliative','with_overdosed','with_IO','with_ORD',
	'sum_prescription','sum_MAT','sum_opioid',
	'max_daily_mme','sum_overlapping','nplusplus',
	'naltrexon','N_MAT_meth_inc')

list_yq = c(paste0(rep(2007:2017, each=4),'q',1:4),'2018q1','2018q2','2018q3')

sel_var = c(sel_var, 'year','quarter')

read_quarterly_data = function(yq){
	message('now working on ',yq)
	data = s3read_gz(file.path(bucket, 'derived_R_v3','pdmp_individual_data','ZIP5','quarter',
		paste0('zip5_cleaned',yq,'_pdmp.csv.gz')))

	infile_naltrexon = file.path(bucket, 'derived_R_v3','patient_indicators', toupper(universe),'oud_naltrexon',time_unit,
		paste0(universe,'_',yq,'_oud_naltrexon.csv.gz'))
	infile_matmeth = file.path(bucket, 'derived_R_v3','patient_indicators', toupper(universe),'mat',time_unit,
		paste0(universe,'_',yq,'_mat_incmeth.csv.gz'))
	naltrexon = s3read_gz(infile_naltrexon)
	matmeth = s3read_gz(infile_matmeth) 

	# adjust files
	data[medicare==0, lowinc := 9]
	data[, medicare_type := medicare]
	data[lowinc==1, medicare_type := 2]

	data[,year := as.integer(substr(yq,1,4))]
	data[,quarter := as.integer(substr(yq,6,6))]

	data = merge(x=data, y=naltrexon,by='PATID',all.x=TRUE)	
	data[, naltrexon := ifelse(N_naltrexone>0, 1L, 0L)]
	data[is.na(naltrexon), naltrexon := 0]

	data = merge(x=data, y=matmeth,by='PATID',all.x=TRUE)
	data[is.na(N_MAT_meth_inc), N_MAT_meth_inc := 0]

	data[is.na(sum_MAT), sum_MAT := 0]
	return(data)
}

list_data = lapply(list_yq, read_quarterly_data)	
list_data = rbindlist(list_data)

list_var = c('PATID','state_fips','state','age','female','lowinc','medicare','year','quarter',
	'with_cancer','with_palliative',
	'sum_prescription','sum_MAT','sum_opioid','naltrexon','N_MAT_meth_inc',
	'max_daily_mme','sum_overlapping',
	'with_overdosed','with_IO','with_ORD','nplusplus')

list_data = list_data[, ..list_var]

s3write_fst(list_data, file.path(bucket,'derived_R_v3','pdmp','pdmp_individual_quarter.fst'))
