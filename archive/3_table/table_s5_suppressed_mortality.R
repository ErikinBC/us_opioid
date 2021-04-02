# ----------------------------------------------------------------------
# ' table s3. check suppressed valued
# ----------------------------------------------------------------------
library(data.table)

here = getwd()

fips_codes = fread(file.path(here, 'data', 'tigris_fips_codes.csv'))

time_unit = 'quarter'

mortality = fread(file.path(here,'data','cdc-data',paste0('CDC_MCD_state_',time_unit,'_opioid_mortality_1999_2018.csv')))

tab1 = mortality[,.(death_all=sum(death_count)),by='icd_code']
tab2 = mortality[death_count >= 10,.(death_suppressed=sum(death_count)),by='icd_code']
tab = merge(tab1, tab2, by='icd_code')
tab[,diff := death_all - death_suppressed]
tab[,suppressing_rate := diff/death_all * 100]

fwrite(tab, file.path(here,'results','table_s5_suppressed_mortality.csv'))

mortality = fread(file.path(here,'data','cdc-data',paste0('CDC_MCD_state_',time_unit,'_opioid_mortality_1999_2018.csv')))
mortality[death_count < 10, death_count := 0]
mortality[death_count < 10, crude_rate_measure := 0]
fwrite(mortality, file.path(here,'data','cdc-data',paste0('CDC_MCD_state_',time_unit,'_opioid_mortality_1999_2018_suppressed.csv')))

# delete the unsuppressed file
file.remove(file.path(here,'data','cdc-data',paste0('CDC_MCD_state_',time_unit,'_opioid_mortality_1999_2018.csv')))
