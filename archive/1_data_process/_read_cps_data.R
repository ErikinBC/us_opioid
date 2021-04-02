#------------------------------------------------------------------------------
# transform xml data (downloaded from ipums website) to fst format
#==============================================================================

library(ipumsr)
library(data.table)
library(fst)

here = getwd()

# process files (once)
cps_ddi = read_ipums_ddi(file.path(here,'data','CPS','cps_00007.xml'))
cps_data = read_ipums_micro(cps_ddi, verbose = FALSE)
write_fst(cps_data, file.path(here,'data','CPS','cps_2005_2019.fst'),100)
