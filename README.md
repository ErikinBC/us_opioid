Date : 01/24/2021
Written by Byungkyu Lee (bl11@indiana.edu)

# README File for replication of all figures and tables reported in 
Lee B, Zhao W, Yang K-C, Ahn Y-Y, Perry BL. Systematic evaluation of state policy interventions targeting the us opioid epidemic, 2007-2018. JAMA Netw Open. 2021;4(2):e2036687. doi:10.1001/jamanetworkopen.2020.36687

This file contains Stata Do files and R codes to reproduce all figures and tables appeared in the main text as well as Appendix. 

## Directory Structure
We used `here` to define the root directory of this replication folder. If you change the folder, please specify `here` in each code.

## Data
While we are not able to include the raw claims data from Optum as well as state-level CDC mortality data due to the privacy concerns and data restrictions, we include the following data sets that can be used to replicate our paper.

1) Claims data : We processed individual-level claims quarterly data and combined them using `code/1_data_process/_create_pdmp_individual_data_from_claims_data.R` : `pdmp_individual_quarter.fst`

2) Mortality data : We was able to collect the unsupressed state-level mortality data from CDC Wonder online database (https://wonder.cdc.gov/mcd.html), but we were not able to share the raw data due to the data use agreement.  Instead we include suppressed state-level mortality data from CDC wonder (`CDC_MCD_state_quarter_opioid_mortality_1999_2018_supressed.csv`). 

3) Current Population Survey data: we downloaded the current population survey data from IPUMS website (https://cps.ipums.org/cps/), and use `code/1_data_process/_read_cps_data.R` to read the data and convert it to fst file format (`cps_2005_2019.fst`).

4) policy-data : we include csv files for policy data that we described in eTable 2 and 3.

## Code 
To replicate results, you should run all the codes in the `1_data_process` folder to generate data files in `data/reg-data` folder. Once you have all these files, you can run the following code to replicate our results.

* Figure 1 : `3_figure/figure_1_policy_change.R`
* Figure 2 and 3: first run `2_model/1_model_panel_matching.R` , and then `2_model/2_meta_analysis.R`. Once you obtain `coef_pmatch.csv` and `pmatch_meta_analysis.csv` files, you can use `3_figure/figure_2-3_main_effects_pmatch.R` to generate Figure 2 and 3.

* Figure S1 : first run `2_model/1_model_panel_matching.R` as well as `2_model/3_model_event_study_design.R`. Once you obtain `coef_pmatch.csv` and `coef_event_study.csv`, you can use `3_figure/figure_s1_illustration_methods.R` to generate Figure S1.
* Figure S2 and S3 : first run `2_model/1_model_panel_matching.R`. Once you obtain `coef_pmatch.csv`, you can use `3_figure/figure_s2-s3_appendix_all.R` to generate Figure S2 and S3.
* Figure S4 : `3_figure/figure_s4_descriptive_outcomes.R` 
* Figure S5 and S6 : first run `2_model/1_model_panel_matching.R` , and then `2_model/2_meta_analysis.R`. Once you obtain `coef_pmatch.csv` and `pmatch_meta_analysis.csv` files, you can use `3_figure/figure_s5-s6_main_effects_pmatch.R` to generate Figure S5 and S6.

* Table S1, S3, and S4 : `3_table/table_s1-s3-s4_descriptive.R`
* Table S5 : `3_table/table_s5_suppressed_mortality.R` : we are not able to include the unsuppressed data files.
* Table S8, S9 : first run `2_model/1_model_panel_matching.R` , and then `2_model/2_meta_analysis.R`. Once you obtain `coef_pmatch.csv` and `pmatch_meta_analysis.csv` files, you can use `3_table/table_s8-s9-meta_analysis_table.R` to generate Table S8 and S9.
* Table S10 : first run `2_model/1_model_panel_matching.R`. Once you obtain `coef_pmatch.csv` file, you can use `3_table/table_s10_main_effects.R` to generate Table S10.

##  NOTE for exact replications
We include `coef_pmatch.csv` and `pmatch_meta_analysis.csv` that you need to exactly replicate our main results that we reported in our paper. Due to the nature of panel matching that uses bootstrapping to generate estimates and confidence intervals, results from your analysis might slightly differ from the paper. We rerun our codes many times and found that our results are very similar across different runs. 

## Session Info
```
R version 4.0.3 (2020-10-10)
Platform: x86_64-apple-darwin17.0 (64-bit)
Running under: macOS Catalina 10.15.7

Matrix products: default
BLAS:   /Library/Frameworks/R.framework/Versions/4.0/Resources/lib/libRblas.dylib
LAPACK: /Library/Frameworks/R.framework/Versions/4.0/Resources/lib/libRlapack.dylib

locale:
[1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8

attached base packages:
[1] grid      parallel  stats     graphics  grDevices utils     datasets 
[8] methods   base     

other attached packages:
 [1] huxtable_5.1.1    ggridges_0.5.3    docshop_0.1.0     withr_2.3.0      
 [5] aws.s3_0.3.21     bit64_4.0.5       bit_4.0.4         usethis_2.0.0    
 [9] here_1.0.1        colorspace_2.0-0  viridis_0.5.1     viridisLite_0.3.0
[13] ggalluvial_0.12.3 ggrepel_0.9.0     gridExtra_2.3     mgcv_1.8-33      
[17] nlme_3.1-151      skimr_2.1.2       forcats_0.5.0     stringr_1.4.0    
[21] dplyr_1.0.2       readr_1.4.0       tidyr_1.1.2       tibble_3.0.4     
[25] tidyverse_1.3.0   lfe_2.8-6         rio_0.5.16        hrbrthemes_0.8.0 
[29] metafor_2.4-0     ggsci_2.9         ggpubr_0.4.0      gghighlight_0.3.1
[33] ggplot2_3.3.3     purrr_0.3.4       survey_4.0        survival_3.2-7   
[37] Matrix_1.3-2      PanelMatch_1.0.0  wfe_1.9.1         zoo_1.8-8        
[41] fst_0.9.4         data.table_1.13.6 ipumsr_0.4.5     

loaded via a namespace (and not attached):
 [1] readxl_1.3.1        backports_1.2.1     Hmisc_4.4-2        
 [4] systemfonts_0.3.2   plyr_1.8.6          repr_1.1.0         
 [7] splines_4.0.3       digest_0.6.27       foreach_1.5.1      
[10] htmltools_0.5.1     fansi_0.4.1         magrittr_2.0.1     
[13] checkmate_2.0.0     cluster_2.1.0       aws.signature_0.6.0
[16] openxlsx_4.2.3      modelr_0.1.8        extrafont_0.17     
[19] sandwich_3.0-0      extrafontdb_1.0     jpeg_0.1-8.1       
[22] rvest_0.3.6         mitools_2.4         haven_2.3.1        
[25] xfun_0.20           crayon_1.3.4        jsonlite_1.7.2     
[28] lme4_1.1-26         zeallot_0.1.0       iterators_1.0.13   
[31] glue_1.4.2          gtable_0.3.0        car_3.0-10         
[34] Rttf2pt1_1.3.8      shape_1.4.5         abind_1.4-5        
[37] scales_1.1.1        DBI_1.1.0           rstatix_0.6.0      
[40] Rcpp_1.0.5          xtable_1.8-4        htmlTable_2.1.0    
[43] foreign_0.8-81      Formula_1.2-4       glmnet_4.0-2       
[46] htmlwidgets_1.5.3   httr_1.4.2          MatchIt_4.1.0      
[49] RColorBrewer_1.1-2  ellipsis_0.3.1      pkgconfig_2.0.3    
[52] nnet_7.3-14         dbplyr_2.0.0        tidyselect_1.1.0   
[55] rlang_0.4.10        munsell_0.5.0       cellranger_1.1.0   
[58] tools_4.0.3         cli_2.2.0           generics_0.1.0     
[61] broom_0.7.3         evaluate_0.14       arm_1.11-2         
[64] knitr_1.30          fs_1.5.0            zip_2.1.1          
[67] xml2_1.3.2          compiler_4.0.3      rstudioapi_0.13    
[70] curl_4.3            png_0.1-7           ggsignif_0.6.0     
[73] CBPS_0.21           reprex_0.3.0        statmod_1.4.35     
[76] stringi_1.5.3       ps_1.5.0            gdtools_0.2.3      
[79] lattice_0.20-41     nloptr_1.2.2.2      vctrs_0.3.6        
[82] pillar_1.4.7        lifecycle_0.2.0     R6_2.5.0           
[85] latticeExtra_0.6-29 codetools_0.2-18    boot_1.3-25        
[88] MASS_7.3-53         assertthat_0.2.1    rprojroot_2.0.2    
[91] hms_0.5.3           rpart_4.1-15        coda_0.19-4        
[94] minqa_1.2.4         rmarkdown_2.6       carData_3.0-4      
[97] numDeriv_2016.8-1.1 lubridate_1.7.9.2   base64enc_0.1-3    
```

If you have any question on these files or fail to reproduce any of figures or tables, then please direct your correspondence to BK Lee (bklee009@gmail.com). 

