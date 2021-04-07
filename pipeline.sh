#!/bin/bash

# SHELL SCRIPT TO CARRY OUT REPLICATION AND EXPLORATION OF LEE 2021 PAPER

echo "----- (1) CREATE PACKRAT -----"
#sudo Rscript run_packrat.R  # note running with sudo in order to write to /usr/R

echo "----- (2) INSTALL PACKAGES -----"
#sudo Rscript run_packages.R

echo "----- (3) DATA EXPLORE -----"

#Rscript explore_data.R
# output: (8 figures) gg_check1, gg_check2, gg_qq_deaths, gg_dpc, gg_growth_pct, gg_pct_policy, gg_comp_policy, gg_diff_policy

#Rscript explore_matching.R
# output: (8 figures) gg_top_weight, gg_csum_weights, gg_comp_np, gg_erik_fig1, gg_pct_lag, gg_pct_dist, gg_att_np, gg_auc_ps

echo "----- (4) FULL PIPELINE -----"

for i in {1..250..1}; do 
     echo "--------------- Seed: "$i" ----------------"
     Rscript run_permutation.R $i
done
# output: ../output/permute/coef_pmatch_{i}.csv & /tab_{i}.csv

Rscript analyze_permutation.R

echo "----- (5) REPLICATION RUN -----"
#Rscript 1_data_process/0_process_cps_measure_state_controls.R
# output: cps_state_control_quarter.csv

# Rscript 1_data_process/1_claims_data_aggregate_to_state.R # script below cannot be run because it needs access to pdmp_individual.fst file
# output: claims_state_zip5_quarter.csv

#Rscript 1_data_process/2_combine_policy_did_data.R
# output: did_policy_quarter.csv

#Rscript 1_data_process/3_clean_produce_regtable.R
# output: did_quarter_addmeth.csv (has all the merged variables)
#         note, our file does not have t40.9 (deaths from unknown illicit drugs?) deaths from CDC, not found in CDC_MCD_state_quarter_opioid_mortality_1999_2018_suppressed.csv
