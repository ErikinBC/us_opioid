#!/bin/bash

# SHELL SCRIPT TO CARRY OUT REPLICATION AND EXPLORATION OF LEE 2021 PAPER

echo "----- (1) CREATE PACKRAT -----"
sudo Rscript run_packrat.R  # note running with sudo in order to write to /usr/R

echo "----- (2) INSTALL PACKAGES -----"
sudo Rscript run_packages.R

echo "----- (3) DATA EXPLORE -----"

Rscript explore_data.R
# output: (8 figures) gg_check1, gg_check2, gg_qq_deaths, gg_dpc, gg_growth_pct, gg_pct_policy, gg_comp_policy, gg_diff_policy

Rscript explore_matching.R
# output: (8 figures) gg_top_weight, gg_csum_weights, gg_comp_np, gg_erik_fig1, gg_pct_lag, gg_pct_dist, gg_att_np, gg_auc_ps

echo "----- (4) PERMUTATION -----"

for i in {1..250..1}; do 
     echo "--------------- Seed: "$i" ----------------"
     Rscript run_permutation.R $i
done
# output: ../output/permute/coef_pmatch_{i}.csv & /tab_{i}.csv

# Analyze the permutation results
Rscript analyze_permutation.R

