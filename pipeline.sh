#!/bin/bash

# SHELL SCRIPT TO CARRY OUT REPLICATION AND EXPLORATION OF LEE 2021 PAPER

echo "----- (1) CREATE PACKRAT -----"
sudo Rscript run_packrat.R  # note running with sudo in order to write to /usr/R

echo "----- (2) INSTALL PACKAGES -----"
sudo Rscript run_packages.R

echo "----- (3) RUN PROCESSING -----"
Rscript 1_data_process/0_process_cps_measure_state_controls.R
# Rscript 1_data_process/1_claims_data_aggregate_to_state.R
