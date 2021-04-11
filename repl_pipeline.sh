echo "----- (5) REPLICATION RUN -----"
Rscript 1_data_process/0_process_cps_measure_state_controls.R
output: cps_state_control_quarter.csv

Rscript 1_data_process/1_claims_data_aggregate_to_state.R # script below cannot be run because it needs access to pdmp_individual.fst file
output: claims_state_zip5_quarter.csv

Rscript 1_data_process/2_combine_policy_did_data.R
output: did_policy_quarter.csv

Rscript 1_data_process/3_clean_produce_regtable.R
output: did_quarter_addmeth.csv (has all the merged variables)
        note, our file does not have t40.9 (deaths from unknown illicit drugs?) deaths from CDC, not found in CDC_MCD_state_quarter_opioid_mortality_1999_2018_suppressed.csv
