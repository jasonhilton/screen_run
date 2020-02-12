#! /bin/bash

module load R/3.5.1

Rscript scripts/gen_lhs_design.R
csv_file=$(ls designs/lhs | tail -n 1)
runs=$(cat designs/lhs/$csv_file | wc --lines)
runs=$((runs-1)) # one line is the headers!
arrayid=$(qsub run_model.pbs -v DESIGN_FILE=designs/lhs/$csv_file,N_REPS=12 -t 1-$runs)

