#! /bin/bash

module load R/3.5.1

Rscript gen_morris_design.R
csv_file=$(ls designs/morris | tail -n 1)
runs=$(cat designs/morris$csv_file | wc --lines)
runs=$((runs-1)) # one line is the headers!
arrayid=$(qsub run_model.pbs -v DESIGN_FILE=designs/morris$csv_file,N_REPS=5 -t 1-$runs)





