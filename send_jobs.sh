#! /bin/bash

module load conda
source activate Rstan

Rscript scripts/gen_lhs_design.R config/varied_pars_$1.yaml # config varied_pars file (M3 or M4)
csv_file=$(ls designs/lhs | tail -n 1)
runs=$(cat designs/lhs/$csv_file | wc --lines)
runs=$((runs-1)) # one line is the headers!
arrayid=$(qsub run_model.pbs -v MOD_VERSION=$1,DESIGN_FILE=designs/lhs/$csv_file,N_REPS=12 -t 1-$runs)

