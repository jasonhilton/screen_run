#! /bin/bash

module load conda
source activate Rstan

csv_file=$(ls designs/posterior | tail -n 1)
runs=$(cat designs/posterior/$csv_file | wc --lines)
runs=$((runs-1)) # one line is the headers!
arrayid=$(qsub run_post.pbs -v DESIGN_FILE=designs/posterior/$csv_file,N_REPS=1 -t 1-$runs)

