#! /bin/bash

module load R/3.5.1

Rscript gen_morris_design.R
csv_file=$(ls designs/morris | tail -n 1)
runs=$(cat $csv_file | wc --lines)
runs=$((runs-1)) # one line is the headers!
arrayid=$(qsub run_model.pbs -v DESIGN_FILE=$csv_file,REP=1 -t 1-$runs)

# strategy - after the first one, do a loop for the repetitions



for id in {2..5}; do 
  newarray=$(qsub -W depend=afteranyarray:$arrayid run_model.pbs DESIGN_FILE=$csv_file REP=$id)
  arrayid=$newarray
done