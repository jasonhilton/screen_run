#! /bin/bash

module load R/3.5.1

Rscript gen_lhs_design.R
csv_file=$(ls designs/lhs | tail -n 1)
runs=$(cat designs/lhs/$csv_file | wc --lines)
runs=$((runs-1)) # one line is the headers!
arrayid=$(qsub run_model.pbs -v DESIGN_FILE=designs/lhs/$csv_file,REP=1 -t 1-$runs)

# strategy - after the first one, do a loop for the repetitions
# qsub -W depend=afteranyarray:$arrayid PBS/run_loos.pbs


for id in {2..5}; do 
  echo $id
  newarray=$(qsub -W depend=afteranyarray:$arrayid run_model.pbs -v DESIGN_FILE=designs/lhs/$csv_file,REP=$id -t 1-$runs)
  arrayid=$newarray
done
