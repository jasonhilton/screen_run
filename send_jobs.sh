#! /bin/bash

module load R/3.5.1


runs=$(cat $1 | wc --lines)
arrayid=$(qsub run_model.pbs -v DESIGN_FILE=$1 -t 1-$runs)

# qsub -W depend=afterokarray:$arrayid PBS/run_loos.pbs
