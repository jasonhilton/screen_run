#! /bin/bash

module load R/3.5.1


runs=$(cat $csv_file | wc --lines)
arrayid=$(qsub run_model.pbs -v DESIGN_FILE=$csv_file -t 1-$runs)

# qsub -W depend=afterokarray:$arrayid PBS/run_loos.pbs