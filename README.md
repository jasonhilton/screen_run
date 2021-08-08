# screen_run


This repo has supporting R code for the analysis of the Routes and Rumours simulation model.
This involves identifying the set of parameters that influence particular outputs.

The `send_jobs.sh` script generates experimental designs and trigger batch jobs via the pbs script `run_model.pbs`
The `send_jobs.sh` script takes a single command line argument that indicates which model version is to be used (M3 or M4).

The simulation results will be saved in a time-stamped folder in the results directory.

In the `scripts` folder are R scripts for processing simulation output files and generating the necessary variables for analysis.

Each script takes only one command line argument - the timestamped folder where the simulation results we wish to analyse are saved (e.g. `20210806_222446` or similar).

1. The `process_data.R` script aggregates and simplifies simulation output files. Results are saved in a timestamped directory within the `results/summary` folder.
2. The `construct_output.R` script does further manipulation to produce the small set of summary outputs needed for further analysis. Again results are saved in the `results/summary` folder.
3. The `sensitivity.r` script conducts a sensitivity analysis to indicate the contribution of each input to the total simulation variance. Outputs are saved as PDF plots in the `SA_results` folder.



