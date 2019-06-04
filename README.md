# screen_run


This repo has supporting R code to attempt to 'screen' the parameters of the RRgraph simulation model.
This involves identifying the set of parameters that influence particular outputs.

Two methods are used: 'morris' screening and automatic relevance detection.

The send_jobs.sh and send_jobs_morris.sh scripts generate experimental designs and trigger batch jobs via the pbs script `run_model.pbs`
It is assumed that the simulation run script sits in a folder named RRGraphs in the directory above this one.

In the scripts folder are R scripts for processing simulation output files and generating the necessary variables for analysis.

