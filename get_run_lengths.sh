ls *.pbs.o* | xargs grep -P -o '(?<=kb,walltime=)[0-1][0-9]:[0-9]{2}:[0-9]{2}'
