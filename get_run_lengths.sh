ls *.pbs.o* | xargs grep -P -o '(?<=kb,walltime=)[0-9]{2}:[0-9]{2}:[0-9]{2}'
