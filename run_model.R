library(yaml)
library(glue)

cmd_arg <- commandArgs(trailingOnly = T)

if (length(cmd_arg)==0){
  ff <- tail(list.files("designs/lhs/"),1)
  cmd_arg <- c(file.path("designs","lhs",ff), 1, 1)
}

design_file <- cmd_arg[1]
run_no <- cmd_arg[2] # ie. csv line
n_reps <- cmd_arg[3] 


design_space <- read.csv(design_file)
time_stamp <- stringi::stri_extract(design_file, regex="[0-9]{8}_[0-9]{6}")

point <- design_space[run_no,]

# JULIA_LOAD_PATH=$JULIA_LOAD_PATH:../rgct_data 
# julia ../rgct_data/run.jl 
# ../params.jl 
# -m ../map_med1.json 
# -s ../mediterranean '--wait 10 --warmup 100 --int 0.01 0.077 0.153 0.428 0.389 
#                      --risks 0.01 0.023 0.020 0.029 0.048'  
# -t 500



## Parameter args --------------------------------------------------------------

par_file_path <- file.path("../screen_run/param_files", paste0("param_", run_no, ".jl"))

arg <- par_file_path

## Maps ------------------------------------------------------------------------

map_arg <- "-m med/map_med1.json"

arg <- paste(arg, map_arg)

## Output args -----------------------------------------------------------------
out_dir <- file.path("../screen_run", "results", time_stamp)

dir.create(out_dir,
           recursive = T,
           showWarnings = F)

out_opts <- c("--log-file", "--city-out-file",
          "--link-out-file", "--par-out-file")
out_prefixes <- c("log", "cities",
              "link", "par")

out_arg <- mapply(paste, 
                  out_opts,
                  paste(out_dir, out_prefixes, sep="/"))
out_arg <- paste0(out_arg, 
                  "_",
                  run_no,
                  "_")


## Parameter args --------------------------------------------------------------
scen_out <- paste0("--out ", out_dir,  
                   "/interceptions_", run_no, 
                   "_{rep_no}", ".txt'")
scen <- paste0("-s ../screen_run/med/mediterranean ",
               "'--wait 10 --warmup 100 --int 0.01 0.077 0.153 0.428 0.389 ",
               "--risks 0.01 0.023 0.020 0.029 0.048 ",
               scen_out)  

arg = paste(arg, scen)



## Control meta parameters -----------------------------------------------------
# meta_pars <- read_yaml("config/meta_pars.yaml")
# meta_param_names <- paste0("--", gsub("_", "-",names(meta_pars)))
# 
# meta_arg <- mapply(function(name, val) paste(name,val),
#        meta_param_names, as.numeric(meta_pars))



## Stick them together and run reps in a loop ----------------------------------
Sys.setenv(JULIA_LOAD_PATH="../rgct_data:")
for (rep_no in 1:n_reps){
  cat("Repetition ", rep_no, " of ", n_reps)
  #rand_arg <- paste0("--rand-seed-sim ", sample(10000,1))
  temp_arg <- arg
  #arg <- paste(c(meta_arg, # put this first, so varied pars overwrite non-varied
  temp_arg <- glue(temp_arg, rep_no=rep_no)
  temp_arg <- paste(temp_arg,
               paste0(out_arg, rep_no,".txt", collapse=" "),
               "-t 500",
               collapse=" ")
  run_cmd <- paste0("julia ../rgct_data/run.jl ", temp_arg)
  print(run_cmd)
  #system(run_cmd)
}


if (file.exists("backup_copy.sh")){
  command <- paste0("backup_copy.sh ", out_dir)
  system(command)
}


if (file.exists("backup_copy_file.sh")){
  command <- paste0("backup_copy_file.sh ",
                    design_file,
                    " ",
                    out_dir)
  system(command)
}

