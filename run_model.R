library(yaml)
library(stringi)

cmd_arg <- commandArgs(trailingOnly = T)

if (length(cmd_arg)==0){
  ff <- tail(list.files("designs/lhs/"),1)
  cmd_arg <- c(
    "../rgct_data", # model path
    file.path("designs","lhs",ff),  # design
    1, # design point number (run number)
    1  # rep number)
  )
}

model_path <- cmd_arg[1]
design_file <- cmd_arg[2]
run_no <- cmd_arg[3] # ie. csv line
n_reps <- cmd_arg[4] 
if (length(rep)==0){
  n_reps <- 1
}

design_space <- read.csv(design_file)
time_stamp <- stringi::stri_extract(design_file, regex="[0-9]{8}_[0-9]{6}")

point <- design_space[run_no,]

## Parameter args --------------------------------------------------------------
param_names <- paste0("--", gsub("_", "-",names(point)))

param_args <- mapply(function(name, val) paste(name,val), 
                     param_names, as.numeric(point))

arg <- paste(param_args, collapse = " ")


## Output args -----------------------------------------------------------------
out_dir <- file.path("results", time_stamp)

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
             

## Control meta parameters -----------------------------------------------------

#  extract the last dir in the model path. should be e.g. rgct or RRGraphs
path_els <- strsplit(model_path, "/")[[1]]
config_suffix <- path_els[length(path_els)]

meta_pars <- read_yaml(paste0("config/meta_pars_",config_suffix, ".yaml"))
meta_param_names <- paste0("--", gsub("_", "-",names(meta_pars)))

# add quotes so that vectors are interpreted as one arg.
meta_arg <- mapply(function(name, val) paste0(name, " '", val, "'"),
       meta_param_names, meta_pars)



## Stick them together and run reps in a loop ----------------------------------
Sys.setenv(JULIA_LOAD_PATH=paste0(model_path, ":"))
for (rep_no in 1:n_reps){
  cat("Repetition ", rep_no, " of ", n_reps)
  rand_arg <- paste0("--rand-seed-sim ", sample(10000000,1))
  arg <- paste(c(meta_arg, # put this first, so varied pars overwrite non-varied
                 arg, 
                 paste0(out_arg, rep_no,".txt"),
                 rand_arg), 
               collapse=" ")
  run_cmd <- paste0("julia ", model_path, "/run.jl ", arg)
  
  system(run_cmd)
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

