library(yaml)

cmd_arg <- commandArgs(trailingOnly = T)

if (length(cmd_arg)==0){
  ff <- tail(list.files("designs/lhs/"),1)
  cmd_arg <- c(file.path("designs","lhs",ff), 1, 1)
}

design_file <- cmd_arg[1]
run_no <- cmd_arg[2] # ie. csv line
n_reps <- cmd_arg[3] 
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

out_opts <- c("--log-file", "--city-file",
          "--link-file", "--par-file")
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
meta_pars <- read_yaml("config/meta_pars.yaml")
meta_param_names <- paste0("--", gsub("_", "-",names(meta_pars)))

meta_arg <- mapply(function(name, val) paste(name,val),
       meta_param_names, as.numeric(meta_pars))



## Stick them together and run reps in a loop ----------------------------------
Sys.setenv(JULIA_LOAD_PATH="../RRGraphs:")
for (rep_no in 1:n_reps){
  cat("Repetition ", rep_no, " of ", n_reps)
  rand_arg <- paste0("--rand-seed-sim ", sample(10000000,1))
  arg <- paste(c(meta_arg, # put this first, so varied pars overwrite non-varied
                 arg, 
                 paste0(out_arg, rep_no,".txt"),
                 rand_arg), 
               collapse=" ")
  run_cmd <- paste0("julia ../RRGraphs/run.jl ", arg)
  
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

