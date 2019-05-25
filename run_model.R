library(yaml)

cmd_arg <- commandArgs(trailingOnly = T)

if (length(cmd_arg)==0){
  ff <- tail(list.files("designs/lhs/"),1)
  cmd_arg <- c(file.path("designs","lhs",ff), 1, 1)
}

design_file <- cmd_arg[1]
run_no <- cmd_arg[2] # ie. csv line
rep <- cmd_arg[3] # ie. csv line
if (length(rep)==0){
  rep <- 0
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
          "--link-file", "--par-file", "--out-file")
out_prefixes <- c("log", "cities",
              "link", "par", "out")

out_arg <- mapply(paste, 
                  out_opts,
                  paste(out_dir, out_prefixes, sep="/"))
out_arg <- paste0(out_arg, 
                  "_",
                  run_no,
                  "_",
                  rep,
                  ".txt")

## Control meta parameters -----------------------------------------------------
meta_pars <- read_yaml("meta_pars.yaml")
meta_param_names <- paste0("--", gsub("_", "-",names(meta_pars)))

meta_arg <- mapply(function(name, val) paste(name,val),
       meta_param_names, as.numeric(meta_pars))

rand_arg <- paste0("--rand-seed-sim ", sample(10000,1))

## Stick them together ---------------------------------------------------------

arg <- paste(c(arg, out_arg,
               rand_arg, meta_arg), collapse=" ")


Sys.setenv(JULIA_LOAD_PATH="../RRGraphs:")
run_cmd <- paste0("julia ../RRGraphs/run.jl ", arg)

system(run_cmd)





