library(yaml)

cmd_arg <- commandArgs(trailingOnly = T)

if (length(cmd_arg)==0){
  cmd_arg <- c("design.csv", 1)
}

design_file <- cmd_arg[1]
run_no <- cmd_arg[2] # ie. csv line

design_space <- read.csv(design_file)


point <- design_space[run_no,]


param_names <- paste0("--", gsub("_", "-",names(point)))

param_args <- mapply(function(name, val) paste(name,val), 
                     param_names, as.numeric(point))

arg <- paste(param_args, collapse = " ")
# should I save these in a output folder that actually makes sense
output_opts <- c("--log-file log_", "--city-file cities_",
                 "--link-file link_", "--par-file par_", "--out-file out_")

meta_pars <- read_yaml("meta_pars.yaml")
meta_param_names <- paste0("--", gsub("_", "-",names(meta_pars)))

meta_arg <- mapply(function(name, val) paste(name,val),
       meta_param_names, as.numeric(meta_pars))


rand_arg <- paste0("--rand-seed-sim ", sample(10000,1))

arg <- paste(c(arg, paste0(output_opts, run_no, ".txt"),
               rand_arg, meta_arg), collapse=" ")


Sys.setenv(JULIA_LOAD_PATH="../RRGraphs:")
run_cmd <- paste0("julia ../RRGraphs/run.jl ", arg)

system(run_cmd)

