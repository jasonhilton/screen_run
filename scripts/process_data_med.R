# This script will take a while to run as it has to load all the individual
# sim results in 

library(readr)
library(tibble)
library(dplyr)
library(purrr)
library(tidyr)
library(magrittr)
library(stringi)
library(yaml)

source("R/ingest_data_functions.R")

cmd_args <- commandArgs(trailingOnly = T)


if (length(cmd_args)==0){
  results_date <- tail(list.files("results", "^[20]"),1)
} else {
  results_date <- cmd_args[1]  
}

res_path <- file.path("results", results_date)

## params ----------------------------------------------------------------------

# only need one per point (although: seeds?)

csv_file <- list.files(res_path, pattern = "*.csv")

lhs <- read_csv(file.path(res_path, csv_file))

# fragile (relying on point number)
lhs %<>% select(-Float64)

par_df <- lhs %>% mutate(Point=1:n())


## logs ------------------------------------------------------------------------
files <- list.files(res_path, pattern = "log*")

logs_df <- map_df(files,
                  function(f, res_path) {
                    quietly(load_file)(f,res_path)$result
                  },
                  res_path=res_path)

logs_df %<>% group_by(Point, Repetition) %>% mutate(Step=1:n())
logs_df %<>% mutate(Point=as.numeric(Point),
                    Repetition=as.numeric(Repetition))

logs_df %<>% left_join(par_df)

dir.create(file.path("results", "summary",results_date),
           showWarnings = F)

saveRDS(logs_df, file.path("results", "summary",results_date, "log.rds"))


## interceptions ----------------------------------------------------------------------

files <- list.files(res_path, pattern="interceptions")

ints_df <- map_df(files,
                  function(f, res_path) {
                    quietly(load_file)(f,res_path)$result
                  },
                  res_path=res_path)

ints_df %<>% select(-X2)

ints_df %<>% group_by(Point, Repetition) %>% mutate(Step=1:n())
ints_df %<>% mutate(Point=as.numeric(Point),
                    Repetition=as.numeric(Repetition))

ints_df %<>% left_join(par_df)


saveRDS(ints_df, file.path("results", "summary",results_date, "ints.rds"))
