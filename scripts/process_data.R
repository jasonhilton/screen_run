# This script will take a while to run as it has to load all the individual
# sim results in 

library(readr)
library(tibble)
library(dplyr)
library(purrr)
library(ggplot2)
library(tidyr)
library(magrittr)
library(stringi)
library(yaml)

source("R/ingest_data_functions.R")

cmd_args <- commandArgs(trailingOnly = T)

results_date <- cmd_args[1]

res_path <- file.path("results", results_date)

## params ----------------------------------------------------------------------

# only need one per point (although: seeds?)

files <- list.files(res_path, pattern = "par_[0-9]{1+}_1.txt")

par_df <- map_df(files, load_params, res_path=res_path)

saveRDS(par_Df, file.path( "results", "summary", results_date, "par.rds"))

## logs ------------------------------------------------------------------------
files <- list.files(res_path, pattern = "log*")

logs_df <- map_df(files, load_file, res_path=res_path)

logs_df %<>% group_by(Point, Repetition) %>% mutate(Step=1:n())
logs_df %<>% rename(mean_cap = `# mean_cap`)

logs_df %<>% left_join(par_df)

dir.create(file.path("results", "summary", results_date), recursive=T)
saveRDS(logs_df, file.path("results", "summary",results_date, "log.rds"))


## cities ----------------------------------------------------------------------

files <- list.files(res_path, pattern = "cities*")

city_df <- map_df(files, parse_file, res_path=res_path)

city_df %<>% filter(type=="EXIT") %>% rename(id=`# id`) %>%
  select(-x, -qual,-N)

city_df %<>% left_join(par_df)

saveRDS(city_df, file.path("results", "summary",results_date, "exits.rds"))


