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
print(paste0("csv_file = ", csv_file))
lhs <- read_csv(file.path(res_path, csv_file))

par_df <- lhs %>% mutate(Point=1:n())

## logs ------------------------------------------------------------------------
files <- list.files(res_path, pattern = "log*")

logs_df <- imap_dfr(files, 
                    function(f,i, res_path) {
                      print(i)
                      quietly(load_file)(f,res_path)$result
                    },
                    res_path=res_path)

logs_df %<>% group_by(Point, Repetition) %>% mutate(Step=1:n())
#logs_df %<>% rename(mean_cap = `# mean_cap`)
logs_df %<>% mutate(Point=as.numeric(Point)) %>% left_join(par_df)

dir.create(file.path("results", "summary",results_date))
saveRDS(logs_df, file.path("results", "summary",results_date, "log.rds"))


## cities ----------------------------------------------------------------------

files <- list.files(res_path, pattern = "cities*")

city_df <- imap_dfr(files,
                  function(f, i, res_path){
                    print(i)
                    quietly(load_file)(f, res_path)$result
                  }, 
                  res_path=res_path)

city_df %<>% filter(type=="EXIT")%>%
  select(-x, -qual,-N)

city_df %<>% mutate(Point=as.numeric(Point)) %>% left_join(par_df)

saveRDS(city_df, file.path("results", "summary",results_date, "exits.rds"))


## links ----------------------------------------------------------------------

files <- list.files(res_path, pattern = "link*")


link_df <- imap_dfr(files,
                  function(f, i, res_path) {
                    print(i)
                    quietly(load_file)(f, res_path)$result
                  }, 
                  res_path=res_path)

saveRDS(link_df, file.path("results", "summary",results_date, "links.rds"))
