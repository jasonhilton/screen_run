# This scripts takes the simulation batch results, and extracts the variables we
# are actually interested in analysing...

library(lubridate)
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

if(length(cmd_args)==0){
  results_date <- tail(list.files(file.path("results", "summary"),
                                  pattern="^20"),1)
} else {
  results_date <- cmd_args[1]
}

res_path <- file.path("results", "summary", results_date)

logs <- readRDS(file.path(res_path, "log.rds")) %>% ungroup()

#logs %<>% filter(Step==max(Step))

var_pars <- read_yaml("config/varied_pars_med.yaml")

# arrivals and deaths -----------------------------------------------------


out_df <- logs %>% select(Point, Repetition, Step, names(var_pars), 
                          n_migrants, n_arrived, n_dead) %>% 
  mutate_all(as.numeric)


out_df %<>% group_by(Point, Repetition) %>% arrange(Step) %>%
  mutate(annual_arrived= n_arrived - lag(n_arrived, 99),
         annual_dead = n_dead - lag(n_dead, 99))


out_df %<>% filter(Step %in% c(200,300, 400, 500)) %>%
  mutate(log_diff_arrived = c(NA, diff(log(annual_arrived))),
         log_diff_dead = c(NA,diff(log(annual_dead))))

out_df %<>% filter(Step!=200)

# interceptions --------------------------------------------------------


ints_df <- readRDS(file.path(res_path, "ints.rds"))

ints_df %<>% group_by(Point, Repetition) %>% arrange(Step) %>%
  mutate(annual_interceptions= n_interc - lag(n_interc, 99))

ints_df %<>% filter(Step %in% c(200,300, 400, 500)) %>%
  mutate(log_diff_ints = c(NA, diff(log(annual_interceptions))))


ints_df %<>% filter(Step!=200)


# Join ------------

out_df <- out_df %>% ungroup() %>% left_join(ints_df %>% ungroup())


out_df %<>% select(-n_migrants, -n_arrived, -n_dead,
                  - annual_arrived,
                  - annual_dead, - n_interc, -seed,
                  - log_diff_dead,
                  - annual_interceptions)

out_df %>% pivot_longer(cols=c(log_diff_arrived, log_diff_ints),
             names_to="Variable", values_to = "Value") %>%
  saveRDS(file.path(res_path, "out_df.rds"))





sim_vals <- out_df %>% select(Point, Repetition, 
                              Step, log_diff_arrived, 
                              log_diff_ints) %>%
  mutate(Year = case_when(Step==300 ~ 2017,
                          Step==400 ~ 2018,
                          Step==500 ~ 2019)) %>% 
  select(-Step)


sim_vals %<>% pivot_longer(cols=c(log_diff_arrived, log_diff_ints),
                          names_to="Variable", values_to = "Value")

saveRDS(sim_vals, file.path(res_path, "sim_vals.rds"))


