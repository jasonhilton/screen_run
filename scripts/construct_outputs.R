# This scripts takes the simulation batch results, and extracts the variables we
# are actually interested in analysing...
## At present these are:
# - number of migrants arrived
# - mean capital.
# - log determinant of correlation matrix of correlation in exit proportion 
# across repetitions (small sample?) 
# - log entropy of distribution over outputs. -\sum_i p_i log(p_i) where p_i is the 
# proportion of exits through city i.
# - variance in counts of people passing over links
# - variance of exit proportion over repetitions
# - variance of exit proportion within repetitions
# The third has one output per point, the others, one output per repetition.

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


logs %<>% filter(Step==max(Step))

#var_pars <- read_yaml("config/varied_pars.yaml")
var_pars <- read_csv(file.path("results", results_date, 
                               paste0("lhs_", results_date, ".csv")))


out_df <- logs %>% select(Point, Repetition, names(var_pars), 
                          n_arrived, mean_freq_plan,var_n_link) %>% 
  mutate_all(as.numeric)


## links -----------------------------------------------------------------------
link_df <- readRDS(file.path(res_path, "links.rds")) %>% ungroup()

link_df %<>% group_by(Point, Repetition) %>% 
  summarise(var_links=var(count)) 

link_df %<>% ungroup() %<>% mutate_all(as.numeric)

## exits -----------------------------------------------------------------------

exit_df  <- readRDS(file.path(res_path, "exits.rds"))

# proportion through each exit within reps.
exit_df %<>% group_by(Point, Repetition) %>% 
  mutate(p_count = count/sum(count))

## this describes how flat the dist is over cities.
# on the absolute scale it is bounded by 0 and log(N), with N the number of cities
# on the log scale, it is bounded between log(log(N)) and -oo. 
# The max occurs when p_i=p_j for all i,j. 
# The min occurs when p_i= 1 for any i.
# Is this a sensible measure?

exit_ent <-  exit_df %>% group_by(Point, Repetition) %>%
  filter(p_count!=0) %>% # plogp =0 when p is 0...
  summarise(Log_ent_count = log(-sum(p_count*log(p_count + 1e-15))))

exit_ent %<>% ungroup() %>% mutate_all(as.numeric)


# comparison - the variance within rep (across)
exit_var_df <- exit_df %>% group_by(Point, Repetition) %>%
  summarise(Var_count = var(p_count)) %>% ungroup() %>% mutate_all(as.numeric)

exit_ent %<>% left_join(exit_var_df)


# determinant ------------------------------------------------------------------
## We want to also know the degree of sameness over repetitions. 
## Is the distribution in repetition 1 correlated with the distribution in rep. 2?
## The question is - what is the volume of the correlation matrix
## which should be n_reps * n_reps


# perfectly uncorrelated (unit diagonals) correlation matrix have 
# a determinant of exactly 1 - eg prod(diag(diag(50)))
# randomly sampled values will be a little bit correlated in a small sample
# by chance, and therefore will be less than 1
# e.g. mean(map_dbl(1:10000,function(x) determinant(cor(matrix(rnorm(50),10,5)))$modulus))
# (Note that determinant returns the log by default)
# The log will therefore necessarily be negative.

get_log_det <- function(D){
  X <- D %>% spread(Repetition, count) %>% 
    select(-id) %>% as.matrix()
  return(determinant(cor(X))$modulus)
}

det_df <- exit_df %>% ungroup()%>% select(id,Point,Repetition, count) %>% 
  nest(data = c(id, Repetition, count)) %>%
  mutate(log_det_exit_cor = map_dbl(data, get_log_det))

det_df %<>% mutate(Point=as.numeric(Point))

#det_df %>% ggplot(aes(Point, log_det_exit_cor)) + geom_point()

var_rep_df <- exit_df %>% group_by(Point, id) %>%
  summarise(Var_count_rep = var(p_count)) %>% 
  ungroup() %>% group_by(Point) %>% 
  summarise(Var_count_rep=mean(Var_count_rep)) %>% 
  ungroup() %>% mutate_all(as.numeric)

det_df %<>% left_join(var_rep_df)

# --- combine and save

def_ent <- left_join(det_df, exit_ent %>% ungroup() %>% 
                       mutate(Point=as.numeric(Point)) %>% 
                       group_by(Point) %>%
                       summarise(Log_ent_count=mean(Log_ent_count)) %>% ungroup())


out_df %<>% left_join(exit_ent) %>% left_join(select(det_df, -data))

out_df %<>% left_join(link_df)
out_df %<>% arrange(Point, Repetition)

saveRDS(out_df, file.path(res_path, "output_df.rds"))



