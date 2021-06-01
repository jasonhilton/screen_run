library(readr)
library(dplyr)
library(tidyr)
library(tibble)
library(purrr)
library(magrittr)
library(doParallel)
library(foreach)
library(rstan)
library(DiceKriging)
library(GGally)

source("r/calib_functions.R")

cmd_args <- commandArgs(trailingOnly = T)

if(length(cmd_args)==0){
  results_date <- tail(list.files(file.path("results", "summary"),
                                  pattern="^20"),1)
} else {
  results_date <- cmd_args[1]
}

res_path <- file.path("results", "summary", results_date) 

out_df <- readRDS(file.path(res_path, "out_df.rds"))

out_df %<>% arrange(Point, Repetition)



D_df <- out_df %>% select(-Variable, -Value) %>% 
  group_by(Point) %>%
  summarise_all(first)

outputs_df <- out_df %>% 
  select(Point, Repetition, Step, Variable, Value) %>% 
  pivot_wider(names_from =c(Variable, Step), values_from = Value)

out_names <- outputs_df %>% ungroup() %>%
  select(-Point, - Repetition) %>% colnames



outs <- outputs_df %>% 
  group_by(Point) %>% 
  summarise_at(out_names, list(mean=mean,var=var))

yy <- outputs_df[out_names]


out_ms <- apply(yy, 2, mean)
out_vs <- apply(yy, 2, var)
yy %<>% apply(2, function(x)(x-mean(x))/sd(x))
#yy_dash <- yy  %>% apply(2, function(x)(x-mean(x))/sd(x))



source("R/calib_functions.R")
n_pc <- 4
pcs <- extract_principal_components(t(yy),n_pc)

standard_directions <- pcs$standard_directions
scores <- pcs$scores

message(paste0("Cumulative proportion of variance for first ", n_pc,
               " principal components: \n"), 
        paste0(pcs$prop_variance, sep="\n"))

stopifnot(all(apply(scores, 2, var) -1 <1e-06))
stopifnot(all(apply(scores, 2, mean)<1e-06))

D <- D_df %>% select(-Point, -Repetition, - Step)

D <- apply(D,2,function(x) (x-min(x)) /(max(x)-min(x)) )



scores_df <- as_tibble(scores,.name_repair = "unique")

scores_df <-cbind(Point=outputs_df$Point, scores_df)

input_means <- scores_df %>% group_by(Point) %>% summarise_all(mean) %>% 
  arrange(Point)
#input_vars <- scores_df %>% group_by(Point) %>% summarise_all(~0.05) %>% 
input_vars <- scores_df %>% group_by(Point) %>% summarise_all(var) %>% 
  arrange(Point)

# cv1 <- do_kfold(D,input_means$scores_1, input_vars$scores_1, n_folds = 13)
# cv2 <- do_kfold(D,input_means$scores_2, input_vars$scores_2, n_folds = 13)
# cv3 <- do_kfold(D,input_means$scores_3, input_vars$scores_3, n_folds = 13)
# cv4 <- do_kfold(D,input_means$scores_4, input_vars$scores_4, n_folds = 13)


library(doParallel)
library(foreach)

registerDoParallel()


mods <- map2((input_means %>% select(-Point)),#[-leave_out,],
             (input_vars %>% select(-Point)),#[-leave_out,],
             function(m, v){
               km(design=D,#[-leave_out,],
                  response=m, 
                  noise.var=v,
                  multistart = 10) # defaults to matern5_2
             }
)
# Empirical ------------------------------------------------------------

# library(lubridate)
# real_df <- read_csv("observations/arrivals_interceptions.csv",
#                     col_types = cols(col_date(format="%b-%y"),
#                                      col_number(),
#                                      col_number(),
#                                      col_number(),
#                                      col_number()))
# 
# 
# real_df[is.na(real_df)] <- 0 
# 
# real_df %<>% 
#   mutate(Arrivals = `Sea arrivals in Italy` + `Sea arrivals in Malta`,
#          Interceptions = `Interceptions by Libyan Coast Guard` + 
#            `Interceptions by Tunisian Coast Guard`)
# 
# real_df %<>% 
#   select(Date, Arrivals, Interceptions) %>% 
#   mutate(Year=year(Date)) %>% group_by(Year) %>%
#   summarise(across(where(is.numeric), sum))
# 
# target <- 
#   real_df %>% summarise(
#     Year = Year[-1],
#     log_diff_arrived= diff(log(Arrivals)),
#     log_diff_ints= diff(log(Interceptions))
#   )
# 
# target %<>% pivot_longer(cols=c(log_diff_arrived, log_diff_ints),
#                          names_to="Variable", values_to = "Value")
# 
# obs <- target$Value


obs=c(-0.4186751,
      0.3556379, 
      -1.5707796,
      -0.1210168, 
      -0.5116991,
      -0.6438180)

stan_data <- make_stan_data(obs=c(-0.4186751,
                                  0.3556379, 
                                  -1.5707796,
                                  -0.1210168, 
                                  -0.5116991,
                                  -0.6438180),
                            mods=mods,
                            design = D,
                            standard_directions = standard_directions,
                            sim_means =out_ms,
                            sim_vars=out_vs)

stan_data$theta_as <-  c(1,1,1,1,1,1,1)
stan_data$theta_bs <-  c(1,1,1,1,1,1,1)
stan_data$theta_uppers <-  c(1,1,1,1,1,1,1)
stan_data$theta_lowers <-  c(0,0,0,0,0,0,0)


cal_mod <- stan_model("stan/calib.stan")

cal_fit <- sampling(cal_mod, iter=10000, cores=3, chains=3, data=stan_data, 
                    thin=5)

saveRDS(list(stan_data=stan_data, cal_fit=cal_fit), "calibrated_1.rds")

