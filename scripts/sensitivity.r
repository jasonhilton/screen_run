library(here)
library(tibble)
library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
library(purrr)
library(GGally)
library(magrittr)
library(doParallel)
library(foreach)
library(DiceKriging)
set.seed(1234)

cmd_args <- commandArgs(trailingOnly = T)

if(length(cmd_args)==0){
  results_date <- tail(list.files(file.path("results", "summary"),
                                  pattern="^20"),1)
} else {
  results_date <- cmd_args[1]
}

res_path <- file.path("results", "summary", results_date)

output_df <- readRDS(file.path(res_path, "output_df.rds")) 
output_df %<>% mutate(stdd_link_c=sqrt(var_links))

varied_pars <- read_csv(file.path("results", results_date, 
                               paste0("lhs_", results_date, ".csv"))) %>%
  names()


out_pars <- c("mean_freq_plan", "stdd_link_c","log_det_exit_cor", "Log_ent_count")

out_df <- output_df
out_df %<>% mutate_at(vars(one_of(varied_pars)), function(x) x/max(x))
out_std_df <- out_df %>% mutate_at(vars(one_of(out_pars)),
                                   function(x) (x - mean(x))/sd(x))

out_std_df %<>% group_by(Point) %>% 
  summarise_at(vars(one_of(out_pars)), 
               list(mean=mean, 
                    var=function(x) var(x)/(n()-1)))

covar_df <- left_join(out_std_df,out_df) %>%
  filter(Repetition==1) %>% select(-one_of(out_pars))

D <- covar_df %>% select(one_of(varied_pars))

yy <- out_std_df$mean_freq_plan_mean
yy_var <- out_std_df$mean_freq_plan_var

registerDoParallel()
mod1 <- km(response=yy, design=D, noise.var=yy_var, 
           covtype = "matern5_2",
           multistart = 10, 
           control=list(trace=T))#, upper=rep(5, length(varied_pars)))

library(sensitivity)

d <- mod1@d
n <- 2000
X1 <- data.frame(matrix(runif(d * n), nrow = n))
X2 <- data.frame(matrix(runif(d * n), nrow = n))
nsim <- 250
nboot <- 250

SA <- sobolGP(mod1,
              type="UK",
              MCmethod="sobol2007",
              X1,
              X2,
              nsim=nsim,
              conf = 0.95,
              nboot = nboot,
              sequential = FALSE,
              sequential.tot=FALSE,
              max_iter = 1000
)

SA_df <- tibble(Variables=colnames(mod1@X),
                Main_effect=t(SA$S$mean),
                Total_effect=t(SA$T$mean)) 
SA_df %<>% pivot_longer(cols=c("Main_effect", "Total_effect"),
                       names_to="Effect Type", 
                       values_to="Sensitivity Index")
       

ggplot(SA_df, aes(x=Variables, y=`Sensitivity Index`,fill=`Effect Type`)) + 
  geom_bar(stat="identity", position="dodge") + 
  theme_bw(base_size=18) +
  coord_flip()

dir.create(file.path("SA_results", results_date))
ggsave(file.path("SA_results", results_date, "out1.pdf"))

yy <- out_std_df$stdd_link_c_mean
yy_var <- out_std_df$stdd_link_c_var

mod2 <- km(response=yy, design=D, noise.var=yy_var, 
           covtype = "matern5_2",
           multistart = 10, 
           control=list(trace=T))#, upper=rep(5, length(varied_pars)))

SA <- sobolGP(mod2,
              type="UK",
              MCmethod="sobol2007",
              X1,
              X2,
              nsim=nsim,
              conf = 0.95,
              nboot = nboot,
              sequential = FALSE,
              sequential.tot=FALSE,
              max_iter = 5000
)

SA_df <- tibble(Variables=colnames(mod2@X),
                Main_effect=t(SA$S$mean),
                Total_effect=t(SA$T$mean)) 

SA_df %<>% pivot_longer(cols=c("Main_effect", "Total_effect"),
                        names_to="Effect Type", 
                        values_to="Sensitivity Index")


ggplot(SA_df, aes(x=Variables, y=`Sensitivity Index`,fill=`Effect Type`)) + 
  geom_bar(stat="identity", position="dodge") + 
  theme_bw(base_size=18) +
  coord_flip()

dir.create(file.path("SA_results", results_date))
ggsave(file.path("SA_results", results_date, "out2.pdf"))
