library(readr)
library(tidyverse)
library(magrittr)
library(doParallel)
library(foreach)
library(rstan)
library(DiceKriging)
library(GGally)

# 65-point lhs design, repeated 6 times
in_df <- read_table2("results/JB/Inputs_6x65.txt")
out_df <- read_table2("results/JB/Output_6x65_various.txt")
source("r/calib_functions.R")

out_names <- names(out_df)


data_df <- cbind(in_df, out_df)


ggplot(data_df, aes(x=p_transfer_info, y=mean_freq_plan))+ geom_point()

GGally::ggcorr(data_df)
ggpairs(data_df)


D <- in_df[1:65,]

out_ms <- apply(out_df, 2, mean)
out_vs <- apply(out_df, 2, var)

out_df %<>% apply(2, function(x)(x-mean(x))/sd(x)) 



out_df <- cbind(expand.grid(Point=1:65,Repetition=1:6), out_df)



outs <- out_df %>% 
  group_by(Point) %>% 
  summarise_at(out_names, list(mean=mean,var=var))

varied_pars <- colnames(D)

D <- apply(D,2,function(x) (x-min(x)) /(max(x)-min(x)) )

mod_mfp <- km(design=D, response=outs$mean_freq_plan_mean,
              noise.var=outs$mean_freq_plan_var,
              multistart = 10,control=list(trace=T))




cv1 <- do_kfold(D, 
                outs$mean_freq_plan_mean,
                outs$mean_freq_plan_var,13)



varied_pars


predD <- predict(mod_mfp, newdata=D, type="SK")

predD <- cbind.data.frame(D,predD[c("mean", "lower95", "upper95")])

stdize <- function(x) (x-mean(x))/sd(x)

ggplot(predD, aes(x=p_info_contacts,y=mean))  + 
  geom_ribbon(aes(ymin=lower95,ymax=upper95),fill="darkgreen", alpha=0.2)+ 
  geom_line() +
  theme_bw() + 
  geom_point(data=data_df,aes(y=stdize(mean_freq_plan)))


ggplot(predD, aes(x=p_transfer_info,y=mean))  + 
  geom_ribbon(aes(ymin=lower95,ymax=upper95),fill="darkgreen", alpha=0.2)+ 
  geom_line() +
  theme_bw() + 
  geom_point(data=data_df, aes(y=stdize(mean_freq_plan)))





newd <- newdata_1d("p_info_contacts", D)

pred1 <- predict(mod_mfp, newdata=newd, type="SK")

pred1 <- cbind.data.frame(newd,pred1[c("mean", "lower95", "upper95")])

ggplot(pred1, aes(x=p_info_contacts,y=mean))  + 
  geom_ribbon(aes(ymin=lower95,ymax=upper95),fill="darkgreen", alpha=0.2)+ 
  geom_line() +
  theme_bw()

### principal components

yy <- out_df[out_names[1:3]]

source("R/calib_functions.R")
n_pc <- 3
pcs <- extract_principal_components(t(yy),n_pc)

standard_directions <- pcs$standard_directions
scores <- pcs$scores

message(paste0("Cumulative proportion of variance for first ", n_pc,
               " principal components: \n"), 
        paste0(pcs$prop_variance, sep="\n"))

stopifnot(all(apply(scores, 2, var) -1 <1e-06))
stopifnot(all(apply(scores, 2, mean)<1e-06))

scores_df <- as_tibble(scores,.name_repair = "unique")


scores_df <-cbind(Point=out_df$Point, scores_df)

input_means <- scores_df %>% group_by(Point) %>% summarise_all(mean) %>% 
  arrange(Point)
input_vars <- scores_df %>% group_by(Point) %>% summarise_all(var) %>% 
  arrange(Point)

cv1 <- do_kfold(D,input_means$scores_1, input_vars$scores_1, n_folds = 13)
cv2 <- do_kfold(D,input_means$scores_2, input_vars$scores_2, n_folds = 13)
cv3 <- do_kfold(D,input_means$scores_3, input_vars$scores_3, n_folds = 13)
library(doParallel)
library(foreach)

registerDoParallel()

leave_out <- 16

mods <- map2((input_means %>% select(-Point)),#[-leave_out,],
             (input_vars %>% select(-Point)),#[-leave_out,],
             function(m, v){
               km(design=D,#[-leave_out,],
                  response=m, 
                  noise.var=v,
                  multistart = 10) # defaults to matern5_2
             }
)


# true values
# 
# p_drop_contact   p_info_mingle p_info_contacts p_transfer_info           error 
# 0.843750        0.546875        0.750000        0.031250        0.500000 
# p_find      speed_expl 
# 0.640625        0.828125 

stan_data <- make_stan_data(obs=c(-2.3,1,-1.95),
                            mods=mods,
                            design = D,#[-leave_out,],
                            standard_directions = standard_directions,
                            sim_means = c(0,0,0))

stan_data$theta_as <-  c(1,1,1,1,1,1,1)
stan_data$theta_bs <-  c(1,1,1,1,1,1,1)
stan_data$theta_uppers <-  c(1,1,1,1,1,1,1)
stan_data$theta_lowers <-  c(0,0,0,0,0,0,0)

cal_mod <- stan_model("stan/calib.stan")

cal_fit <- sampling(cal_mod, iter=10000, cores=3, chains=3, data=stan_data, 
                    thin=5)
  #                    init=initials)

y_obs <- as.matrix(cal_fit, "y_obs")

for (i in 1:3){
  y_obs[,i] <- y_obs[,i] * sqrt(out_vs[i]) + out_ms[i]
}

real_vals <-  c(-2.3,1,-1.95)*sqrt(out_vs[1:3]) + out_ms[1:3]

y_df <- as.tibble(y_obs)

colnames(y_df) <- out_names[1:3]

y_df %<>% pivot_longer(cols=out_names[1:3], names_to="Output variable", 
                      values_to="Value")

ggplot(y_df, aes(x=Value)) + geom_density(fill="darkgreen", alpha=0.5) + 
  facet_wrap(~`Output variable`, scales="free") + 
  theme_bw(base_size = 14) +
  geom_vline(data=tibble(`Output variable`=out_names[1:3], 
                         Real_vals=real_vals),
             aes(xintercept=Real_vals),
             col="red", linetype=2,size=1.5) +
  ggtitle("Posterior Predicted Output")


theta <- as.matrix(cal_fit, "theta")

theta_df <- as.tibble(theta)

colnames(theta_df) <- varied_pars


theta_df %<>% pivot_longer(cols=varied_pars, 
                           names_to="Parameter", 
                           values_to="Value")


ggplot(theta_df, aes(x=Value)) + geom_density(fill="darkgreen", alpha=0.5) + 
  facet_wrap(~`Parameter`, scales="free") + 
  theme_bw(base_size = 14) +
  ggtitle("Calibrated Posterior Distribution")


pair_df <- as.tibble(theta)

ggpairs(pair_df, lower=list(continuous = "density",combo = "box_no_facet"),
        upper=list(continuous=null))


