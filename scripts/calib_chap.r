library(here)
library(tibble)
library(dplyr)
library(tidyr)
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


varied_pars <- c("p_drop_contact", "p_info_mingle", "p_info_contacts", 
                 "p_transfer_info", "error", "speed_expl_stay")


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

pred_D <- predict(mod1,D, type="UK")




x<- seq(0,1,0.01)
D_star <- expand.grid(0.5,0.5,x,x,0.5,0.5)

colnames(D_star) <- names(D)

pred1 <- predict(mod1, D_star, type="UK")

pred_df <- tibble(D_star, f_hat =pred1$mean, sd_f_hat =pred1$sd)

ggplot(pred_df, aes(x=p_transfer_info, y=p_info_contacts,fill=f_hat)) + 
  geom_tile()

ggplot(pred_df, aes(x=p_transfer_info, y=p_info_contacts,fill=sd_f_hat)) + 
  geom_tile()



do_kfold <- function(D, yy, yy_var, n_folds){
  nn <- dim(D)[1]
  rands <- sample.int(nn)
  
  errors <- c()
  
  inds <- 1:nn
  size_test <- floor(nn/n_folds)
  for (i in 0:(n_folds-1)){
    test_ind <- i*size_test + 1:size_test
    train_ind <- (1:nn)[!(inds %in% test_ind)]
    
    train_sample <- D[rands[train_ind],]
    train_y_m <- yy[rands[train_ind]]
    train_y_v <- yy_var[rands[train_ind]]
    
    test_sample <- D[rands[test_ind],]
    test_y_m <- yy[rands[test_ind]]
    test_y_v <- yy_var[rands[test_ind]]
    
    mod <- km(response=train_y_m, design=train_sample, noise.var=train_y_v, 
              covtype = "matern5_2",
              multistart = 10, 
              control=list(trace=T))
    pred <- predict(mod, test_sample, type="UK")
    
    sq_stdised_err <- ((test_y_m - pred$mean)/ pred$sd)**2
    
    errors <- c(errors, sqrt(mean(sq_stdised_err)))
  }
  return(mean(errors))
}





errors <- do_kfold(D, yy, yy_var, 10)
k_rmsse1 <- mean(errors)

yy <- out_std_df$stdd_link_c_mean
yy_var <- out_std_df$stdd_link_c_var

mod2 <- km(response=yy, design=D, noise.var=yy_var, 
           covtype = "matern5_2",
           multistart = 10, 
           control=list(trace=T))#, upper=rep(5, length(varied_pars)))


D_star <- expand.grid(0.5,0.5,0.5,x,x,0.5)

colnames(D_star) <- names(D)


pred2 <- predict(mod2, D_star, type="UK")
pred_df <- tibble(D_star, f_hat =pred2$mean, sd_f_hat =pred2$sd)

ggplot(pred_df, aes(x=error, y=p_transfer_info, fill=f_hat)) + 
  geom_tile()

ggplot(pred_df, aes(x=error, y=p_transfer_info, fill=sd_f_hat)) + 
  geom_tile()

errors <- do_kfold(D, yy, yy_var, 10)
k_rmsse2 <- mean(errors)
# 1.482


mod1 <-km(response=train_y_m, design=train_sample, noise.var=train_y_v, 
          covtype = "matern5_2",
          multistart = 10, 
          control=list(trace=T))

imp_m <- 0 
imp_var <- 0.1

calib <- sample.int(400,1)


mod1 <- km(response=out_std_df$mean_freq_plan_mean[-calib], 
          design=D[-calib,], 
          noise.var=out_std_df$mean_freq_plan_var[-calib],
          covtype = "matern5_2",
          multistart = 10, 
          control=list(trace=T))

mod2 <- km(response=out_std_df$stdd_link_c_mean[-calib], 
          design=D[-calib,], 
          noise.var=out_std_df$stdd_link_c_var[-calib],
          covtype = "matern5_2",
          multistart = 10, 
          control=list(trace=T))


nonimp <- list()

target <- c(out_std_df$mean_freq_plan_mean[calib], 
            out_std_df$stdd_link_c_mean[calib])
target_var <- c(out_std_df$mean_freq_plan_var[calib], 
                out_std_df$stdd_link_c_var[calib])


for (i in 1:1e5){
  D_star <- matrix(runif(1000*6),ncol=6)
  colnames(D_star) <- names(D)
  pred1 <- predict(mod1, D_star, type="SK")
  pred2 <- predict(mod2, D_star, type="SK")
  error1 <- (pred1$mean - target[1])**2
  error2 <- (pred2$mean - target[2])**2
  # implausibility var is 0
  #imp1 <- error1/(target_var[1]/2 + pred1$sd**2)
  #imp2 <- error2/(target_var[2]/2 + pred2$sd**2)
  imp1 <- error1/(pred1$sd**2)
  imp2 <- error2/(pred2$sd**2)
  
  inds <- map2_lgl(imp1,imp2, function(x,y) max(x,y)<2)
  nonimp <- c(nonimp,list(D_star[inds,]))
}



