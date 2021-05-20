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


newdata_1d <- function(par_name,D,n=100){
  varied_pars <-  colnames(D)
  match.arg(par_name, varied_pars)
  constant_vals <- as.list(rep(0.5, ncol(D)-1))
  ind <- which(par_name==varied_pars)
  newdata <- expand.grid(append(constant_vals, 
                                list(seq(0,1,length.out=n)),
                                after=ind-1))
  colnames(newdata) <- varied_pars
  return(newdata)
}


extract_principal_components <- function(data_matrix, n_pc){
  m <- dim(data_matrix)[2]
  svd_xsi <- svd(data_matrix)
  comp_var <- svd_xsi$d**2
  
  prop_variance <- cumsum(comp_var[1:n_pc])/sum(comp_var) 
  
  scores <- svd_xsi$v[,1:n_pc] *sqrt(m - 1) # wi in higdon
  standard_directions <- diag(svd_xsi$d[1:n_pc]) %*% t(svd_xsi$u[,1:n_pc]) / sqrt(m-1)
  colnames(scores) <- paste0("scores_", 1:n_pc)
  return(list(scores=scores,
              standard_directions=standard_directions,
              prop_variance=prop_variance))
}



make_stan_data <- function(obs, mods, design, 
                           standard_directions,
                           sim_means){
  chol_covars <- map(mods, function(x) x@T)
  em_means <- map(mods, function(x) x@trend.coef)
  covar_pars <- map(mods, function(x) x@covariance@range.val)
  em_var <- map(mods, function(x) x@covariance@sd2)
  X <- design
  
  inv_chol_covars <- map(chol_covars, solve)
  inv_covars <- map(inv_chol_covars, function(X) (X) %*% t(X))
  alpha <- map2(mods,inv_covars,  
                function(m, C_inv){
                  z = m@y - m@trend.coef
                  return((C_inv %*% z)[,1])
                } )
  
  stan_data <- list(
    d=dim(X)[2],
    n_eta=length(obs),
    m = dim(X)[1],
    X=X,
    y=obs,
    n_pc=dim(standard_directions)[1],
    KK=standard_directions,
    cov_pars=do.call(rbind,covar_pars),
    em_var=unlist(em_var),
    em_mu=unlist(em_means),
    inv_covar=inv_covars,
    alpha=alpha,
    mean_simulation=sim_means
  )
  return(stan_data)
}


#' Plot each row or column of a matrix
#' 
#' @param sample_mat The matrix to plot
#' @param plot_dim Which dimension to plot - must be one of "columns" or "rows"
#' @param add Should the plot be added to an existing. False by default.
#' 
#' 
plot_matrix <- function(target_matrix, plot_dim =c("columns","rows"), add=F, ...){
  plot_dim <- match.arg(plot_dim)
  if (plot_dim == "rows"){
    target_matrix <- t(target_matrix)
  }
  
  iters <- 1:ncol(target_matrix)
  if (!add){
    plot(target_matrix[,1], ylim=range(target_matrix), type="l", ...)  
  }
  trash <- sapply(iters, function(j) points(target_matrix[,j], type="l", ...))
}


