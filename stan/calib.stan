
functions {

  real matern5_2_kernel (vector h, vector tau, real sigma2, int d){
    vector[d] vals;
    vector[d] h_abs;
    vector[d] h_sq;
    vector[d] tau_sq;
    for (i in 1:d){
      h_abs[i] = fabs(h[i]);
      h_sq[i] = h[i]^2;
      tau_sq[i] = tau[i]^2;
    }

    vals = ((1 + sqrt(5)* h_abs ./ tau + 5 * h_sq ./(3* tau_sq)) .* 
      exp(-sqrt(5)* h_abs ./ tau));

    

    return sigma2 * prod(vals);
  }

}



data {
	int d; // number of input dimensions (to be calibrated)
  int n_eta; // number of observations of reality
  int m; // number of design points in the original simulation

  matrix[m, d] X; // simulation design matrix

  vector[n_eta] y; // real observations

  int n_pc; // number of principle components

  matrix[n_pc, n_eta] KK; // principle directions

  matrix[n_pc, d] cov_pars; // covariance parameters

  vector[n_pc] em_var; // emulator variances
  vector[n_pc] em_mu;

  matrix[m, m] inv_covar[n_pc];

  vector[m] alpha[n_pc]; // inv_covar * (eta - mu_eta)

  // vector[d] theta_prior_means;
  // vector[d] theta_prior_sd;

  vector[n_eta] mean_simulation;

  vector[d] theta_uppers;
  vector[d] theta_lowers;

  vector[d] theta_as;
  vector[d] theta_bs;
}
transformed data{
  vector[d] theta_ranges;

  theta_ranges = theta_uppers - theta_lowers;

}

parameters {
  // vector[theta]
  //vector<lower=1, upper=3>[2] theta_12; // true value of the parameters
  vector<lower=0, upper=1>[d] theta; // true value of the parameters


  real<lower=0> sigma_obs;

}


transformed parameters {
  vector[n_pc] em_post_mu;
  vector[n_pc] em_post_var;
  vector[m] cross_covar[n_pc];

  vector[n_eta] obs_mu;
  matrix[n_eta, n_eta] obs_var;

  // vector[d] theta;

//  theta = theta_raw .* theta_ranges  + theta_lowers;
  

  for (j in 1:n_pc){
    for (i in 1:m){
      vector[d] distances;
      distances = X[i]' - theta;
      cross_covar[j][i] = matern5_2_kernel(distances, cov_pars[j]', em_var[j], d);
    }  
  }
  
  for (j in 1:n_pc){
    em_post_mu[j] = em_mu[j] + dot_product(alpha[j] , cross_covar[j]);
    em_post_var[j] = em_var[j] - cross_covar[j]' * inv_covar[j] * cross_covar[j];
  }


  obs_mu = mean_simulation + KK' * em_post_mu;

  obs_var = KK' * diag_pre_multiply(em_post_var,  KK) + diag_matrix(rep_vector(sigma_obs^2, n_eta));

  obs_var = (obs_var + obs_var')/2; // for stability / symmetricity only
}

model {




  y ~ multi_normal(obs_mu, obs_var);

  // theta ~ normal(theta_prior_means, theta_prior_sd);
  // theta_12 ~ normal(theta_prior_means[1:2],theta_prior_sd[1:2]);

  for (i in 1:d){
    theta[d] ~ beta(theta_as[d], theta_bs[d]); 
  }
  

  sigma_obs ~ normal(0,0.1);

	
}
generated quantities {
  vector[n_eta] y_obs;
  y_obs = multi_normal_rng(obs_mu, obs_var);

}




