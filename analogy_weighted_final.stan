data {
  // This version has a generated quantities block to generate estimates of the permanent component.
  int T; // number of observations
  int P; // number of variables
  vector[P] Y[T]; // An array of P columns and T rows containing the data
  vector[P*P] linear_est_1; // prior for theta_1
  vector[P*P] linear_est_2; // prior for theta_2
  vector[P] linear_int; // prior for the intercept
  vector[T] weights; // The analogy weights
  vector[T] weights2; // weights for the regular (unweighted) VAR (a vector of 1s with zeros for the last 4 observations)
  real nu_var; // The degrees of freedom paramter for the LKJ prior on the residual correlation matrix
  }
parameters {
  vector[P] intercept;
  matrix[P,P] theta_1; 
  matrix[P,P] theta_2; 
  corr_matrix[P] Omega_VAR;
  vector<lower= 0>[P] tau_VAR;
  vector[P] intercept2;
  matrix[P,P] theta_12; 
  matrix[P,P] theta_22; 
  corr_matrix[P] Omega_VAR2;
  vector<lower= 0>[P] tau_VAR2;
}
transformed parameters{
  matrix[P,P] Sig_VAR;
  matrix[P,P] Sig_VAR2;
  Sig_VAR2 <- quad_form_diag(Omega_VAR2, tau_VAR2);
  Sig_VAR <- quad_form_diag(Omega_VAR, tau_VAR);
}
model {
  // LKJ priors for the covariance matrices
  tau_VAR ~ cauchy(0,2.5);
  Omega_VAR ~ lkj_corr(nu_var);
  tau_VAR2 ~ cauchy(0,2.5);
  Omega_VAR2 ~ lkj_corr(nu_var);
  
  // prior for the regression coeffiecients
  //intercept ~ multi_normal(linear_int, Sig_int);
  intercept ~ normal(0, 1);
  to_vector(theta_1) ~ normal(linear_est_1, 0.3);
  to_vector(theta_2) ~ normal(linear_est_2, 0.3);
  intercept2 ~ normal(0, 1);
  to_vector(theta_12) ~ normal(linear_est_1, 0.3);
  to_vector(theta_22) ~ normal(linear_est_2, 0.3);
  
  for(t in 3:T){
    increment_log_prob(weights2[t]*multi_normal_log(Y[t], intercept2 + theta_12*Y[t-1] + theta_22*Y[t-2], Sig_VAR2));
    increment_log_prob(weights[t]*multi_normal_log(Y[t], intercept + theta_1*Y[t-1] + theta_2*Y[t-2], Sig_VAR));
  }
}
generated quantities{
  vector[P] Yhat; // The one-year ahead forecasts for the weighted model
  vector[P] Yhat2; // The one-year ahead forecasts for the unweighted model
  vector[P] yhat_lr;
  vector[P] yhat_lr2;
  
  {vector[P] Ytemp[102]; // 
  vector[P] Ytemp2[102]; // 
  int count;
  int count2;
  count <- 0;
  for(t1 in (T-1):(T)){
    count <- count + 1;
    Ytemp[count] <- Y[t1];
    Ytemp2[count] <- Y[t1];
  }
  // Generate forecasts for one-year ahead projections
  count2 <- 2;
  for(t in (T+1):(T+100)){
    count2 <- count2 + 1;
    Ytemp[count2] <- multi_normal_rng(intercept + theta_1*Ytemp[count2-1] + theta_2*Ytemp[count2-2], Sig_VAR);
    Ytemp2[count2] <- multi_normal_rng(intercept2 + theta_12*Ytemp2[count2-1] + theta_22*Ytemp2[count2-2], Sig_VAR2);
    if(t == (T+4)){
      Yhat <- Ytemp[count2];
      Yhat2 <- Ytemp2[count2];
    }
    if(t == (T + 100)){
      yhat_lr <- Ytemp[count2];
      yhat_lr2 <- Ytemp2[count2];
    }
  }}
}
