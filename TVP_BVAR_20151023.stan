data {
  int T; // The number of observations
  int P; // The number of variables in the system
  matrix[T, P] Y; // The data matrix
  real lkj_nu_resid;
  real lkj_nu_cov;
  vector[P] intercept_start;
  matrix[P,P] theta1_start;
  matrix[P,P] theta2_start;
}
parameters {
  vector[P] intercept[T];
  matrix[P,P] theta1[T]; // The parameters used in the model that are transformed to N(0,1)
  matrix[P,P] theta2[T]; // The parameters used in the model that are transformed to N(0,1)
  vector<lower = 0>[P] tau_int; // Scale of intercept standard deviations
  vector<lower = 0>[P] tau_resid; // scale of residual variance
  vector<lower = 0>[P*P] tau_theta1; // scale of theta1 uncertainty
  vector<lower = 0>[P*P] tau_theta2; // scale of theta1 uncertainty
  corr_matrix[P] omega_resid; // Residual correlation
  corr_matrix[P] omega_int; // correlation on intercepts
  corr_matrix[P*P] omega_theta1; // correlation on theta1 innovations
  corr_matrix[P*P] omega_theta2; // correlation on theta2 innovations
}
transformed parameters {
  cov_matrix[P] Sigma_int;
  cov_matrix[P] Sigma_resid;
  cov_matrix[P*P] Sigma_theta1;
  cov_matrix[P*P] Sigma_theta2;
  
  Sigma_int <- quad_form_diag(omega_int, tau_int);
  Sigma_resid <- quad_form_diag(omega_resid, tau_resid);
  Sigma_theta1 <- quad_form_diag(omega_theta1, tau_theta1);
  Sigma_theta2 <- quad_form_diag(omega_theta2, tau_theta2);

}
model {
  // Priors on scale of innovations
  tau_int ~ cauchy(0, 0.05);
  tau_resid ~ cauchy(0, 0.5);
  tau_theta1 ~ cauchy(0, 0.05);
  tau_theta1 ~ cauchy(0, 0.05);
  
  // Priors on correlations
  omega_int ~ lkj_corr(lkj_nu_cov);
  omega_resid ~ lkj_corr(lkj_nu_resid);
  omega_theta1 ~ lkj_corr(lkj_nu_cov);
  omega_theta2 ~ lkj_corr(lkj_nu_cov);
  
  // priors on starting values
  
  increment_log_prob(normal_log(intercept[1], intercept_start, 0.5));
  increment_log_prob(normal_log(intercept[2], intercept_start, 0.5));
  increment_log_prob(normal_log(to_vector(theta1[1]), to_vector(theta1_start), 0.5));
  increment_log_prob(normal_log(to_vector(theta1[2]), to_vector(theta1_start), 0.5));
  increment_log_prob(normal_log(to_vector(theta2[1]), to_vector(theta2_start), 0.5));
  increment_log_prob(normal_log(to_vector(theta2[2]), to_vector(theta2_start), 0.5));
  
  
  // likelihood 
  
  for (t in 3:T){
    increment_log_prob(multi_normal_log(to_vector(row(Y, t)), intercept[t] + theta1[t]*to_vector(row(Y, t-1)) + theta2[t]*to_vector(row(Y, t-2)), Sigma_resid));
    increment_log_prob(multi_normal_log(intercept[t], intercept[t-1], Sigma_int));
    increment_log_prob(multi_normal_log(to_vector(theta1[t]), to_vector(theta1[t-1]), Sigma_theta1));
    increment_log_prob(multi_normal_log(to_vector(theta2[t]), to_vector(theta2[t-1]), Sigma_theta2));
  }
}
generated quantities {
  vector[P] yhat_lr[T];
  vector[P] Yhat[T]; // The one-year ahead forecasts for the weighted model
  yhat_lr[1] <- to_vector(row(Y, 1));
  yhat_lr[2] <- to_vector(row(Y, 2));
  Yhat[1] <- to_vector(row(Y, 1));
  Yhat[2] <- to_vector(row(Y, 2));
  
  for(t2 in 2:T){
    {vector[P] Ytemp[102]; // 
      int count;
      int count2;
      count <- 0;
      for(t1 in (t2-1):(t2)){
        count <- count + 1;
        Ytemp[count] <- to_vector(row(Y,t1));
      }
      // Generate forecasts for one-year ahead projections
      count2 <- 2;
      for(t in (t2+1):(t2+100)){
        count2 <- count2 + 1;
        Ytemp[count2] <- multi_normal_rng(intercept[t2] + theta1[t2]*Ytemp[count2-1] + theta2[t2]*Ytemp[count2-2], Sigma_resid);
        if(t == (t2+4)){
          Yhat[t2] <- Ytemp[count2];
        }
        if(t == (t2+100)){
          yhat_lr[t2] <- Ytemp[count2];
        }
      }
      }
  }
  
  
}