
data {
  int T; // number of observations
  int P; // number of stocks
  vector[P] r[T]; // matrix of log returns
  vector<lower = 0>[T] weights; // observation weights
  real df; // lkj prior degrees of freedom. A large value will induce correlation during unprecedented episodes
} 
parameters {
  
  corr_matrix[P] Omega; // the static correlations
  vector[P] mu; // The mean returns vector
  vector<lower = 0, upper = 1>[P] A; // the coefficient on lagged r in the GARCH model
  real<lower = 0, upper = 1-A[1]> B1; // the AR coefficient in the GARCH model
  real<lower = 0, upper = 1-A[2]> B2; // the AR coefficient in the GARCH model
  real<lower = 0, upper = 1-A[3]> B3; // the AR coefficient in the GARCH model
  real<lower = 0, upper = 1-A[4]> B4; // the AR coefficient in the GARCH model
  real<lower = 0, upper = 1-A[5]> B5; // the AR coefficient in the GARCH model
  real<lower = 0, upper = 1-A[6]> B6; // the AR coefficient in the GARCH model
  vector<lower = 0>[P] c; // the constant term in the GARCH model
  vector<lower = 0>[P] tau1; // starting value of tau
}
transformed parameters {
  vector<lower = 0>[P] tau[T]; // the time-varying scale of volatility
  vector[P] B;
  tau[1] <- tau1;
  
  B[1] <- B1; B[2] <- B2; B[3] <- B3;
  B[4] <- B4; B[5] <- B5; B[6] <- B6;
  
  for(p in 1:P){
    for(tt in 2:T){
      tau[tt][p] <- sqrt(c[p] + 
                         A[p]*pow(r[tt-1][p] - mu[p], 2) + 
                         B[p]*pow(tau[tt-1][p], 2));
      
    }
  }
  
}
model {
  vector[P] eta[T]; // N(0,1) residuals in the measurement equation
  // Priors
  mu ~ normal(0, 1);
  Omega ~ lkj_corr(df);
  c ~ normal(0,0.1);
  A ~ normal(1,0.5);
  B ~ normal(0,0.2);
  tau1 ~ cauchy(0, 2);
  
  // Likelihood
  for(t in 2:T){
    //r[t] ~ multi_normal(mu,quad_form_diag(Omega, tau[t]));
    increment_log_prob(weights[t]*multi_normal_log(r[t], mu, quad_form_diag(Omega, tau[t])));
  }
}
generated quantities {
  vector[P] r_proj; // projected returns for the next week
  vector[P] tau_tp1; // projected tau for next week
  real portfolio_return; // projected portfolio return
  
  // Forecast next week's variance parameters
  for (p in 1:P){
    tau_tp1[p] <- sqrt(c[p] + 
                         A[p]*pow(r[T][p] - mu[p], 2) + 
                         B[p]*pow(tau[T][p], 2));
  }
  
  // A draw of next week's returns
  r_proj <- multi_normal_rng(mu, quad_form_diag(Omega, tau_tp1));
  // An equal weighted portfolio return is just the mean of the individual returns.
  portfolio_return <- mean(r_proj);
  
  
}