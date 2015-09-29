
data {
  int T; // number of observations
  int P; // number of stocks
  vector[P] r[T]; // matrix of log returns
  vector<lower = 0.01>[T] weights; // observation weights
  real df; // lkj prior degrees of freedom. A large value will induce correlation during unprecedented episodes
} 
parameters {
  vector<lower = 0.01>[P] tau[T]; // the time-varying scale of volatility
  corr_matrix[P] Omega; // the static correlations
  vector[P] mu; // The mean returns vector
  vector[P] A; // the coefficient on lagged r in the GARCH model
  vector[P] B; // the AR coefficient in the GARCH model
  vector[P] c; // the constant term in the GARCH model
  corr_matrix[P] Omega_nu; // The covariance matrix on the innovations in the state
  vector<lower = 0.01>[P] tau_nu; // the scale of errors in innovations to the state
}
transformed parameters {
  cov_matrix[P] Sig_nu; // The covariance matrix of innovations in the state
  matrix[P,P] L; // 
  Sig_nu <- quad_form_diag(Omega_nu, tau_nu); // generate 
  L <- cholesky_decompose(Sig_nu); // The Cholesky decompostion of the covariance matrix
}
model {
  vector[P] eta[T]; // N(0,1) residuals in the measurement equation
  vector[P] nu[T]; // N(0,1) residuals in the state equation
  // Priors
  mu ~ normal(0, 0.01);
  Omega ~ lkj_corr(df);
  Omega_nu ~ lkj_corr(4); // fairly tight prior on innovations being independent
  c ~ normal(0,1);
  A ~ normal(0,1);
  B ~ normal(0,0.8);
  for(i in 1:T){
    tau[i] ~ cauchy(0.1, 1);
  }
  tau_nu ~ cauchy(0.1, 1);
  
  // Likelihood
  for(t in 2:T){
    eta[t] <- inverse(quad_form_diag(Omega, tau[t]))*(r[t] - mu);
    nu[t] <- inverse(L)*(tau[t] - c - A .*r[t-1] - B .*tau[t-1]);
    eta[t] ~ normal(0,1);
    nu[t] ~ normal(0,1);
  }
}