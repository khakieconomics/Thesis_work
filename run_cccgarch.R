
library(Quandl); library(rstan); library(dplyr); library(reshape2); library(stringr)
cc <- function(x) x[complete.cases(x),]
Quandl.api_key("BrfAiD2VDaxQEw2zgEfQ")

aapl <- Quandl("YAHOO/AAPL", collapse = "weekly")
ibm <- Quandl("YAHOO/IBM", collapse = "weekly")
goog <- Quandl("YAHOO/GOOGL", collapse = "weekly")
ford <- Quandl("YAHOO/FORD", collapse = "weekly")
BoA <- Quandl("YAHOO/BAC", collapse = "weekly")
JNJ <- Quandl("YAHOO/JNJ", collapse = "weekly")


changer <- function(x, name){
  x <- x %>% select(Date, `Adjusted Close`) %>% arrange(Date) %>% as.data.frame
  names(x)[2] <- name
  x
}

aapl2 <- changer(aapl, "AAPL")
ibm2 <- changer(ibm, "IBM")
goog2 <- changer(goog, "GOOGL")
ford2 <- changer(ford, "FORD")
BoA2 <- changer(BoA, "BAC")
JNJ2 <- changer(JNJ, "JNJ")

data <- aapl2 %>% left_join(ibm2) %>% left_join(goog2) %>% left_join(ford2) %>% left_join(BoA2) %>% left_join(JNJ2) %>% cc %>% select(-Date)

data <- data %>% apply(2, function(x) diff(log(x)))

plot.ts(data)
#data1 <- data[(nrow(data)-400):nrow(data),]
data1 <- data

ccgarch_data <- list(T = nrow(data1), P = ncol(data1), r = data1, weights = rep(1, nrow(data1)), df = 1.2)

code <- "
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
mu ~ normal(0, 0.5);
Omega ~ lkj_corr(df);
c ~ normal(0,1);
A ~ normal(0,1);
B ~ normal(0,1);
for(i in 1:T){
tau[i] ~ cauchy(0.1, 2.5);
}
tau_nu ~ cauchy(0.1, 2.5);

// Likelihood
for(t in 2:T){
eta[t] <- inverse(quad_form_diag(Omega, tau[t]))*(r[t] - mu);
nu[t] <- inverse(L)*(tau[t] - c - A .*r[t-1] - B .*tau[t-1]);
eta[t] ~ normal(0,1);
nu[t] ~ normal(0,1);
}
}"
ccgarch1 <- stan(file = "CCC_GARCH.cpp", data = ccgarch_data, iter = 10, chains = 1, control=list( adapt_delta=0.9))
ccgarch2 <- stan(fit = ccgarch1, data = ccgarch_data, iter = 600, chains = 4, cores = 4, init = 0, warmup = 250, control=list( adapt_delta=0.9))
save(ccgarch2, file = "20150929_ccgarchfit2.RData")
library(shinystan)
launch_shinystan(ccgarch2)

rstan::monitor(ccgarch2)
pars <- rstan::extract(ccgarch2, permuted = F)

pars2 <- do.call(rbind, lapply(1:4, function(x) pars[,x,])) %>% as.data.frame
str(pars2)
taus <- pars2[,grepl("tau", names(pars2))]

taus <- taus %>% melt %>%
  mutate(observation = str_extract(variable, perl("(?<=\\[)[0-9]{1,3}")) %>% as.numeric,
         series = str_extract(variable, perl("(?<=,)[0-9]{1,3}")) %>% as.numeric)
taus <- taus %>% group_by(variable) %>%
  summarise(median = median(value),
            series = first(series),
            observation = first(observation)) %>% filter(!is.na(series))


taus %>% ggplot(aes(x = observation, colour = series, group = series, y = median)) +
  geom_line() +
  facet_grid(series~., scales = "free")
