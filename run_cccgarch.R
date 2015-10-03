
library(Quandl); library(rstan); library(dplyr); library(reshape2); library(stringr)
cc <- function(x) x[complete.cases(x),]
Quandl.api_key("your key here")

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
save(data, file = "6_companies.RData")

plot.ts(data)
#data1 <- data[(nrow(data)-400):nrow(data),]
data1 <- data*100

ccgarch_data <- list(T = nrow(data1), P = ncol(data1), r = data1, weights = rep(1, nrow(data1)), df = 1.2, tau1 = rep(0, 6))

ccgarch1 <- stan(file = "~/Documents/Thesis_work/CCC_GARCH2.stan", data = ccgarch_data, iter = 10, chains = 1, control=list( adapt_delta=0.9))
ccgarch2 <- stan(fit = ccgarch1, data = ccgarch_data, iter = 300, chains = 4, cores = 4, warmup = 150, control=list( adapt_delta=0.9))

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


taus %>% ggplot(aes(x = observation, y = median)) +
  geom_line() +
  facet_grid(series~., scales = "free")
