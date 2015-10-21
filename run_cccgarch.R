
library(Quandl); library(rstan); library(dplyr); library(reshape2); library(stringr)
cc <- function(x) x[complete.cases(x),]
key <- read.csv("~/Documents/quandl_key.csv", header = F)[1] 
key <- key[1,1] %>% as.character()
Quandl.api_key(key)

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

weights1 <- rep(1, nrow(data1))
weights2 <- rep(0.02, nrow(data1))
weights2[(length(weights2)-20):length(weights2)] <- 1
ccgarch_data <- list(T = nrow(data1), P = ncol(data1), r = data1, weights = weights1, df = 1.2, tau1 = abs(data[1,]))

ccgarch1 <- stan(file = "~/Documents/Thesis_work/CCC_GARCH2.stan", data = ccgarch_data, iter = 10, chains = 1, control=list( adapt_delta=0.9))
ccgarch2 <- stan(fit = ccgarch1, data = ccgarch_data, iter = 500, chains = 4, cores = 4, warmup = 150, control=list( adapt_delta=0.9))

#save(ccgarch2, file = "20151002_ccgarchfit2.RData")

pars <- rstan::extract(ccgarch2, permuted = F)
pars2 <- do.call(rbind, lapply(1:4, function(x) pars[,x,])) %>% as.data.frame
taus <- pars2[,grepl("tau", names(pars2))]

portfolio_return <- pars2[,grepl("portfolio", names(pars2))]

hist(portfolio_return)

taus <- taus %>% melt %>%
  mutate(observation = str_extract(variable, perl("(?<=\\[)[0-9]{1,3}")) %>% as.numeric,
         series = str_extract(variable, perl("(?<=,)[0-9]{1,3}")) %>% as.numeric)
taus <- taus %>% group_by(variable) %>%
  summarise(median = quantile(value, 0.5),
            lower = quantile(value, 0.025),
            upper = quantile(value, 0.975),
            series = first(series),
            observation = first(observation)) %>% filter(!is.na(series))


taus %>% ggplot(aes(x = observation, y = median)) +
  geom_ribbon(aes(ymin= lower, ymax = upper), fill = "orange") +
  geom_line() +
  facet_grid(series~., scales = "free")
