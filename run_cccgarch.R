
library(Quandl); library(rstan); library(dplyr)
cc <- function(x) x[complete.cases(x),]
Quandl.api_key("BrfAiD2VDaxQEw2zgEfQ")

aapl <- Quandl("YAHOO/AAPL")
ibm <- Quandl("YAHOO/IBM")
goog <- Quandl("YAHOO/GOOGL")
ford <- Quandl("YAHOO/FORD")
BoA <- Quandl("YAHOO/BAC")
JNJ <- Quandl("YAHOO/JNJ")


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
data1 <- data[(nrow(data)-400):nrow(data),]

ccgarch_data <- list(T = nrow(data1), P = ncol(data1), r = data1, weights = rep(1, nrow(data1)), df = 1.2)

ccgarch1 <- stan(file = "CCC_GARCH.cpp", data = ccgarch_data, iter = 50, chains = 1)

ccgarch2 <- stan()