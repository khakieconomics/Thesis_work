
library(Quandl); library(rstan); library(dplyr); library(reshape2); library(stringr); library(randomForest); library(ggplot2); library(ggthemes)
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

raw_data <- aapl2 %>% left_join(ibm2) %>% left_join(goog2) %>% left_join(ford2) %>% left_join(BoA2) %>% left_join(JNJ2) %>% cc 

data <- raw_data %>% select(-Date) %>% apply(2, function(x) diff(log(x))) %>% as.data.frame
data1 <- data*100

port_ret <- data.frame(Actual = rowMeans(data1), Date = raw_data$Date[-1])

weights1 <- rep(1, nrow(data1))
ccgarch_data <- list(T = nrow(data1), P = ncol(data1), r = data1, weights = weights1, df = 1.2, tau1 = abs(data[1,]))

ccgarch1 <- stan(file = "~/Documents/Thesis_work/CCC_GARCH2.stan", data = ccgarch_data, iter = 10, chains = 1, control=list( adapt_delta=0.9))



# Run simulations -----------------------------------------------------------
dates <- raw_data$Date
gfc_dates <- dates[dates>=as.Date("2007-06-29") & dates< as.Date("2013-07-01")]

VaR <- data.frame(Week = gfc_dates, Unweighted_VaR = NA, Weighted_VaR = NA, Actual_returns = NA)

# For holding the model runs
e <- new.env()

count <- 0
# Run simulations for GFC period (second half of 2008)
for (d in gfc_dates){
  count <- count + 1
  # Set up subset data
  d <- as.Date(d)
  raw_ss <- raw_data %>% filter(Date<=d) %>% select(-Date)
  raw_ss <- raw_ss %>% apply(2, function(x) diff(log(x))) %>% as.data.frame
  raw_ss <- raw_ss*100
  
  # Generate analogy scores
  
  analogy_ss <- as.data.frame(raw_ss)
  # Generate data to estimate analogy scores
  for(i in names(raw_ss)){
    for(p in 1:2){
      x <- lag(analogy_ss[,i], p)
      analogy_ss$x <- x^2
      names(analogy_ss)[ncol(analogy_ss)] <- paste(i, p, sep = "_")
    }
  }
  
  # Estimate random forests
  analogy_ss <- analogy_ss %>% cc
  aapl_mod <- randomForest(I(AAPL^2) ~ . - IBM - GOOGL - FORD - BAC - JNJ, data = analogy_ss, ntree = 5000, proximity = T, oob.prox = T, nodesize = round(nrow(analogy_ss)/5))
  ibm_mod <- randomForest(I(IBM^2) ~ . - AAPL - GOOGL - FORD - BAC - JNJ, data = analogy_ss, ntree = 5000, proximity = T, oob.prox = T, nodesize = round(nrow(analogy_ss)/5))
  googl_mod <- randomForest(I(GOOGL^2) ~ . - IBM - AAPL - FORD - BAC - JNJ, data = analogy_ss, ntree = 5000, proximity = T, oob.prox = T, nodesize = round(nrow(analogy_ss)/5))
  ford_mod <- randomForest(I(FORD^2) ~ . - IBM - GOOGL - AAPL - BAC - JNJ, data = analogy_ss, ntree = 5000, proximity = T, oob.prox = T, nodesize = round(nrow(analogy_ss)/5))
  bac_mod <- randomForest(I(BAC^2) ~ . - IBM - GOOGL - FORD - AAPL - JNJ, data = analogy_ss, ntree = 5000, proximity = T, oob.prox = T, nodesize = round(nrow(analogy_ss)/5))
  jnj_mod <- randomForest(I(JNJ^2) ~ . - IBM - GOOGL - FORD - BAC - AAPL, data = analogy_ss, ntree = 5000, proximity = T, oob.prox = T, nodesize = round(nrow(analogy_ss)/5))
  
  # Construct analogy weights (0 for first two as they have missing values and we need the analogy_weights vector to be same dimension as number of obs)
  analogy_weights <- c(0,0,(aapl_mod$proximity[nrow(aapl_mod$proximity),] +
    jnj_mod$proximity[nrow(jnj_mod$proximity),] +
    ibm_mod$proximity[nrow(ibm_mod$proximity),] +
    googl_mod$proximity[nrow(googl_mod$proximity),] +
    ford_mod$proximity[nrow(ford_mod$proximity),] +
    bac_mod$proximity[nrow(bac_mod$proximity),])/6)
  
  
  # Put together data for models
  ccgarch_data_1 <- list(T = nrow(raw_ss), P = ncol(raw_ss), r = raw_ss, weights = rep(1, nrow(raw_ss)), df = 1.2)
  ccgarch_data_2 <- list(T = nrow(raw_ss), P = ncol(raw_ss), r = raw_ss, weights = analogy_weights, df = 1.2)
  
  # Run the models and save the output to an environment
  ccgarch2_unweighted <- stan(fit = ccgarch1, data = ccgarch_data_1, iter = 500, chains = 4, cores = 4, warmup = 150, control=list( adapt_delta=0.9))
  assign(paste0("unweighted_", count, d),ccgarch2_unweighted, envir = e)
  ccgarch2_weighted <- stan(fit = ccgarch1, data = ccgarch_data_2, iter = 500, chains = 4, cores = 4, warmup = 150, control=list( adapt_delta=0.9))
  assign(paste0("weighted_", count, d),ccgarch2_weighted, envir = e)
  
  # Get VaR for unweighted model
  pars_unweighted <- rstan::extract(ccgarch2_unweighted, permuted = F)
  pars_unweighted <- do.call(rbind, lapply(1:4, function(x) pars_unweighted[,x,])) %>% as.data.frame
  portfolio_return_unweighted <- pars_unweighted[,grepl("portfolio", names(pars_unweighted))]
  
  # Get VaR for weighted model
  pars_weighted <- rstan::extract(ccgarch2_weighted, permuted = F)
  pars_weighted <- do.call(rbind, lapply(1:4, function(x) pars_weighted[,x,])) %>% as.data.frame
  portfolio_return_weighted <- pars_weighted[,grepl("portfolio", names(pars_weighted))]
  
  # Put VAR and actual portfolio returns in the output
  if(count < length(gfc_dates)){
    VaR$Unweighted_VaR[count + 1] <- quantile(portfolio_return_unweighted, 0.05)
    VaR$Weighted_VaR[count + 1] <- quantile(portfolio_return_weighted, 0.05)
  }
  print(paste("Unweighted VaR = ", quantile(portfolio_return_unweighted, 0.05)))
  print(paste("Weighted VaR = ", quantile(portfolio_return_weighted, 0.05)))

}

e_list <- as.list(e)
save(e_list, file = "weighted_unweighted_ccc_garch_A_prior_1.RData")

VaR$Actual_returns <- port_ret$Actual[port_ret$Date %in% gfc_dates]

save(VaR, file = "Value_at_risk_GFC_A_prior_1.RData")

VaR %>% melt(id = "Week") %>% rename(Series = variable) %>%
  mutate(Series = ifelse(Series=="Actual_returns", "Absolute\nActual returns", ifelse(Series=="Weighted_VaR", "Absolute\nVaR weighted\nmodel", "Absolute\nVaR unweighted\nmodel"))) %>%
  ggplot(aes(x = Week, y = abs(value), colour = Series)) + 
  geom_line() +
  theme_fivethirtyeight() +
  ggtitle("Same model, different weights\nVaR calculated with\nand without analogy weights\n") +
  annotate("text", x = as.Date("2008-03-01"), y = 10, label = "Quicker to jump\nduring crisis", colour = 4, size = 3) +
  annotate("text", x = as.Date("2010-01-01"), y = 7, label = "Quicker to fall\nafterwards", colour = 4, size = 3)
  
  
VaR %>% cc %>%
  mutate(`Weighted` = abs(abs(Actual_returns) - abs(Weighted_VaR)),
         `Unweighted` = abs(abs(Actual_returns) - abs(Unweighted_VaR)),
         Ratio = Weighted - Unweighted) %>%
  select(Week, Ratio) %>%
  ggplot(aes(x = Week, y = Ratio)) + 
  geom_line() +
  geom_smooth() +
  theme_fivethirtyeight() +
  ggtitle("Ratio of cost of weighted\nmodel to cost of\nunweighted model")


VaR %>% cc %>%
  mutate(`Weighted` = cumsum(abs(Actual_returns) - abs(Weighted_VaR)),
         `Unweighted` = cumsum(abs(Actual_returns) - abs(Unweighted_VaR)),
         Ratio = Weighted/Unweighted) %>%
  select(Week, Weighted, Unweighted) %>% melt(id = "Week") %>%
  ggplot(aes(x = Week, y = value, colour = variable)) + 
  geom_line() +
  theme_fivethirtyeight() +
  ggtitle("Ratio of cost of weighted\nmodel to cost of\nunweighted model")


cors <- VaR %>%cc %>% summarise(Corr.unweighted = cor(abs(Actual_returns^2),abs(Unweighted_VaR^2)),
                        Corr.weighted = cor(abs(Actual_returns^2),abs(Weighted_VaR^2)))
cors[1,2]/cors[1,1]


VaR %>% mutate(Difference = Weighted_VaR - Unweighted_VaR) %>%
  ggplot(aes(x = Difference, y = Actual_returns)) +
  geom_point() +
  geom_smooth()



# Compete with DCC GARCH --------------------------------------------------
library(rmgarch)

igarch11.spec <- ugarchspec(mean.model = list(armaOrder = c(0,0)),
                           variance.model = list(garchOrder = c(1,1),model = "sGARCH"),
                           distribution.model = "norm")

dcc.garch11.spec  <- dccspec(uspec = multispec( replicate(6,igarch11.spec)), 
                             dccOrder = c(1,1), 
                             distribution = "mvnorm")

cl1 <- makeCluster(spec = 4)
# Be careful, this takes a few hours to estimate
#rolling_fit <- dccroll(spec = dcc.garch11.spec, data = data1,n.ahead = 1, refit.every = 1, n.start = 150,refit.window = "expanding"  , cluster = cl1)
covs <- rcov(rolling_fit)

covlist <- plyr::alply(covs, 3)

perc <- function(cov, n1 = 1000){
  sims <- mvrnorm(n = n1, mu = rep(0, nrow(cov)), Sigma = cov)
  quantile(apply(sims, 1, mean), 0.05)
} 

VaRDCC <- lapply(covlist, perc) %>% unlist
VaRDCC.df <- data.frame(Week = seq(raw_data$Date[151], by = "week", length.out = length(VaRDCC)), VaRDCC)

VaR2 <- left_join(VaR, VaRDCC.df)

VaR2 %>% melt(id = "Week") %>% rename(Series = variable) %>%
  mutate(Series = ifelse(Series=="Actual_returns", "Absolute\nActual returns", ifelse(Series=="Weighted_VaR", "Absolute\nVaR weighted\nmodel", ifelse(Series=="VaRDCC","Absolute\nVaR DCC","Absolute\nVaR unweighted\nmodel")))) %>%
  ggplot(aes(x = Week, y = abs(value), colour = Series)) + 
  geom_line() +
  theme_fivethirtyeight() +
  ggtitle("Same model, different weights\nVaR calculated with\nand without analogy weights\n") +
  annotate("text", x = as.Date("2008-03-01"), y = 10, label = "Quicker to jump\nduring crisis", colour = 4, size = 3) +
  annotate("text", x = as.Date("2010-01-01"), y = 7, label = "Quicker to fall\nafterwards", colour = 4, size = 3)


absdiff <- function(x, y) abs(abs(x) - abs(y))

VaR2  %>% cc%>% mutate(Unweighted_VaR = cumsum(absdiff(Unweighted_VaR,Actual_returns)),
                Weighted_VaR = absdiff(Weighted_VaR, Actual_returns) %>% cumsum,
                VaRDCC = absdiff(VaRDCC, Actual_returns) %>% cumsum) %>%
  melt(id = "Week") %>%
  ggplot(aes(x = Week, y = value, colour = variable)) + geom_line()
  