
# Read in libraries and data ----------------------------------------------
library(randomForest); library(shinystan);library(vars); 
library(zoo); library(rstan);  library(ggplot2); library(magrittr); 
library(lubridate); library(stringr); library(reshape2); library(parallel);  library(dplyr); 
cc <- function(x) x[complete.cases(x),]

load("cpi_r_u_rev.RData")

dataset.f <- vardat %>% filter(vintage_date=="2015-07-01") %>% 
  group_by(quarter = as.yearqtr(Date) %>% as.Date) %>%
  summarise(Unemp = first(U),
            CPI = first(CPI),
            r = first(r)) %>% cc %>% filter(quarter > "1960-01-01") %>% 
  as.data.frame


# Dates that will be the final observation for each dataset
enddates <- seq(from = as.Date("1975-01-01"), to = as.Date("2014-01-01"), by = "year")

out <- data.frame(quarter = enddates) %>% left_join(dataset.f)
out2 <- data.frame(quarter = enddates, matrix(NA, length(enddates), 6))
names(out2)[-1] <- paste0(names(dataset.f)[-1], rep(1:2, each = 3))
out <- left_join(out, out2)


e2 <- new.env()
ii <- 1
for(ii in 1:(length(enddates)-1)){
  
  kk <- enddates[ii]
  kk1 <- enddates[ii+1]
  
  # Subset the dataset and create a "quarterly" version
  dataset.q <- vardat %>% filter(vintage_date==kk) %>% 
    group_by(quarter = as.yearqtr(Date) %>% as.Date) %>%
    summarise(Unemp = first(U),
              CPI = first(CPI),
              r = first(r)) %>% cc %>% filter(quarter > "1960-01-01") %>% 
    as.data.frame
  
  # Remove the date column
  dat1 <- dataset.q[dataset.q$quarter<=kk,-1]

  # Run the random forests to get the proximity matrix weights
  rf1 <- randomForest(Unemp ~ ., data = dat1, proximity = T, ntree = 5000, nodesize = 40, oob.prox = T)
  rf2 <- randomForest(CPI ~ ., data = dat1, proximity = T, ntree = 5000, nodesize = 40, oob.prox = T)
  rf3 <- randomForest(r ~ ., data = dat1, proximity = T, ntree = 5000, nodesize = 40, oob.prox = T)
  proxmat <- (rf1$proximity + rf2$proximity + rf3$proximity)/3
  #proxmat[upper.tri(proxmat, diag  = T)] <- 0
  
  # And get the weights
  weights <- data.frame(w = proxmat[nrow(proxmat),], d = dataset.q$quarter[dataset.q$quarter<=kk])

  # Generate priors for VARs ---------------------------------------------------------------

  vm.int <- VAR(dat1, p = 2, type = "const")
  int_prior <- do.call(rbind, lapply(coef(vm.int), function(x) x[7,1]))
  firstlag <- do.call(rbind, lapply(coef(vm.int), function(x) x[1:3,1]))
  secondlag <- do.call(rbind, lapply(coef(vm.int), function(x) x[4:6,1]))


  # New rewrite -------------------------------------------------------------
  P <- ncol(dat1); T <- nrow(dat1)
  weights2 <- rep(1, nrow(dat1))
  #weights2[(length(weights2)-3):length(weights2)] <- 0
  model.data2 <- list(Y = dat1, 
                    linear_est_1 = as.vector(firstlag),
                    linear_est_2 = as.vector(secondlag),
                    weights = weights$w,
                    weights2 = weights2,
                    nu_var = 1.4,
                    P = P, T = T,
                    linear_int = int_prior %>% as.numeric)

  # hold on to your butts

  if(!exists("tvp_mod")){
    tvp_mod <- stan(file = "analogy_weighted_var3.cpp", data = model.data2, chains = 1, iter = 50)
  }

  tvp_mc <- mclapply(1:3, mc.cores = 3, function(i){
    stan(fit = tvp_mod, data = model.data2, chains =1, chain_id = i, verbose = TRUE, refresh = -1, 
       warmup = 200, iter = 400)
  })
  # Convert into a stan object
  tvp_mc1 <- sflist2stanfit(tvp_mc)
  
  # Save the results in an environment
  assign(paste0("Sim",kk),tvp_mc1, envir = e2)
  #launch_shinystan(tvp_mc1)

  # Extract the fitted series of one-year-ahead forecasts
  yhat <- rstan::extract(tvp_mc1, pars = "Yhat", permuted= FALSE)
  yhat2 <- rstan::extract(tvp_mc1, pars = "Yhat2", permuted= FALSE)
  yhat <- do.call(rbind, lapply(1:3, function(i) yhat[,i,])) %>% as.data.frame
  yhat2 <- do.call(rbind, lapply(1:3, function(i) yhat2[,i,])) %>% as.data.frame

  yhat.mean <- apply(yhat, 2, median)
  names(yhat.mean) <- NULL
  yhat2.mean  <- apply(yhat2, 2, median)
  names(yhat2.mean) <- NULL
  out[out$quarter==kk1,5:7] <- yhat.mean
  out[out$quarter==kk1,8:10] <- yhat2.mean
}

out2 <- out

out2[,5:7] <- out2[,5:7]- out[,2:4]
out2[,8:10] <- out2[,8:10]- out[,2:4]
out2 <- out2#[out2$quarter!=as.Date("2001-01-01"),]
out3 <- apply(out2[,5:10], 2, abs)

apply(out3, 2, function(x) sqrt(mean(x^2)))
library(stringr)

out3 %>% melt %>%
  mutate(Variable = str_extract(Var2, "[A-Za-z]*"),
         Type = str_extract(Var2, "[0-9]")) %>%
  ggplot(aes(x = Var1, y = value, group = Var2, colour = Type)) + 
  geom_line() + 
  facet_grid(Variable~.)


