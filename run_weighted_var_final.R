library(stringr); library(shinystan);library(vars); library(zoo); library(rstan);  library(ggplot2); library(magrittr); library(lubridate); library(stringr); library(reshape2); library(parallel);  library(dplyr); library(randomForest)
cc <- function(x) x[complete.cases(x),]

CPI <- read.csv("./Data/CPIAUCSL.csv")
UNEMP <- read.csv("./Data/UNRATE.csv")
TREAS <- read.csv("./Data/TB3MS.csv")
head(UNEMP)

converter <- function(x, value){
  x$Date <- as.Date(x$DATE)
  x$DATE <- NULL
  x[,value] <- x$VALUE
  x$VALUE <- NULL
  x
}

CPI %<>% converter("CPI")
UNEMP %<>% converter("Unemp")
TREAS %<>% converter("r")
dataset <- left_join(UNEMP, CPI) %>% left_join(TREAS)

dataset.q1 <- dataset %>% group_by(quarter = as.yearqtr(Date) %>% as.Date) %>%
  summarise(Unemp = first(Unemp),
            CPI = first(CPI),
            r = mean(r)) %>%
  mutate(CPI = c(NA, diff(log(CPI))*100)) %>% cc %>% filter(quarter > "1960-01-01")

dat1 <- dataset.q1[,-1]


# Dates that will be the final observation for each dataset
enddates <- seq(from = as.Date("1975-01-01"), to = as.Date("2014-01-01"), by = "year")

out <- data.frame(quarter = enddates) %>% left_join(dataset.q1)
out2 <- data.frame(quarter = enddates, matrix(NA, length(enddates), 6))
names(out2)[-1] <- paste0(names(dataset.q)[-1], rep(1:2, each = 3))
out <- left_join(out, out2)
out_lr <- out2

#load("all_sims.RData")
e2 <- new.env()

for(ii in 1:(length(enddates)-1)){
  
  kk <- enddates[ii]
  print(kk)
  kk1 <- enddates[ii+1]
  #   
  #   # Subset the dataset and create a "quarterly" version
    dataset.q <- dataset.q1%>% filter(quarter<=kk & quarter > "1960-01-01") %>%
      as.data.frame

    # Remove the date column
    dat1 <- dataset.q[,-1]

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
      tvp_mod <- stan(file = "analogy_weighted_final.stan", data = model.data2, chains = 1, iter = 50)
    }

  tvp_mc1 <- stan(fit = tvp_mod, data = model.data2, chains =3, warmup = 200, iter = 400, cores =4)

  # Save the results in an environment
  assign(paste0("Sim",kk),tvp_mc1, envir = e2)
  #launch_shinystan(tvp_mc1)
  #tvp_mc1 <- e3[[paste0("Sim",kk)]]
  
  # Extract the fitted series of one-year-ahead forecasts
  yhat <- rstan::extract(tvp_mc1, pars = "Yhat", permuted= FALSE)
  yhat2 <- rstan::extract(tvp_mc1, pars = "Yhat2", permuted= FALSE)
  yhat_lr <- rstan::extract(tvp_mc1, pars = "yhat_lr", permuted= FALSE)
  yhat_lr2 <- rstan::extract(tvp_mc1, pars = "yhat_lr2", permuted= FALSE)
  
  yhat <- do.call(rbind, lapply(1:3, function(i) yhat[,i,])) %>% as.data.frame
  yhat2 <- do.call(rbind, lapply(1:3, function(i) yhat2[,i,])) %>% as.data.frame
  
  yhat_lr <- do.call(rbind, lapply(1:3, function(i) yhat_lr[,i,])) %>% as.data.frame
  yhat_lr2 <- do.call(rbind, lapply(1:3, function(i) yhat_lr2[,i,])) %>% as.data.frame
  
  
  yhat.mean <- apply(yhat, 2, median)
  names(yhat.mean) <- NULL
  yhat2.mean  <- apply(yhat2, 2, median)
  names(yhat2.mean) <- NULL
  yhat_lr.mean <- apply(yhat_lr, 2, median)
  names(yhat_lr.mean) <- NULL
  yhat_lr2.mean  <- apply(yhat_lr2, 2, median)
  names(yhat_lr2.mean) <- NULL
  
  out[out$quarter==kk1,5:7] <- yhat.mean
  out[out$quarter==kk1,8:10] <- yhat2.mean
  
  out_lr[out_lr$quarter==kk1,2:4] <- yhat.mean
  out_lr[out_lr$quarter==kk1,5:7] <- yhat2.mean
}

e3 <- as.list(e2)
save(e3, file = "all_sims_final.RData")


out2 <- out
out2[,5:7] <- out2[,5:7]- out[,2:4]
out2[,8:10] <- out2[,8:10]- out[,2:4]

plot.ts(data.frame(out2$CPI1, out2$CPI2))


rmse <- apply(out2[,5:10], 2, function(x) sqrt(mean(x^2, na.rm = T)))
rmse[1:3]/rmse[4:6]

out3 <- apply(out2[,5:10], 2, abs) %>% as.data.frame

out3 <- apply(out3[-1,], 2, cumsum) %>% as.data.frame
out3$Date <- out$quarter[-1]

out3 %>% cc%>% melt(id = "Date") %>%#head
  mutate(Variable = str_extract(variable, "[A-Za-z]*"),
         Type = str_extract(variable, "[0-9]")) %>% 
  ggplot(aes(x = Date, y = value, group = variable, colour = Type)) + 
  geom_line() + 
  facet_grid(Variable~., scales = "free_y")


out3 %>% melt(id = "Date") %>%#head
  mutate(Variable = str_extract(variable, "[A-Za-z]*"),
         Type = str_extract(variable, "[0-9]")) %>% group_by(Date, Variable) %>%
  summarise(Relative_error = value[Type=="1"] - value[Type=="2"]) %>%
  ggplot(aes(x = Date, y = Relative_error)) + 
  geom_bar(stat = "identity", fill = "orange", alpha = 0.7) + 
  facet_grid(Variable~., scales = "free_y") +
  theme_bw(base_size = 16) +
  ylab("1-year-ahead forecast error relative\nto benchmark model\nper cent") +
  ggtitle("If you train forecasting models on\nrelevant data only, you get better forecasts\n")



out3 %>% melt(id = "Date") %>%#head
  mutate(Variable = str_extract(variable, "[A-Za-z]*"),
         Type = str_extract(variable, "[0-9]")) %>% group_by(Date, Variable) %>%
  summarise(Relative_error = value[Type=="1"] - value[Type=="2"]) %>% filter(!is.na(Relative_error)) %>%
  group_by(Variable) %>% arrange(Date) %>%
  mutate(Cumulative_improvement = cumsum(Relative_error)) %>%
  ggplot(aes(x = Date, y = -Cumulative_improvement)) + 
  geom_bar(stat = "identity", fill = "orange", alpha = 0.7) + 
  facet_grid(Variable~., scales = "free_y") +
  theme_bw(base_size = 11) +
  ylab("Cumulative 1-year-ahead forecast error of benchmark model\nrelative to proxmatch model\nper cent") +
  ggtitle("If you train forecasting models on\nrelevant data only, you get better forecasts\n")


# Permanent components ----------------------------------------------------

load("perm_components_tvp.RData")

tvp_perm<- perm_components_out %>% select(Date, variable, median)%>% cc %>% as.data.frame %>%
  dcast(Date ~ variable, value.var = "median") %>%
  rename(quarter = Date)
names(tvp_perm)[-1] <- paste0(names(tvp_perm)[-1], "3")

out_lr %>% left_join(dataset.q1) %>% left_join(tvp_perm)%>% melt(id = "quarter") %>%
  mutate(est = str_extract(variable, "[0-9]"),
         series = str_extract(variable, "[A-Za-z]*"),
         est = ifelse(is.na(est), "actual", est)) %>%
  ggplot(aes(x = quarter, y = value, colour= est)) +
  geom_line() +
  facet_grid(series~.)
  
out_mini <- out_lr %>% left_join(dataset.q1) %>% left_join(tvp_perm) %>%
  select(r, CPI1, CPI3) %>% cc
cor(out_mini)

summary(lm(r ~ CPI1 + CPI3 , data = out_mini))

