# Read in libraries and data ----------------------------------------------
library(shinystan);library(vars); library(zoo); library(rstan);  library(ggplot2); library(magrittr); library(lubridate); library(stringr); library(reshape2); library(parallel);  library(dplyr);
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

dataset.q <- dataset %>% group_by(quarter = as.yearqtr(Date) %>% as.Date) %>%
  summarise(Unemp = first(Unemp),
            CPI = first(CPI),
            r = mean(r)) %>%
  mutate(CPI = c(NA, diff(log(CPI))*100)) %>% cc %>% filter(quarter > "1960-01-01")

dat1 <- dataset.q[,-1]

VAR(dat1, p = 2)
stanlist <- list(T = nrow(dat1), P = ncol(dat1), Y = dat1, 
                 lkj_nu_resid = 1.5, lkj_nu_cov = 10, 
                 intercept_start = c(.2, .1, .1),
                 theta1_start = diag(3),
                 theta2_start = matrix(0, 3, 3))


stanfit1 <- stan(file = "TVP_BVAR_20151023.stan", data = stanlist, chains = 1, iter = 30)

#stanfit2 <- stan(fit = stanfit1, data = stanlist, chains = 3, iter = 300, cores = 4)

load("tvp_var_estimates_20151022.RData")

launch_shinystan(stanfit2)


tvp_pars <- rstan::extract(stanfit2, pars = "yhat_lr", permuted = F)
dim(tvp_pars)
test <- plyr::adply(tvp_pars, 2)[,-1]
dat1 %>% head
# Long run estimates
297000/3

qq <- dataset.q$quarter
dat1m <- dat1 %>% mutate(obs = 1:n(),Date = qq) %>% melt(id = c("obs", "Date")) %>% rename(actual = value)
head(dat1m)


# Money plot --------------------------------------------------------------



test %>% melt() %>% 
  mutate(obs = str_extract(variable, perl("(?<=\\[)[0-9]{1,3}")) %>% as.numeric,
         variable = str_extract(variable, perl("(?<=[,])[0-9]{1,3}")) %>% as.numeric,
         variable = ifelse(variable==1, "Unemp", ifelse(variable==2, "CPI", "r"))) %>% cc %>%
  group_by(variable, obs) %>%
  summarise(median = median(value),
            lower = quantile(value, 0.25),
            upper = quantile(value, 0.75)) %>% left_join(dat1m) %>%
  ggplot(aes(x = Date)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "orange", alpha = 0.5) +
  geom_line(aes(y  = median)) +
  geom_line(aes(y = actual), colour = "red") +
  facet_grid(variable~., scales = "free_y") +
  ggthemes::theme_fivethirtyeight() +
  ggtitle("Actual (red) and estimates of permanent\ncomponents")


perm_components_out<-test %>% melt() %>% 
  mutate(obs = str_extract(variable, perl("(?<=\\[)[0-9]{1,3}")) %>% as.numeric,
         variable = str_extract(variable, perl("(?<=[,])[0-9]{1,3}")) %>% as.numeric,
         variable = ifelse(variable==1, "Unemp", ifelse(variable==2, "CPI", "r"))) %>% cc %>%
  group_by(variable, obs) %>%
  summarise(median = median(value),
            lower = quantile(value, 0.25),
            upper = quantile(value, 0.75)) %>% left_join(dat1m)

save(perm_components_out, file = "perm_components_tvp.RData")



# Predictive performance --------------------------------------------------


one_year_forecasts <- rstan::extract(stanfit2, pars = "Yhat", permuted = F)
one_year_forecasts <- plyr::adply(one_year_forecasts, 2)[,-1]

test_forecast <- one_year_forecasts %>% melt() %>% 
  mutate(obs = str_extract(variable, perl("(?<=\\[)[0-9]{1,3}")) %>% as.numeric +4,
         variable = str_extract(variable, perl("(?<=[,])[0-9]{1,3}")) %>% as.numeric,
         variable = ifelse(variable==1, "Unemp", ifelse(variable==2, "CPI", "r"))) %>% cc %>%
  group_by(variable, obs) %>%
  summarise(median = median(value),
            lower = quantile(value, 0.25),
            upper = quantile(value, 0.75)) %>% left_join(dat1m)

save(test_forecast, file = "tvp_forecast_1y.RData")

test_forecast %>% cc %>%group_by(variable) %>%
  summarise(rmse = sqrt(mean((median - actual)^2)))

test_forecast %>% ggplot(aes(x= median, y = actual)) + 
  geom_point() +
  facet_grid(.~variable) +
  geom_smooth(method = "lm") +
  xlab("One year ahead forecast") +
  ylab("Actual")



# Now let’s run the weighted model ----------------------------------------


