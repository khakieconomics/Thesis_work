

# Read in libraries and data ----------------------------------------------
library(randomForest); library(shinystan);library(vars); library(zoo); library(rstan);  library(ggplot2); library(magrittr); library(lubridate); library(stringr); library(reshape2); library(parallel);  library(dplyr); 
cc <- function(x) x[complete.cases(x),]

CPI <- read.csv("./Data/CPIAUCSL.csv")
UNEMP <- read.csv("./Data/UNRATE.csv")
TREAS <- read.csv("./Data/TB3MS.csv")

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
dataset <- left_join(UNEMP, CPI) %>% left_join(TREAS) %>% cc

dataset.q <- dataset %>% group_by(quarter = as.yearqtr(Date) %>% as.Date) %>%
  summarise(Unemp = first(Unemp),
            CPI = first(CPI),
            r = mean(r)) %>%
  mutate(CPI = c(NA, diff(log(CPI))*100)) %>% cc %>% filter(quarter > "1960-01-01")



dat1 <- dataset.q[,-1]

dss <- dataset.q %>%
  mutate(Unemp.1 = lag(Unemp),
         Unemp.2 = lag(Unemp, 2),
         CPI.1 = lag(CPI),
         CPI.2 = lag(CPI, 2),
         r.1 = lag(r),
         r.2 = lag(r, 2)) %>% filter(!is.na(CPI.2))

rf1 <- randomForest(Unemp ~ Unemp.1 + Unemp.2 + CPI.1 + CPI.2 + r.1 + r.2, data = dss, proximity = T, ntree = 5000, nodesize = 40 , oob.prox = T)
rf2 <- randomForest(CPI ~ Unemp.1 + Unemp.2 + CPI.1 + CPI.2 + r.1 + r.2, data = dss, proximity = T, ntree = 5000, nodesize = 40, oob.prox = T)
rf3 <- randomForest(r ~ Unemp.1 + Unemp.2 + CPI.1 + CPI.2 + r.1 + r.2, data = dss, proximity = T, ntree = 5000, nodesize = 40, oob.prox = T)
proxmat <- (rf1$proximity + rf2$proximity + rf3$proximity)/3
proxmat[upper.tri(proxmat, diag  = T)] <- 0

weights <- data.frame(w = c(0,0,proxmat[nrow(proxmat),]), d = dataset.q$quarter)
proxmat <- as.data.frame(proxmat)
proxmat$Date <- dataset.q$quarter[-1:-2]
names(proxmat)[-length(names(proxmat))] <- paste0("X", dataset.q$quarter[-1:-2])

proxmat.m <- melt(proxmat, id = "Date")
proxmat.m$variable <- as.Date(gsub("X", "", proxmat.m$variable))

# Plot of analogies -------------------------------------------------------


png("analogy_matrix.png")
proxmat.m %>% ggplot(aes(x = Date, y = variable, fill = value)) + geom_tile() +
  scale_fill_gradient(low = "white", high = "red", "Analogy score\n") +
  theme_bw(base_size = 11) +
  xlab("Date") +
  ylab("Comparison Date") +
  scale_x_date(expand = c(0,0)) +
  scale_y_date(expand = c(0,0)) +
  coord_flip() +
  ggtitle("How good an analogy for 'Date'\nis 'Comparison Date'?\n")
dev.off()

# Plot of proportion of history analogous ---------------------------------
proxmat <- (rf1$proximity + rf2$proximity + rf3$proximity)/3
proxmat.d <- proxmat[,-ncol(proxmat)] %>% as.matrix
proxmat.d[lower.tri(proxmat.d, diag = T)] <- NA

prop.history <- apply(proxmat.d, 2, function(x) sum(x, na.rm = T)/sum(!is.na(x)))[-1]

prop.history <- data.frame(Date = dataset.q$quarter[-1:-4], prop.history)

png("relevant_histories.png")
prop.history %>% filter(Date>"1970-01-01") %>% ggplot(aes(x = Date, prop.history))+ 
  geom_line(size = 1) +
  theme_bw(base_size = 11) +
  ylab("Proportion") +
  scale_y_continuous(expand = c(0,0), limits = c(0,0.5)) +
  ggtitle("At each point in time, how much recorded economic\nhistory was relevant?")
dev.off()
# Analogies ---------------------------------------------------------------


sum(weights$w)

ggplot(weights, aes(x = d, y = w)) + geom_line() +
  xlab("Date") +
  ylab("Analogy score") +
  ggtitle("How similar are historical periods on\nthe dimensions that matter?\n") +
  theme_bw(base_size = 16)
# End ---------------------------------------------------------------------

vm.int <- VAR(dat1, p = 2, type = "const")
coef(vm.int)
int_prior <- do.call(rbind, lapply(coef(vm.int), function(x) x[7,1]))
firstlag <- do.call(rbind, lapply(coef(vm.int), function(x) x[1:3,1]))
secondlag <- do.call(rbind, lapply(coef(vm.int), function(x) x[4:6,1]))


# New rewrite -------------------------------------------------------------
P <- ncol(dat1); T <- nrow(dat1)

model.data2 <- list(Y = dat1, linear_est = as.vector(m4), 
                    linear_est_1 = as.vector(firstlag),
                    linear_est_2 = as.vector(secondlag),
                    weights = weights$w,
                    nu_var = 1.4,
                    P = P, T = T,
                    linear_int = linearint)

# hold on to your butts

tvp_mod <- stan(file = "analogy_weighted_var.cpp", data = model.data2, chains = 1, iter = 50)

tvp_mc <- mclapply(1:3, mc.cores = 3, function(i){
  stan(fit = tvp_mod, data = model.data2, chains =1, chain_id = i, verbose = TRUE, refresh = -1, 
       warmup = 200, iter = 400)
})


tvp_mc1 <- sflist2stanfit(tvp_mc)
launch_shinystan(tvp_mc1)

test <- e3[[1]]
shinystan::launch_shinystan(test)
yhat <- rstan::extract(tvp_mc1, pars = "Yhat", permuted= FALSE)
yhat <- do.call(rbind, lapply(1:3, function(i) yhat[,i,])) %>% as.data.frame

yhat.m <- yhat %>% melt %>%
  mutate(Time_period = str_extract(variable, perl("(?<=Yhat\\[)[0-9]{1,3}")) %>% as.numeric,
         Series = str_extract(variable, perl("(?<=[,])[0-9]{1,3}")) %>% as.numeric,
         Series = ifelse(Series==1, "Unemp", ifelse(Series==2, "CPI", "r"))) %>% left_join(dates)

yhat.m %<>% filter(between(value, -3, 17))
head(yhat.m)

yhat.s <- yhat.m %>% dplyr::group_by(Date = quarter, Series) %>%
  summarise(median = median(value, na.rm = T),
            Q.975 = quantile(value, 0.9, na.rm = T),
            Q.025 = quantile(value, 0.1, na.rm = T)) %>% ungroup

head(yhat.s)
dat2 <- dat1
dat2$Date <- dates$quarter 

dat2 %<>% melt(id = "Date") %>%
  rename(Series = variable)

yhat.s %>% 
  ggplot(aes(x  = Date, y= median)) + geom_line(aes(colour = Series %>% as.factor)) +
  geom_line(data = dat2 , aes(y = value), colour = "red") +
  geom_ribbon(aes(ymin = Q.025, ymax = Q.975), fill = "orange", alpha = 0.5) +
  facet_grid(Series ~., scale = "free_y")


# Questions ---------------------------------------------------------------


- What is "significance" of the weights. How much variation could we expect by chance alone?

