library("curl"); library(rlist);library(dplyr); library(XML); library(data.table); library(ggplot2)

api.key <- read.csv("fred_key.csv")[1,2] %>% as.character



# Define functions --------------------------------------------------------

# A function to download a particular FRED series in XML format
# We want to specify the realtime data--probably the last two quarters for 

fred_url <- function(series, api.key, realtime_start, realtime_end, series_start = "1960-01-01"){
  paste0("https://api.stlouisfed.org/fred/series/observations?series_id=", series, 
         "&api_key=", api.key, 
         "&realtime_start=", realtime_start, 
         "&realtime_end=", realtime_end,
         "&observation_start=", series_start,
         "&observation_end=", realtime_end)
}

# A function that translates the XML into a data_table
xml2dt <- function(xmlObj){
  require(pipeR)
  xmlApply(xmlRoot(xmlObj), xmlAttrs) %>>%
    list.map(. %>>% as.list %>>% as.data.table) %>%
    rbindlist(fill = TRUE) ->
    dt
  dt <- dt %>% mutate(value = as.numeric(value),
                      date = as.Date(date),
                      realtime_start = as.Date(realtime_start),
                      realtime_end = as.Date(realtime_end))
  return(dt)
}


dloggy <- function(x, mu, sigma){
  exp(-((log(x) - mu)^2)/2*sigma^2)/(x*sigma*sqrt(2*pi))
}

dloggy(x, 4, 5)

fitdistr(x = x, densfun = dloggy, start = list(mu =5, sigma = 1.3))

#And a function that returns a data_table containing real-time data

gen_dt <- function(series, api.key = NULL, realtime_start = NULL, realtime_end = NULL,
                   series_start = NULL,
                   series_end = NULL,
                   diff = NULL){
  print(realtime_end)
  browser()
  if(!is.null(diff)){
    realtime_conn <- curl(paste0(fred_url(series, api.key = api.key, 
                                   realtime_start = realtime_start, 
                                   realtime_end = realtime_end, 
                                   series_start = series_start),"&units=pc1") )
    
  } else {
    # Establish the connection
    realtime_conn <- curl(fred_url(series, api.key = api.key, 
                                   realtime_start = realtime_start, 
                                   realtime_end = realtime_end, 
                                   series_start = series_start))
    
  }
  
  # Download the XML
  realtime_xml <- readLines(realtime_conn) %>% xmlTreeParse(
    useInternalNodes = TRUE
  )
  close(realtime_conn)
  # Convert to a data table
  realtime <- xml2dt(realtime_xml)
  realtime
}



# Now go and get some data ------------------------------------------------

# The starting and ending days of the months for which we want to grab data
dates <- data.frame(from = seq(from = as.Date("1975-01-01"), to = as.Date("2015-07-01"),by = "quarter"), to = seq(from = as.Date("1975-02-01"), to = as.Date("2015-08-01"),by = "quarter")-1)

# Lists of data series of various vintages
CPI <- lapply(1:nrow(dates), function(x) gen_dt(series = "CPIAUCSL", api.key = api.key, realtime_start = dates[x,1], realtime_end = dates[x,2], series_start = "1960-01-01", series_end = dates[x,2], diff = T))
UNEMP <- lapply(1:nrow(dates), function(x) gen_dt(series = "UNRATE", api.key = api.key, realtime_start = dates[x,1], realtime_end = dates[x,2], series_start = "1960-01-01", series_end = dates[x,2]))
POLICYRATE <- lapply(1, function(x) gen_dt(series = "TB3MS", api.key = api.key, realtime_start = "2015-08-22", realtime_end = "2015-08-22", series_start = "1960-01-01", series_end = "2015-08-22"))

# These take a while to download, so save them

save(list = c("CPI", "UNEMP", "POLICYRATE"), file = "vintagedata.RData")


# Now a function required to convert each into full data ------------------
# The reason is that ALFRED only returns the observations for which the archived version of the data contains different values to the current version

vintagiser <- function(archive_list){
  require(dplyr)
  lastseries <- archive_list[[length(archive_list)]]
  f <- function(x){
    maxdate <- max(x$date)
    x <- (lastseries %>% select(date, value)) %>% filter(date<=maxdate) %>% left_join(x %>% rename(archive_value = value)) %>% 
      mutate(archive_value = ifelse(is.na(archive_value), value, archive_value),
             realtime_start = first(realtime_start[!is.na(realtime_start)]),
             realtime_end = first(realtime_end[!is.na(realtime_end)])) %>% select(-value)
    x
  }
  lapply(archive_list, f)
}

CPI2 <- vintagiser(CPI)
UNEMP2 <- vintagiser(UNEMP)

CPI3 <- do.call(rbind, CPI2)
UNEMP3 <- do.call(rbind, UNEMP2)

rolling_revisions <- function(x) {
  out <- NULL
  for(i in 13:length(x)){
    out[i] <- sum(x[(i-12):i])
  }
  out
}
UNEMP3 %>% group_by(date) %>% arrange(realtime_start) %>%
  summarise(revision = last(archive_value) - first(archive_value)) %>% filter(date>"1970-01-01") %>%
  mutate(rolling_revision= rolling_revisions(revision)) %>%
  ggplot(aes(x = date, y = rolling_revision)) + geom_line(colour = "orange", size = 2, alpha = 0.9) +
  theme_bw(base_size = 16) +
  xlab("Date") +
  ylab("Per cent") +
  ggtitle("12 month rolling revisions to US unemployment\n")

CPI3 %>% group_by(date) %>% arrange(realtime_start) %>%
  summarise(revision = last(archive_value) - first(archive_value)) %>% filter(date>"1970-01-01") %>%
  mutate(rolling_revision= rolling_revisions(revision)) %>%
  ggplot(aes(x = date, y = rolling_revision)) + geom_line(colour = "orange", size = 2, alpha = 0.9) +
  theme_bw(base_size = 16) +
  xlab("Date") +
  ylab("Per cent") +
  ggtitle("12 month rolling revisions to US CPI\n")

CPI3 %>% ggplot(aes(x = date, y = archive_value)) + geom_point()
CPI4 <- CPI3 %>% 
  mutate(keep = ifelse(date<"1990-01-01" & archive_value>150, F, ifelse(date<"1980-01-01" & archive_value>100, F, T))) %>% filter(keep)

CPI4 %>% group_by(date) %>% arrange(realtime_start) %>%
  summarise(revision = last(archive_value) - first(archive_value)) %>% filter(date>"1970-01-01") %>%
  mutate(rolling_revision= rolling_revisions(revision)) %>%
  ggplot(aes(x = date, y = rolling_revision)) + geom_line(colour = "orange", size = 2, alpha = 0.9) +
  theme_bw(base_size = 16) +
  xlab("Date") +
  ylab("Per cent") +
  ggtitle("12 month rolling revisions to US CPI\n")