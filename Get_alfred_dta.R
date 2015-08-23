library("curl"); library(rlist);library(dplyr); library(XML); library(data.table); library(ggplot2)

api.key <- read.csv("fred_key.csv")[1,2] %>% as.character



# Define functions --------------------------------------------------------

# A function to download a particular FRED series in XML format
# We want to specify the realtime data--probably the last two quarters for 

fred_url <- function(series, api.key, vintage_date, series_start = "1960-01-01", units = "lin"){
  paste0("https://api.stlouisfed.org/fred/series/observations?series_id=", series, 
         "&api_key=", api.key, 
         "&vintage_dates=", vintage_date,
         "&output_type=2",
         "&observation_start=", series_start,
         "&units=", units)
}


# A function that translates the XML into a data_table
xml2dt <- function(xmlObj, vintage_date){
  require(pipeR)
  xmlApply(xmlRoot(xmlObj), xmlAttrs) %>>%
    list.map(. %>>% as.list %>>% as.data.table) %>%
    rbindlist(fill = TRUE) %>% as.data.frame ->
    dt
  names(dt) <- c("name","value")
  dt$vintage_date <- vintage_date
  dt <- mutate(dt, value = as.numeric(value),
               name = as.Date(name)) %>% filter(name<=vintage_date)
  return(dt)
}

#And a function that returns a data_table containing real-time data

gen_dt <- function(series, api.key = NULL, realtime_start = NULL, realtime_end = NULL,
                   vintage_date = NULL,
                   units = NULL,
                   diff = NULL,
                   series_start = NULL){
  print(vintage_date)
  # Establish the connection
  realtime_conn <- curl(fred_url(series, api.key = api.key, vintage_date = vintage_date, units = units))
  
  # Download the XML
  realtime_xml <- readLines(realtime_conn) %>% xmlTreeParse(
    useInternalNodes = TRUE
  )
  close(realtime_conn)
  # Convert to a data table
  realtime <- xml2dt(realtime_xml, vintage_date)
  realtime
}



# Now go and get some data ------------------------------------------------

# The starting and ending days of the months for which we want to grab data
dates <- data.frame(from = seq(from = as.Date("1975-01-01"), to = as.Date("2015-07-01"),by = "quarter"), to = seq(from = as.Date("1975-02-01"), to = as.Date("2015-08-01"),by = "quarter")-1)

# Lists of data series of various vintages
CPI <- lapply(1:nrow(dates), function(x) gen_dt(series = "CPIAUCSL", api.key = api.key, vintage_date = dates[x,1], series_start = "1960-01-01", units = "pc1"))
UNEMP <- lapply(1:nrow(dates), function(x) gen_dt(series = "UNRATE", api.key = api.key, vintage_date = dates[x,1], series_start = "1960-01-01", units = "lin"))
POLICYRATE <- lapply(1, function(x) gen_dt(series = "TB3MS", api.key = api.key, vintage_date = "2015-08-23", series_start = "1960-01-01", units = "lin"))
CPI[[163]]
# These take a while to download, so save them
save(list = c("CPI", "UNEMP", "POLICYRATE"), file = "vintagedata.RData")


# Now a function required to convert each into full data ------------------
# The reason is that ALFRED only returns the observations for which the archived version of the data contains different values to the current version

vintagiser <- function(archive_list){
  require(dplyr)
  lastseries <- archive_list[[length(archive_list)]]
  f <- function(x){
    maxdate <- max(x$name)
    x <- (lastseries %>% select(name, value)) %>% filter(name<=maxdate) %>% left_join(x %>% rename(archive_value = value)) %>% 
      mutate(archive_value = ifelse(is.na(archive_value), value, archive_value),
             vintage_date = first(vintage_date[!is.na(vintage_date)])) %>% select(-value)
    x
  }
  lapply(archive_list, f)
}
# 
 CPI2 <- vintagiser(CPI)
 UNEMP2 <- vintagiser(UNEMP)

CPI2[[1]]

CPI3 <- do.call(rbind, CPI2)
UNEMP3 <- do.call(rbind, UNEMP2)


rolling_revisions <- function(x) {
  out <- NULL
  for(i in 13:length(x)){
    out[i] <- sum(x[(i-12):i])
  }
  out
}
head(UNEMP3)
UNEMP3 %>% group_by(name) %>% arrange(vintage_date) %>%
  summarise(revision = last(archive_value) - first(archive_value)) %>% 
  mutate(rolling_revision= rolling_revisions(revision)) %>% 
  ggplot(aes(x = name, y = rolling_revision)) + geom_line(colour = "orange", size = 2, alpha = 0.9) +
  theme_bw(base_size = 16) +
  xlab("Date") +
  ylab("Per cent") +
  ggtitle("12 month rolling revisions to US unemployment\n")


CPI3 %>%group_by(name) %>% arrange(vintage_date) %>%
  summarise(revision = last(archive_value) - first(archive_value)) %>% 
  mutate(rolling_revision= rolling_revisions(revision)) %>% filter(name > "1975-01-01") %>%
  ggplot(aes(x = name, y = rolling_revision)) + geom_line(colour = "orange", size = 2, alpha = 0.9) +
  theme_bw(base_size = 16) +
  xlab("Date") +
  ylab("Per cent") +
  ggtitle("12 month rolling revisions to US CPI\n")



# Build the dataset -------------------------------------------------------



CPI3 <- CPI3 %>% rename(CPI = archive_value)
UNEMP3 <- UNEMP3 %>% rename(U = archive_value)
R <- POLICYRATE[[1]] %>% select(-vintage_date) %>%
  rename(r = value)

vardat <- CPI3 %>% left_join(UNEMP3) %>% left_join(R) %>%
  rename(Date = name)



