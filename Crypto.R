nullToNA <- function(x) {
  x[sapply(x, is.null)] <- NA
  return(x)
}
is_date <- function(date, format) {
  formatted = try(as.Date(date, format), silent = TRUE)
  return(DIZutils::equals2(as.character(formatted), date))
}
TimestamptoDate <- function(x) {
  #x[sapply(x, !(is.date))] <- (as.POSIXct(as.numeric(as.character(x)), origin="1970-01-01", tz="GMT"))
  x[sapply(x,is_date(x,"%d-%m-%Y"))] <- (as.POSIXct(as.numeric(as.character(x)), origin="1970-01-01", tz="GMT"))
  return(x)
  #head(as.POSIXct(as.numeric(as.character(try$time)), origin="1970-01-01", tz="GMT"))
}
setwd("C:/Users/Ewen/Downloads/maths R/crypto")
getwd()
mkt_data <- read.csv("bitstampUSD_1-min_data_2012-01-01_to_2021-03-31.csv")
str(mkt_data)
timestamp <- mkt_data$Timestamp
head(timestamp)
# Timestamp to date
#mkt_data2$Date <- as.Date(as.POSIXct(timestamp, origin=“1970-01-01”))
mkt_data <- lapply(mkt_data,nullToNA)
mkt_data$Date <- seq(lapply(mkt_data$Timestamp,TimestamptoDate))
mkt_data <- do.call(rbind,lapply(mkt_dat,data.frame,stringsAsFactors=FALSE))
#head(mkt_data)
head(mkt_data$Timestamp)
head(mkt_data$Date)
