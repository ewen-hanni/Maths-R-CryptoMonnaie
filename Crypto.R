nullToNA <- function(x) {
  x[sapply(x, is.null)] <- NA
  return(x)
}
TimestamptoDate <- function(x) {
  x[sapply(x, !(is.Date))] <- (as.POSIXct(as.numeric(as.character(x)), origin="1970-01-01", tz="GMT"))
  return(x)
  #head(as.POSIXct(as.numeric(as.character(try$time)), origin="1970-01-01", tz="GMT"))
}
setwd("C:/Users/Ewen/Downloads/maths R/crypto")
getwd()
mkt_data <- read.csv("bitstampUSD_1-min_data_2012-01-01_to_2021-03-31.csv")
str(mkt_data)
timestamp <- mkt_df$Timestamp
head(timestamp)
# Timestamp to date
#mkt_data2$Date <- as.Date(as.POSIXct(timestamp, origin=“1970-01-01”))
mkt_data2 <- lapply(mkt_data,nullToNA)
mkt_data3 <- lapply(mkt_data2,TimestamptoDate)
mkt_df <- do.call(rbind,lapply(mkt_data3,data.frame,stringsAsFactors=FALSE))
head(mkt_df)