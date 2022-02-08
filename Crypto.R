# On Mac :
# Be sure to execute first the following brew CLI command
# brew install openssl

library(tidyverse)
library(lubridate)
library(fpp2)
library(astsa)
library(plotly)

os <- .Platform$OS.type

wd_path = getwd()
sprintf("wd = %s", getwd())

# Do all OS affected operations here
if(os == "unix") {
  # unix like OS
  print("Using an unix like OS")

} else if (os == "windows") {
  # windows OS
  print("Using windows")
  windowsFonts(Garamond = windowsFont("Garamond"))
  
} else {
  print("Not a supported OS")
}

#------------------------------------------------------------------------------#

# Change all NULL values to NA
setNullToNA <- function(x) {
  x[sapply(x, 0)] <- NA
  return(x)
}

# Check if it's in date format (???)
is_date <- function(date) {
  formatted = try(as.Date(date, "%d-%m-%Y"), silent = TRUE)
  return(DIZutils::equals2(as.character(formatted), date))
}

#------------------------------------------------------------------------------#

data <- read.csv(file = "crypto-markets.csv")
#data <- lapply(data, setNullToNA)
data[data == 0] <- NA

#------------------------------------------------------------------------------#

# Get all the data from 2016-01-01 to 2018-01-01
get2016to2018Data <- function() {
  df <- data[data$date > "2016-01-01" & data$date < "2018-01-01",]
  return(df)
}

get2016to2018NonBTCData <- function() {
  df <- get2016to2018Data()
  nonBTCData = df[df$name != "Bitcoin",]
  return(nonBTCData)
}

get2016to2018BTCData <- function() {
  df <- get2016to2018Data()
  btcData <- df[df$name == "Bitcoin",]
  return(btcData)
}

#------------------------------------------------------------------------------#

get2018Data <- function() {
  df <- data[data$date > "2018-01-01" & data$date < "2019-01-01",]
  return(df)
}

get2018BTCData <- function() {
  df <- get2018Data()
  btcData <- df[df$name == "Bitcoin",]
  return(btcData)
}

get2018NonBTCData <- function() {
  df <- get2018Data()
  nonBTCData = df[df$name != "Bitcoin",]
  return(nonBTCData)
}

#------------------------------------------------------------------------------#
                              # 2016 to 2018
# Average minimum value of BTC during the 2016 to 2018 period
print("Avg min BTC value")
df <- get2016to2018BTCData()
minAvgBTCValue1 <- mean(df$low, na.rm=T)
minAvgBTCValue1

# Average maximum value of BTC during the 2016 to 2018 period
print("Avg max BTC value")
df <- get2016to2018BTCData()
maxAvgBTCValue1 <- mean(df$high, na.rm=T)
maxAvgBTCValue1

# Average BTC volume traded during 2016 to 2018 period
print("Average BTC traded volume")
df <- get2016to2018BTCData()
avgBTCVol1 <- mean(df$volume, na.rm=T)
avgBTCVol1

# Sum of all volume traded including BTC and the rest during 2016 to 2018 period
print("Sum of all volume traded")
df <- get2016to2018Data()
tradedAllVol1 = sum(df$volume, na.rm=T)
tradedAllVol1

# Sum of all BTC volume traded during 2016 to 2018 period
print("Sum of BTC volume traded")
df <- get2016to2018BTCData()
tradedBTCVol1 <- sum(df$volume, na.rm=T)
tradedBTCVol1

# Sum of all non BTC volume traded during 2016-2018 period
print("Sum of non BTC volume traded")
df <- get2016to2018NonBTCData()
tradedNonBTCVol1 <- sum(df$volume, na.rm=T)
tradedNonBTCVol1

# Percentage of BTC volume traded on all volume traded during 2016 to 2018 period
print("Percentage of BTC volume traded on all volume")
percentageBTC1 <- tradedBTCVol1/tradedAllVol1
percentageBTC1

# Percentage of non BTC volume traded on all volume traded during 2016 to 2018 period
print("Percentage of non BTC volume traded on all volume")
percentageNonBTC1 <- tradedNonBTCVol1/tradedAllVol1
percentageNonBTC1

#------------------------------------------------------------------------------#
                              # 2018 ONLY

# Average minimum value of BTC during the 2018 to 2019 period
print("Avg min BTC value")
df <- get2018BTCData()
minAvgBTCValue2 <- mean(df$low, na.rm=T)
minAvgBTCValue2

# Average maximum value of BTC during the 2018 to 2019 period
print("Avg max BTC value")
df <- get2018BTCData()
maxAvgBTCValue2 <- mean(df$high, na.rm=T)
maxAvgBTCValue2

# Average BTC volume traded during the 2018 to 2019 period
print("Average BTC traded volume")
df <- get2018BTCData()
avgBTCVol2 <- mean(df$volume, na.rm=T)
avgBTCVol2

# Sum of all volume traded including BTC and the rest during 2018 to 2019 period
print("Sum of all volume traded")
df <- get2018Data()
tradedAllVol2 = sum(df$volume, na.rm=T)
tradedAllVol2

# Sum of all BTC volume traded during 2018 to 2019 period
print("Sum of BTC volume traded")
df <- get2018BTCData()
tradedBTCVol2 <- sum(df$volume, na.rm=T)
tradedBTCVol2

# Sum of all non BTC volume traded during 2018-2019 period
print("Sum of non BTC volume traded")
df <- get2018NonBTCData()
tradedNonBTCVol2 <- sum(df$volume, na.rm=T)
tradedNonBTCVol2

# Percentage of BTC volume traded on all volume traded during 2018 to 2019 period
print("Percentage of BTC volume traded on all volume")
percentageBTC2 <- tradedBTCVol2/tradedAllVol2
percentageBTC2

# Percentage of non BTC volume traded on all volume traded during 2016 to 2018 period
print("Percentage of non BTC volume traded on all volume")
percentageNonBTC2 <- tradedNonBTCVol2/tradedAllVol2
percentageNonBTC2

#------------------------------------------------------------------------------#

# Plot 1
x = c(minAvgBTCValue1,maxAvgBTCValue1) 
y = c(minAvgBTCValue2,maxAvgBTCValue2) 
x;y
type = c("Avg min BTC value","Avg max BTC value") 
moyennes = c(x,y) 
moyennes = matrix(moyennes,nc=2, nr=2, byrow=T) # nc : nombre de tests - nr : nombre de barres accolées (ici par paire) 
colnames(moyennes) = type 
barplot(moyennes,beside=T) ; box() 

