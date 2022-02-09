# On Mac :
# Be sure to execute first the following brew CLI command
# brew install openssl

library(tidyverse)
library(lubridate)
library(fpp2)
library(astsa)
library(plotly)

os <- .Platform$OS.type
setwd("C:/Users/ewen/Documents/GitHub/Maths-R-CryptoMonnaie")
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

# Change all NA values to 0
setNAtoZero <- function(x) {
  x[sapply(x, is.na)] <- 0
  return(x)
}

#------------------------------------------------------------------------------#

data <- read.csv(file = "crypto-markets.csv")
mkt_data <- read.csv(file = "crypto-markets.csv")

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


# Get all the data from 2013-01-01 to 2018-01-01
get2013to2018Data <- function() {
  df2 <- data[data$date > "2013-01-01" & data$date < "2018-01-01",]
  return(df2)
}

get2013to2018NonBTCData <- function() {
  df2 <- get2013to2018Data()
  nonBTCData2 = df2[df2$name != "Bitcoin",]
  return(nonBTCData2)
}

get2013to2018BTCData <- function() {
  df2 <- get2013to2018Data()
  btcData2 <- df2[df2$name == "Bitcoin",]
  return(btcData2)
}
#------------------------------------------------------------------------------#
# 2013 to 2018
# Average minimum value of BTC during the 2013 to 2018 period
print("Avg min BTC value")
df2 <- get2013to2018BTCData()
minAvgBTCValue3 <- mean(df2$low, na.rm=T)
minAvgBTCValue3

# Average maximum value of BTC during the 2013 to 2018 period
print("Avg max BTC value")
df2 <- get2013to2018BTCData()
maxAvgBTCValue3 <- mean(df2$high, na.rm=T)
maxAvgBTCValue3


# Average BTC volume traded during 2013 to 2018 period
print("Average BTC traded volume")
df2 <- get2013to2018BTCData()
df2$volume<- setNAtoZero(df2$volume)
avgBTCVol3 <- mean(df2$volume)
avgBTCVol3

# Sum of all volume traded including BTC and the rest during 2013 to 2018 period
print("Sum of all volume traded")
df2 <- get2013to2018Data()
df2$volume<- setNAtoZero(df2$volume)
tradedAllVol3 = sum(df2$volume, na.rm=T)
tradedAllVol3

# Sum of all BTC volume traded during 2013 to 2018 period
print("Sum of BTC volume traded")
df2 <- get2013to2018BTCData()
df2$volume<- setNAtoZero(df2$volume)
tradedBTCVol3 <- sum(df2$volume)
tradedBTCVol3

# Sum of all non BTC volume traded during 2013-2018 period
print("Sum of non BTC volume traded")
df2 <- get2013to2018NonBTCData()
df2$volume<- setNAtoZero(df2$volume)
tradedNonBTCVol3 <- sum(df2$volume, na.rm=T)
tradedNonBTCVol3

# Percentage of BTC volume traded on all volume traded during 2013 to 2018 period
print("Percentage of BTC volume traded on all volume")
percentageBTC3 <- tradedBTCVol/tradedAllVol
percentageBTC3

# Percentage of non BTC volume traded on all volume traded during 2013 to 2018 period
print("Percentage of non BTC volume traded on all volume")
percentageNonBTC3 <- tradedNonBTCVol/tradedAllVol
percentageNonBTC3
#------------------------------------------------------------------------------#



#moyennes
head(mean( mkt_data$open))
head(mean( mkt_data$high))
head(mean( mkt_data$low))
head(mean( mkt_data$volume))

#variance
head(var( mkt_data$open))
head(var( mkt_data$high))
head(var( mkt_data$low))
head(var( mkt_data$volume))



#------------------------------------------------------------------------------#

# Plot 1
x = c(minAvgBTCValue1,maxAvgBTCValue1) 
y = c(minAvgBTCValue2,maxAvgBTCValue2) 
x;y
type = c("Avg min BTC value (2016-2018)","Avg max BTC value (2018-2019)") 
moyennes = c(x,y) 
# nc : nombre de tests
# nr : nombre de barres accolées
moyennes = matrix(moyennes,nc=2, nr=2, byrow=T)
colnames(moyennes) = type 
barplot(moyennes,beside=T) ; box() 

# Plot 2
x = c(avgBTCVol1) 
y = c(avgBTCVol2) 
x;y
type = c("Avg BTC Volume Traded (2016-2018)","Avg BTC Volume Traded (2018-2019)") 
moyennes = c(x,y) 
# nc : nombre de tests
# nr : nombre de barres accolées
moyennes = matrix(moyennes,nc=1, nr=2, byrow=T)
colnames(moyennes) = type 
barplot(moyennes,beside=T) ; box() 

# Plot 3
x = c(percentageBTC1) ; y = c(percentageNonBTC1)
type = c("% of BTC vs Others (2016-2018)") 
moyennes = c(x,y) 
# nc : nombre de tests
# nr : nombre de barres accolées
moyennes = matrix(moyennes,nc=1, nr=2, byrow=T)
colnames(moyennes) = type 
barplot(moyennes,beside=F,col=c("orange","yellow"),ylim=c(0,1)) ; box()

# Plot 4
x = c(percentageBTC2) ; y = c(percentageNonBTC2)
type = c("% of BTC vs Others (2018-2019)") 
moyennes = c(x,y) 
# nc : nombre de tests
# nr : nombre de barres accolées
moyennes = matrix(moyennes,nc=1, nr=2, byrow=T)
colnames(moyennes) = type 
barplot(moyennes,beside=F,col=c("orange","yellow"),ylim=c(0,1)) ; box()

# Plot 5
x = c(minAvgBTCValue1,maxAvgBTCValue1) 
y = c(minAvgBTCValue3,maxAvgBTCValue3) 
x;y
type = c("Avg min BTC value (2013-2018)","Avg max BTC value (2018-2019)") 
moyennes = c(x,y) 
# nc : nombre de tests
# nr : nombre de barres accolées
moyennes = matrix(moyennes,nc=2, nr=2, byrow=T)
colnames(moyennes) = type 
barplot(moyennes,beside=T) ; box() 

# Plot 6
x = c(avgBTCVol1) 
y = c(avgBTCVol3) 
x;y
type = c("Avg BTC Volume Traded (2013-2018)","Avg BTC Volume Traded (2018-2019)") 
moyennes = c(x,y) 
# nc : nombre de tests
# nr : nombre de barres accolées
moyennes = matrix(moyennes,nc=1, nr=2, byrow=T)
colnames(moyennes) = type 
barplot(moyennes,beside=T) ; box() 





