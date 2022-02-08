# On Mac :
# Be sure to execute first the following brew CLI command
# brew install openssl

library(tidyverse)
library(lubridate)
library(fpp2)
library(astsa)
library(plotly)
library(DIZutils)

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
minAvgBTCValue <- mean(df$low, na.rm=T)
minAvgBTCValue

# Average maximum value of BTC during the 2016 to 2018 period
print("Avg max BTC value")
df <- get2016to2018BTCData()
maxAvgBTCValue <- mean(df$high, na.rm=T)
maxAvgBTCValue

# Average BTC volume traded during 2016 to 2018 period
print("Average BTC traded volume")
df <- get2016to2018BTCData()
avgBTCVol <- mean(df$volume)
avgBTCVol

# Sum of all volume traded including BTC and the rest during 2016 to 2018 period
print("Sum of all volume traded")
df <- get2016to2018Data()
tradedAllVol = sum(df$volume, na.rm=T)
tradedAllVol

# Sum of all BTC volume traded during 2016 to 2018 period
print("Sum of BTC volume traded")
df <- get2016to2018BTCData()
tradedBTCVol <- sum(df$volume)
tradedBTCVol

# Sum of all non BTC volume traded during 2016-2018 period
print("Sum of non BTC volume traded")
df <- get2016to2018NonBTCData()
tradedNonBTCVol <- sum(df$volume, na.rm=T)
tradedNonBTCVol

# Percentage of BTC volume traded on all volume traded during 2016 to 2018 period
print("Percentage of BTC volume traded on all volume")
percentageBTC <- tradedBTCVol/tradedAllVol
percentageBTC

# Percentage of non BTC volume traded on all volume traded during 2016 to 2018 period
print("Percentage of non BTC volume traded on all volume")
percentageNonBTC <- tradedNonBTCVol/tradedAllVol
percentageNonBTC

#------------------------------------------------------------------------------#
                              # 2018 ONLY

# Average minimum value of BTC during the 2018 to 2019 period
print("Avg min BTC value")
df <- get2018BTCData()
minAvgBTCValue <- mean(df$low, na.rm=T)
minAvgBTCValue

# Average maximum value of BTC during the 2018 to 2019 period
print("Avg max BTC value")
df <- get2018BTCData()
maxAvgBTCValue <- mean(df$high, na.rm=T)
maxAvgBTCValue

# Average BTC volume traded during the 2018 to 2019 period
print("Average BTC traded volume")
df <- get2018BTCData()
avgBTCVol <- mean(df$volume, na.rm=T)
avgBTCVol

# Sum of all volume traded including BTC and the rest during 2018 to 2019 period
print("Sum of all volume traded")
df <- get2018Data()
tradedAllVol = sum(df$volume, na.rm=T)
tradedAllVol

# Sum of all BTC volume traded during 2018 to 2019 period
print("Sum of BTC volume traded")
df <- get2018BTCData()
tradedBTCVol <- sum(df$volume, na.rm=T)
tradedBTCVol

# Sum of all non BTC volume traded during 2018-2019 period
print("Sum of non BTC volume traded")
df <- get2018NonBTCData()
tradedNonBTCVol <- sum(df$volume, na.rm=T)
tradedNonBTCVol

# Percentage of BTC volume traded on all volume traded during 2018 to 2019 period
print("Percentage of BTC volume traded on all volume")
percentageBTC <- tradedBTCVol/tradedAllVol
percentageBTC

# Percentage of non BTC volume traded on all volume traded during 2016 to 2018 period
print("Percentage of non BTC volume traded on all volume")
percentageNonBTC <- tradedNonBTCVol/tradedAllVol
percentageNonBTC

#------------------------------------------------------------------------------#

#template des graph
PlotTemplate = theme(
plot.title = element_text(hjust = .5, size = 28, colour = 'yellow'),
text = element_text(family = 'Garamond'),
axis.text = element_text(size = 12),
axis.title = element_text(size = 20, family = 'Garamond', face = 'bold'),
axis.line = element_line(colour = 'grey', size = 1),
panel.grid = element_line(color = 'lightgrey'),
panel.background = element_rect(fill = 'white'),
strip.background = element_rect(colour = "black", fill = "white"),
strip.text = element_text(face = 'bold'))

data <- do.call(rbind.data.frame, data)
# affichage graph

ggplotly(data %>%
# à retirer pour un graph complet (long à charger)
filter(Timestamp >1385815860  %>%
aes(Timestamp, Price)) + geom_line(col = 'orange') +
labs(title = 'Bitcoin', x = '') +
scale_y_continuous(breaks = c(0, 5000, 10000, 15000),
labels = c('$0', '$5,000', '$10,000', '$15,000')) + PlotTemplate)
