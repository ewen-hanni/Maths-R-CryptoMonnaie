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

} else if (os == "windows") {
  # windows OS
  print("Using windows")
  windowsFonts(Garamond = windowsFont("Garamond"))
  
} else { 
  print("Not a supported OS") 
}

loadDataFromCSVFiles<- function(fileName) {
  dataAave = read.csv(file = "coin_Aave.csv")
  dataBitcoin = read.csv(file = "coin_Bitcoin.csv")
  dataChainLink = read.csv(file = "coin_ChainLink.csv")
  dataCryptocomCoin = read.csv(file = "coin_CryptocomCoin.csv")
  # to do
  
}

# Change all NULL values to NA
setNullToNA <- function(x) {
x[sapply(x, is.null)] <- NA
return(x)
}

# Check if it's in date format (???)
is_date <- function(date) {
formatted = try(as.Date(date, "%d-%m-%Y"), silent = TRUE)
return(DIZutils::equals2(as.character(formatted), date))
}

# Change a timestamp in seconds (unix Epoch) to a date
setEpochToDate <- function(x) {
#print(cat("setEpochToDate 1 : " , x))
#x[sapply(x,is_date)]
# à fix
x<- as.Date(as.POSIXct(as.numeric(as.character(x)), origin="1970-01-01", tz="GMT"))
# x<-dmy(x)
#print(cat("setEpochToDate 2 : " , x))
return(x)
#head(as.POSIXct(as.numeric(as.character(try$time)), origin="1970-01-01", tz="GMT"))
}
#setwd("C:/Users/Ewen/Downloads/maths R/crypto")
#getwd()
#mkt_data <- read.csv("bitstampUSD_1-min_data_2012-01-01_to_2021-03-31.csv")
mkt_data <- read.csv("coin_Bitcoin.csv")
str(mkt_data)
timestamp <- mkt_data$Timestamp
head(timestamp)
# Timestamp to date
#mkt_data2$Date <- as.Date(as.POSIXct(timestamp, origin=“1970-01-01”))
mkt_data <- lapply(mkt_data,setNullToNA)
#prends une demie éternité :

mkt_data$Date <- seq(lapply(mkt_data$Timestamp,setEpochToDate))
mkt_data <- do.call(rbind,lapply(mkt_data,data.frame,stringsAsFactors=FALSE))
#head(mkt_data)
head(mkt_data$Timestamp)
head(mkt_data$Date)

# le tibble (≈petit df)
bit_df = mkt_data %>%
#mutate(Date = dmy(Date)) %>%
Date %>%
mutate(Vol. = as.numeric(str_sub(Volume_(Currency), end = -2))*1000,
Weighted_Price = as.numeric(str_sub(Weighted_Price, end = -2))) %>%
arrange(Date)

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

mkt_data <- do.call(rbind.data.frame, mkt_data)
# affichage graph

ggplotly(mkt_data %>%
# à retirer pour un graph complet (long à charger)
filter(Timestamp >1385815860  %>%
aes(Timestamp, Price)) + geom_line(col = 'orange') +
labs(title = 'Bitcoin', x = '') +
scale_y_continuous(breaks = c(0, 5000, 10000, 15000),
labels = c('$0', '$5,000', '$10,000', '$15,000')) + PlotTemplate)

#moyennes, à tester
head(mean( mkt_data$Open))
head(mean( mkt_data$High))
head(mean( mkt_data$Low))
head(mean( mkt_data$Close))
head(mean( mkt_data$Weighted_Price))
