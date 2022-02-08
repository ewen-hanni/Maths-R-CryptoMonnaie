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

dataAave <- read.csv(file = "coin_Aave.csv")
dataAave <- lapply(dataAave, setNullToNA)

dataBitcoin <- read.csv(file = "coin_Bitcoin.csv")
dataBitcoin <- lapply(dataBitcoin, setNullToNA)

dataChainLink <- read.csv(file = "coin_ChainLink.csv");
dataChainLink <- lapply(dataChainLink, setNullToNA)

dataCryptocomCoin <- read.csv(file = "coin_CryptocomCoin.csv");
dataCryptocomCoin <- lapply(dataCryptocomCoin, setNullToNA)

dataEOS = read.csv(file = "coin_EOS.csv");
dataEOS <- lapply(dataEOS, setNullToNA)

dataIota = read.csv(file = "coin_Iota.csv");
dataIota <- lapply(dataIota, setNullToNA)

# and the reste ...
#dataMonero = read.csv(file = "coin_Monero.csv");
#dataPolkadot = read.csv(file = "coin_Polkadot.csv");
#dataStellar = read.csv(file = "coin_Stellar.csv");
#dataTron = read.csv(file = "coin_Tron.csv");
#dataUSDCoin = read.csv(file = "coin_USDCoin.csv");
#dataXRP = read.csv(file = "coin_XRP.csv");
#dataBitcoinHistorical = read.csv(file = "Bitcoin_Historical_Data.csv");
#dataBinanceCoin = read.csv(file = "coin_BinanceCoin.csv");
#dataCardano = read.csv(file = "coin_Cardano.csv");
#dataCosmos = read.csv(file = "coin_Cosmos.csv");
#dataDogecoin = read.csv(file = "coin_Dogecoin.csv");
#dataEthereum = read.csv(file = "coin_Ethereum.csv");
#dataLitecoin = read.csv(file = "coin_Litecoin.csv");
#dataNEM = read.csv(file = "coin_NEM.csv");
#dataSolana = read.csv(file = "coin_Solana.csv");
#dataTether = read.csv(file = "coin_Tether.csv");
#dataUniswap = read.csv(file = "coin_Uniswap.csv");
#dataWrappedBitcoin = read.csv(file = "coin_WrappedBitcoin.csv");


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

#dataBitcoin <- read.csv("bitstampUSD_1-min_data_2012-01-01_to_2021-03-31.csv")

str(dataBitcoin)
timestamp <- dataBitcoin$Timestamp
head(timestamp)
# Timestamp to date
#dataBitcoin2$Date <- as.Date(as.POSIXct(timestamp, origin=“1970-01-01”))
dataBitcoin <- lapply(dataBitcoin,setNullToNA)
#prends une demie éternité :

dataBitcoin$Date <- seq(lapply(dataBitcoin$Timestamp,setEpochToDate))
dataBitcoin <- do.call(rbind,lapply(dataBitcoin,data.frame,stringsAsFactors=FALSE))
#head(dataBitcoin)
head(dataBitcoin$Timestamp)
head(dataBitcoin$Date)

# le tibble (≈petit df)
bit_df = dataBitcoin %>%
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

dataBitcoin <- do.call(rbind.data.frame, dataBitcoin)
# affichage graph

ggplotly(dataBitcoin %>%
# à retirer pour un graph complet (long à charger)
filter(Timestamp >1385815860  %>%
aes(Timestamp, Price)) + geom_line(col = 'orange') +
labs(title = 'Bitcoin', x = '') +
scale_y_continuous(breaks = c(0, 5000, 10000, 15000),
labels = c('$0', '$5,000', '$10,000', '$15,000')) + PlotTemplate)

#moyennes, à tester
head(mean( dataBitcoin$Open))
head(mean( dataBitcoin$High))
head(mean( dataBitcoin$Low))
head(mean( dataBitcoin$Close))
head(mean( dataBitcoin$Weighted_Price))
