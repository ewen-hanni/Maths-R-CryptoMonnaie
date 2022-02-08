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

data <- read.csv(file = "crypto-markets.csv")
data <- lapply(data, setNullToNA)

get2016to2018Data <- function() {
  selectData = data[data$date > "2016-01-01" & data$date < "2018-01-01",]
  return(selectData)
}

#data <- read.csv("bitstampUSD_1-min_data_2012-01-01_to_2021-03-31.csv")

str(data)
timestamp <- data$Timestamp
head(timestamp)
# Timestamp to date
#dataBitcoin2$Date <- as.Date(as.POSIXct(timestamp, origin=“1970-01-01”))
#prends une demie éternité :

data$Date <- seq(lapply(data$Timestamp,setEpochToDate))
data <- do.call(rbind,lapply(data,data.frame,stringsAsFactors=FALSE))
#head(data)
head(data$Timestamp)
head(data$Date)

# le tibble (≈petit df)
bit_df = data %>%
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

data <- do.call(rbind.data.frame, data)
# affichage graph

ggplotly(data %>%
# à retirer pour un graph complet (long à charger)
filter(Timestamp >1385815860  %>%
aes(Timestamp, Price)) + geom_line(col = 'orange') +
labs(title = 'Bitcoin', x = '') +
scale_y_continuous(breaks = c(0, 5000, 10000, 15000),
labels = c('$0', '$5,000', '$10,000', '$15,000')) + PlotTemplate)

#moyennes, à tester
head(mean( data$Open))
head(mean( data$High))
head(mean( data$Low))
head(mean( data$Close))
head(mean( data$Weighted_Price))
