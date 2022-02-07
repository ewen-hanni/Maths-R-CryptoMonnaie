print("////////////////////////////////////////////////////////////////////////////")

library(tidyverse)
library(lubridate)
library(fpp2)
library(astsa)
library(plotly)
library(DIZutils)
windowsFonts(Garamond = windowsFont("Garamond"))

nullToNA <- function(x) {
  x[sapply(x, is.null)] <- NA
  return(x)
}
is_date <- function(date) {
  formatted = try(as.Date(date, "%d-%m-%Y"), silent = TRUE)
  return(DIZutils::equals2(as.character(formatted), date))
}
TimestamptoDate <- function(x) {
  print(cat("TimestamptoDate 1 : " , x))
   
  #x[sapply(x,is_date)]
  # à fix
  x<- as.Date(as.POSIXct(as.numeric(as.character(x)), origin="1970-01-01", tz="GMT"))
  
  print(cat("TimestamptoDate 2 : " , x))
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
#prends une demie éternité :

mkt_data$Date <- seq(lapply(mkt_data$Timestamp,TimestamptoDate))
mkt_data <- do.call(rbind,lapply(mkt_data,data.frame,stringsAsFactors=FALSE))
#head(mkt_data)
head(mkt_data$Timestamp)
head(mkt_data$Date)

# le tibble (≈petit df)
bit_df = mkt_data %>%
  mutate(Date = dmy(Date)) %>%
  mutate(Vol. = as.numeric(str_sub(Volume_(Currency), end = -2))*1000,
         Change = as.numeric(str_sub(Weighted_Price, end = -2))) %>%
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

# affichage graph
ggplotly(ggplot(bit_df, aes(Timestamp, Price)) + geom_line(col = 'orange') + 
           labs(title = 'Bitcoin', x = '') +
           scale_y_continuous(breaks = c(0, 5000, 10000, 15000, 20000), 
                              labels = c('$0', '$5,000', '$10,000', '$15,000', '$20,000')) + PlotTemplate)

