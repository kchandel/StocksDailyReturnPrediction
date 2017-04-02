## Purpose : Capstone Project - Download data from Yahoo finance for all 
#          : NSE stocks
## Date    : 09th Jan 2017


setwd("C:/Users/user/Documents/GL-Classes/Capstone/Project Data/Equities")
getwd()

## Loading the required packages

library(doParallel)
library(foreach)
library(rbenchmark)
library(data.table)
library(tidyquant)
library(dplyr)
stockList<-read.csv("NSE_Symbol.csv", stringsAsFactors = FALSE,header = FALSE)

FOREACH = {
  
  registerDoParallel(cores = 4)
  
  equities_data <- foreach(x=iter(stockList, by='row'), .combine=dplyr::bind_rows)  %dopar% { 
    if (!is.na(tidyquant::tq_get(x))) {
      cbind(Symbol=x,as.data.frame(tidyquant::tq_get(x,from="2012-01-01")))
    }
  }
  
}


equities_data.1 <- equities_data[,-c(9)]

write.csv(equities_data.1,"Equities_data_all.csv")


# Download index data 
# NIFTY - ^NSEI
# NASDAQ - ^IXIC
# Dow  - ^DJI
# Nikkei  - ^N225
# Hang Seng -^HSI

index_list <- as.list(c("^NSEI","^IXIC","^DJI","^N225","^HSI"))

FOREACH = {
  
  registerDoParallel(cores = 4)
  
  index_data <- foreach(i = index_list, .combine=dplyr::bind_rows) %dopar% { 
    if (!is.na(tidyquant::tq_get(i))) {
      cbind(Symbol=i,as.data.frame(tidyquant::tq_get(i,from="2012-01-01")))
    }
  }
}

index_data <- index_data[,-c(3,4,5,6,7)]

index_data_spread <- spread(index_data,Symbol,adjusted,fill=NA)

# Write Index data to file 
write.csv(index_data_spread,"All_Indices_data.csv")



# Download key  NIFTY indices  
# NIFTY BANK - ^NSEBANK
# NIFTY COMMODITIES - ^CNXCMDT
# NIFTY INFRA  - ^CNXINFRA
# NIFTY SIT - ^CNXIT
# NIFTY METAL - ^CNXMETAL

cnxIdx_list <- as.list(c("^NSEBANK","^CNXIT"))

FOREACH = {
  
  registerDoParallel(cores = 4)
  
  cnxIdx_data <- foreach(i = cnxIdx_list, .combine=dplyr::bind_rows) %dopar% { 
    if (!is.na(tidyquant::tq_get(i))) {
      cbind(Symbol=i,as.data.frame(tidyquant::tq_get(i,from="2012-01-01")))
    }
  }
}

cnxIdx_data <- cnxIdx_data[,-c(3,4,5,6,7)]

cnxIdx_data_spread <- spread(cnxIdx_data,Symbol,adjusted,fill=NA)

# Write Index data to file 
write.csv(cnxIdx_data_spread,"Nifty_Indices_data.csv")


# Download currency data 
# USD/INR - USD/INR
# EUR/INR - EUR/INR
# GBP/INR  - GBP/INR
# JPY/INR  - JPY/INR

currency_list <- as.list(c("USD/INR","EUR/INR","GBP/INR","JPY/INR"))

FOREACH = {
  
  registerDoParallel(cores = 4)
  
  currency_data <- foreach(i = currency_list, .combine=dplyr::bind_rows) %dopar% { 
    if (!is.na(tidyquant::tq_get(i,get = "exchange.rates"))) {
      cbind(Symbol=i,as.data.frame(tidyquant::tq_get(i,get = "exchange.rates",from="2012-01-01")))
    }
  }
}

currency_data_spread <- spread(currency_data,Symbol,exchange.rate,fill=NA)

# Write Index data to file 
write.csv(currency_data_spread,"Currency_rates_data.csv")
