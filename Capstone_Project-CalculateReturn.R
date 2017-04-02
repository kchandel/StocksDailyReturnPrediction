## Purpose : Capstone Project - Calcualate daily return for various parameters 
## Date    : 20th Mar 2017

library(data.table)

setwd("C:/Users/user/Documents/GL-Classes/Capstone/Project Data/Equities")
getwd()

index_data <- read.csv("All_Indices_data.csv", stringsAsFactors = FALSE,header = TRUE)
nifty_data <- read.csv("Nifty_Indices_data.csv", stringsAsFactors = FALSE,header = TRUE)
currency_data <- read.csv("Currency_rates_data.csv", stringsAsFactors = FALSE,header = TRUE)

# remove first column 
index_data <- index_data[,-c(1)]
nifty_data <- nifty_data[,-c(1)]
currency_data <- currency_data[,-c(1)]

# Convert data frame into data.table
index_data <- as.data.table(index_data)
nifty_data <- as.data.table(nifty_data)
currency_data <- as.data.table(currency_data)

# Set keys for the dataset 
setkey(index_data, date)
setkey(nifty_data, date)
setkey(currency_data, date)

# cerate new column for previous day index price for each eachange 

index_data_lag <- as.data.frame.list(lapply( index_data , function(x)  lag(x, 1)))
nifty_data_lag <- as.data.frame.list(lapply( nifty_data , function(x)  lag(x, 1)))
currency_data_lag <- as.data.frame.list(lapply( currency_data , function(x)  lag(x, 1)))

names(index_data_lag) <- sub("^", "Previous", names(index_data_lag))
names(nifty_data_lag) <- sub("^", "Previous", names(nifty_data_lag))
names(currency_data_lag) <- sub("^", "Previous", names(currency_data_lag))

index_data_lag <- index_data_lag[,-c(1)]
nifty_data_lag <- nifty_data_lag[,-c(1)]
currency_data_lag <- currency_data_lag[,-c(1)]

index_data <- cbind(index_data,index_data_lag)
nifty_data <- cbind(nifty_data,nifty_data_lag)
currency_data <- cbind(currency_data,currency_data_lag)



# calculating the Daily Return and adding it to new column for index data 
index_data[,Dow.DailyReturn:=(((X.DJI-PreviousX.DJI)/PreviousX.DJI)*100), ]
index_data[,HengSeng.DailyReturn:=(((X.HSI-PreviousX.HSI)/PreviousX.HSI)*100), ]
index_data[,Nasdaq.DailyReturn:=(((X.IXIC-PreviousX.IXIC)/PreviousX.IXIC)*100), ]
index_data[,Nikkei.DailyReturn:=(((X.N225-PreviousX.N225)/PreviousX.N225)*100), ]
index_data[,Nifty.DailyReturn:=(((X.NSEI-PreviousX.NSEI)/PreviousX.NSEI)*100), ]


# calculating the Daily Return and adding it to new column for nifty index  data 
nifty_data[,NSEIT.DailyReturn:=(((X.CNXIT-PreviousX.CNXIT)/PreviousX.CNXIT)*100), ]
nifty_data[,NSEBANK.DailyReturn:=(((X.NSEBANK-PreviousX.NSEBANK)/PreviousX.NSEBANK)*100), ]


# calculating the Daily Return and adding it to new column for currency ratedata 
currency_data[,EUR.DailyReturn:=(((EUR.INR-PreviousEUR.INR)/PreviousEUR.INR)*100), ]
currency_data[,GBP.DailyReturn:=(((GBP.INR-PreviousGBP.INR)/PreviousGBP.INR)*100), ]
currency_data[,JPY.DailyReturn:=(((JPY.INR-PreviousJPY.INR)/PreviousJPY.INR)*100), ]
currency_data[,USD.DailyReturn:=(((USD.INR-PreviousUSD.INR)/PreviousUSD.INR)*100), ]


# keep only daily return column
index_data[,2:11] <- NULL
nifty_data[,2:5] <- NULL
currency_data[,2:9] <- NULL

# write return data to csv
write.csv(index_data,"Index_DailyReturn.csv")
write.csv(nifty_data,"NiftySector_DailyReturn.csv")
write.csv(currency_data,"Currency_DailyReturn.csv")
