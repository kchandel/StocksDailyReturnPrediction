## Purpose : Capstone Project - calculate target variable and daily return for stock prices 
## Date    : 09th Jan 2017


## Setting the current working directory
#

setwd("C:/Users/user/Documents/GL-Classes/Capstone/Project Data/Equities")
getwd()

## Loading the required packages

library(doParallel)
library(foreach)
library(rbenchmark)
library(data.table)
library(dplyr)
library(sqldf)
library(TTR)


equities_data.all <-fread("Equities_data_all.csv", stringsAsFactors = FALSE,header = TRUE)

# Convert date field as date type and order the data by date 


equities_data.all$date <- as.POSIXct(strptime(equities_data.all$date,format="%Y-%m-%d"))
equities_data.all <- equities_data.all[order(as.Date(equities_data.all$date,format="%Y-%m-%d")),,drop=FALSE]


# Dropping unnecessary columns 
equities_data.all$V1 <- NULL


# Rename columns 

stocks_data <- equities_data.all
stocks_data <- plyr::rename(stocks_data, c(date="TIMESTAMP",Symbol="SYMBOL"))
#stocks_data <- plyr::rename(stocks_data, c(OPEN.y="OPEN_FUT",HIGH.y="HIGH_FUT",LOW.y="LOW_FUT",CLOSE.y="CLOSE_FUT"))

length(unique(stocks_data$TIMESTAMP))
length(unique(stocks_data$SYMBOL))
head(stocks_data)


# Convert data frame into data.table
stocks_data <- as.data.table(stocks_data)

# Set keys for the dataset 
setkey(stocks_data, SYMBOL, TIMESTAMP)

# cerate new column for previous day adjusted close price 
stocks_data[,PreviousDay_Adjusted:=lag(adjusted, 1), by=SYMBOL]

# Filter out rows where PreviousDay_Adjusted and adjusted is NA
stocks_data <- stocks_data[!is.na(PreviousDay_Adjusted) & !is.na(adjusted)]




# calculating the Daily Return and adding it to new column 
stocks_data[,Daily_Return:=(((adjusted-PreviousDay_Adjusted)/PreviousDay_Adjusted)*100), ]

stocks_data <- as.data.frame(stocks_data)

stocks_data$Target[stocks_data$Daily_Return >= 2 ] <- 1
stocks_data$Target[stocks_data$Daily_Return < 2 ] <- 0

#Target Rate 
sum(stocks_data$Target)/nrow(stocks_data)


# convert dataframe into data table 
stocks_data <- as.data.table(stocks_data)

# Sort data by symbol and timestamp

setkey(stocks_data,SYMBOL,TIMESTAMP)

str(stocks_data)
sum(is.na(stocks_data$adjusted))

# Calculate adjusted open, high and low price 
stocks_data[,adjustedOpen:=((adjusted)/(1 + ((close-open)/open))), ]
stocks_data[,adjustedHigh:=((adjusted)/(1 + ((close-high)/high))), ]
stocks_data[,adjustedLow:=((adjusted)/(1 + ((close-low)/low))), ]
stocks_data[,adjustedClose:=(adjusted), ]

# Load global indices daily return data 

indices_data.all <-fread("Index_DailyReturn.csv", stringsAsFactors = FALSE,header = TRUE)
indices_data.all <- as.data.table(indices_data.all)
setnames(indices_data.all,"date","TIMESTAMP")


indices_data.all[,1] <- NULL

indices_data.all$TIMESTAMP <- as.POSIXct(strptime(indices_data.all$TIMESTAMP,format="%Y-%m-%d"))

# Merge indices data to the stocks data

stocks_data <- merge(stocks_data,indices_data.all,by=c("TIMESTAMP"), all=TRUE)


# Load nifty sector daily return data 

niftysector.all <-fread("NiftySector_DailyReturn.csv", stringsAsFactors = FALSE,header = TRUE)
niftysector.all <- as.data.table(niftysector.all)
setnames(niftysector.all,"date","TIMESTAMP")


niftysector.all[,1] <- NULL

niftysector.all$TIMESTAMP <- as.POSIXct(strptime(niftysector.all$TIMESTAMP,format="%Y-%m-%d"))

# Merge indices data to the stocks data

stocks_data <- merge(stocks_data,niftysector.all,by=c("TIMESTAMP"), all=TRUE)


# Load currency daily return data 

currency.all <-fread("Currency_DailyReturn.csv", stringsAsFactors = FALSE,header = TRUE)
currency.all <- as.data.table(currency.all)
setnames(currency.all,"date","TIMESTAMP")


currency.all[,1] <- NULL

currency.all$TIMESTAMP <- as.POSIXct(strptime(currency.all$TIMESTAMP,format="%Y-%m-%d"))

# Merge indices data to the stocks data

stocks_data <- merge(stocks_data,currency.all,by=c("TIMESTAMP"), all=TRUE)

# Remove records where Symbol is NA
sum(is.na(stocks_data$SYMBOL))
stocks_data <- filter(stocks_data,!is.na(SYMBOL))

stocks_data <- as.data.table(stocks_data)
setkey(stocks_data,SYMBOL,TIMESTAMP)

# write data to csv
write.csv(stocks_data,"stocks_data.csv")

