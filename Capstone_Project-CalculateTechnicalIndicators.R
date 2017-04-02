## Purpose : Capstone Project - Calculate technical indicators
## Date    : 1st April 2017

setwd("C:/Users/user/Documents/GL-Classes/Capstone/Project Data/Equities")
getwd()

## Loading the required packages

library(data.table)
library(dplyr)
library(TTR)

stocks_data <-fread("stock_beta_all.csv", stringsAsFactors = FALSE,header = TRUE)

# Dropping unnecessary columns 
stocks_data$V1 <- NULL

stocks_data$TIMESTAMP <- as.POSIXct(strptime(stocks_data$TIMESTAMP,format="%Y-%m-%d"))

# Convert data frame into data.table
stocks_data <- as.data.table(stocks_data)

# Set keys for the dataset 
setkey(stocks_data, SYMBOL, TIMESTAMP)


stocks_data <- stocks_data[,freq:=length(adjusted),by=.(SYMBOL)]

stocks_data <- stocks_data[which(freq > 50)]


# Calculate 10 day SMA 
stocks_data <- stocks_data[,sma_10day:=list(sma_10day=SMA(adjustedClose,n=10)),by=.(SYMBOL)]

# Calculate 20 day SMA 
stocks_data <- stocks_data[,sma_20day:=list(sma_20day=SMA(adjustedClose,n=20)),by=.(SYMBOL)]


# Calculate 50 day SMA 
stocks_data <- stocks_data[,sma_50day:=list(sma_50day=SMA(adjustedClose,n=50)),by=.(SYMBOL)]

# Calculate 10 Day EMA
stocks_data <- stocks_data[,ema_10day:=list(ema_10day=EMA(adjustedClose,n=10)),by=.(SYMBOL)]

# Calculate 20 Day EMA
stocks_data <- stocks_data[,ema_20day:=list(ema_20day=EMA(adjustedClose,n=20)),by=.(SYMBOL)]

# Calculate 50 Day EMA
stocks_data <- stocks_data[,ema_50day:=list(ema_50day=EMA(adjustedClose,n=50)),by=.(SYMBOL)]

# Aroon UP , Aroon Down and Aroon Oscillator - 5 Days
stocks_data <- stocks_data[,c("aroonUP_5D","aroonDN_5D","aroonOscillator_5D"):=.(aroon(.SD,n=5)[,1],aroon(.SD,n=5)[,2],aroon(.SD,n=5)[,3]),by=.(SYMBOL),.SDcols=c("adjustedHigh","adjustedLow")]

# Aroon UP , Aroon Down and Aroon Oscillator - 10 Days
stocks_data <- stocks_data[,c("aroonUP_10D","aroonDN_10D","aroonOscillator_10D"):=.(aroon(.SD,n=10)[,1],aroon(.SD,n=10)[,2],aroon(.SD,n=10)[,3]),by=.(SYMBOL),.SDcols=c("adjustedHigh","adjustedLow")]

# Aroon UP , Aroon Down and Aroon Oscillator - 20 Days
stocks_data <- stocks_data[,c("aroonUP_20D","aroonDN_20D","aroonOscillator_20D"):=.(aroon(.SD,n=20)[,1],aroon(.SD,n=20)[,2],aroon(.SD,n=20)[,3]),by=.(SYMBOL),.SDcols=c("adjustedHigh","adjustedLow")]

# MCAD 12,26,9

stocks_data <- stocks_data[,c("MACD","MCADSignal"):=.(MACD(.SD,12,26,9,maType="EMA")[,1],MACD(.SD,12,26,9,maType="EMA")[,2]),by=.(SYMBOL),.SDcols=c("adjustedClose")]

# RSI
stocks_data <- stocks_data[,Rsi:=list(Rsi=RSI(adjustedClose)),by=.(SYMBOL)]

# Bollinger Bands - 5 Days
stocks_data <- stocks_data[,c("LowerBB_5D","MiddleBB_5D","UperBB_5D","PercentBB_5D"):=.(BBands(.SD, n=5)[,1],BBands(.SD, n=5)[,2],BBands(.SD,n=5)[,3],BBands(.SD,n=5)[,4]),by=.(SYMBOL),.SDcols=c("adjustedHigh","adjustedLow","adjustedClose")]

# Bollinger Bands - 10 Days
stocks_data <- stocks_data[,c("LowerBB_10D","MiddleBB_10D","UperBB_10D","PercentBB_10D"):=.(BBands(.SD, n=10)[,1],BBands(.SD, n=10)[,2],BBands(.SD,n=10)[,3],BBands(.SD,n=10)[,4]),by=.(SYMBOL),.SDcols=c("adjustedHigh","adjustedLow","adjustedClose")]

# Bollinger Bands - 20 Days
stocks_data <- stocks_data[,c("LowerBB_20D","MiddleBB_20D","UperBB_20D","PercentBB_20D"):=.(BBands(.SD, n=20)[,1],BBands(.SD, n=20)[,2],BBands(.SD,n=10)[,3],BBands(.SD,n=10)[,4]),by=.(SYMBOL),.SDcols=c("adjustedHigh","adjustedLow","adjustedClose")]

# Stochastic Oscillator
stocks_data <- stocks_data[,c("StochFastK","StochFastD","StochLowD"):=.(stoch(.SD)[,1],stoch(.SD)[,2],stoch(.SD)[,3]),by=.(SYMBOL),.SDcols=c("adjustedHigh","adjustedLow","adjustedClose")]

# Stochastic Momentum Index
stocks_data <- stocks_data[,c("StochMomInd","StochMomSignal"):=.(SMI(.SD)[,1],SMI(.SD)[,2]),by=.(SYMBOL),.SDcols=c("adjustedHigh","adjustedLow","adjustedClose")]

#Chande Momentum Oscillator (CMO)
stocks_data <- stocks_data[,Cmo:=list(Cmo=CMO(adjustedClose)),by=.(SYMBOL)]

#Commodity Channel Index (CCI) - 5 Days
stocks_data <- stocks_data[,Cci_5D:=list(Cci_5D=CCI(.SD, n=5)),by=.(SYMBOL),.SDcols=c("adjustedHigh","adjustedLow","adjustedClose")]

#Commodity Channel Index (CCI) - 10 Days
stocks_data <- stocks_data[,Cci_10D:=list(Cci_10D=CCI(.SD, n=10)),by=.(SYMBOL),.SDcols=c("adjustedHigh","adjustedLow","adjustedClose")]

#Commodity Channel Index (CCI) - 20 Days
stocks_data <- stocks_data[,Cci_20D:=list(Cci_20D=CCI(.SD, n=20)),by=.(SYMBOL),.SDcols=c("adjustedHigh","adjustedLow","adjustedClose")]

#Chaikin Volatility indicator - 5 Days
stocks_data <- stocks_data[,ChaikinVolatility_5D:=list(ChaikinVolatility_5D=chaikinVolatility(.SD,n=5)),by=.(SYMBOL),.SDcols=c("adjustedHigh","adjustedLow")]

#Chaikin Volatility indicator - 10 Days
stocks_data <- stocks_data[,ChaikinVolatility_10D:=list(ChaikinVolatility_10D=chaikinVolatility(.SD,n=10)),by=.(SYMBOL),.SDcols=c("adjustedHigh","adjustedLow")]

#Chaikin Volatility indicator - 20 Days
stocks_data <- stocks_data[,ChaikinVolatility_20D:=list(ChaikinVolatility_20D=chaikinVolatility(.SD,n=20)),by=.(SYMBOL),.SDcols=c("adjustedHigh","adjustedLow")]

#Money Flow Index  (MFI) - 14 Days
stocks_data <- stocks_data[,Mfi:=list(Mfi=MFI(.SD,stocks_data[,c(volume)])),by=.(SYMBOL),.SDcols=c("adjustedHigh","adjustedLow","adjustedClose")]

#Trend Detection Index (TDI) - 5 Days
stocks_data <- stocks_data[,c("Tdi_Index_5D","Tdi_indicator_5D"):=.(TDI(.SD,n=5)[,1],TDI(.SD,n=5)[,2]),by=.(SYMBOL),.SDcols=c("adjustedClose")]

#Trend Detection Index (TDI) - 10 Days
stocks_data <- stocks_data[,c("Tdi_Index_10D","Tdi_indicator_10D"):=.(TDI(.SD,n=10)[,1],TDI(.SD,n=10)[,2]),by=.(SYMBOL),.SDcols=c("adjustedClose")]

#Trend Detection Index (TDI) - 20 Days
stocks_data <- stocks_data[,c("Tdi_Index_20D","Tdi_indicator_20D"):=.(TDI(.SD,n=20)[,1],TDI(.SD,n=20)[,2]),by=.(SYMBOL),.SDcols=c("adjustedClose")]

# ROC Price  2 Days
stocks_data <- stocks_data[,c("ROC_Price_2D"):=.(ROC(.SD,n=2)),by=.(SYMBOL),.SDcols=c("adjustedClose")]

# ROC Price  5 Days
stocks_data <- stocks_data[,c("ROC_Price_5D"):=.(ROC(.SD,n=5)),by=.(SYMBOL),.SDcols=c("adjustedClose")]

# ROC Price  10 Days
stocks_data <- stocks_data[,c("ROC_Price_10D"):=.(ROC(.SD,n=10)),by=.(SYMBOL),.SDcols=c("adjustedClose")]

# ROC Volume  2 Days
stocks_data <- stocks_data[,c("ROC_Vol_2D"):=.(ROC(.SD,n=2)),by=.(SYMBOL),.SDcols=c("volume")]

# ROC Volume  5 Days
stocks_data <- stocks_data[,c("ROC_Vol_5D"):=.(ROC(.SD,n=5)),by=.(SYMBOL),.SDcols=c("volume")]

# ROC Volume  10 Days
stocks_data <- stocks_data[,c("ROC_Vol_10D"):=.(ROC(.SD,n=10)),by=.(SYMBOL),.SDcols=c("volume")]

#William % R (WPR) - 5 Days
stocks_data <- stocks_data[,Wpr_5D:=list(Wpr_5D=WPR(.SD, n=5)),by=.(SYMBOL),.SDcols=c("adjustedHigh","adjustedLow","adjustedClose")]

#William % R (WPR) - 10 Days
stocks_data <- stocks_data[,Wpr_10D:=list(Wpr_10D=WPR(.SD, n=10)),by=.(SYMBOL),.SDcols=c("adjustedHigh","adjustedLow","adjustedClose")]

#William % R (WPR) - 20 Days
stocks_data <- stocks_data[,Wpr_20D:=list(Wpr_20D=WPR(.SD, n=20)),by=.(SYMBOL),.SDcols=c("adjustedHigh","adjustedLow","adjustedClose")]

# using fast write method fwrite : need to re-install the data.table package from github
#install.packages("data.table", 
 #                repos = "https://Rdatatable.github.io/data.table", type = "source")

write.csv(stocks_data,"Final_stocks_data.csv")
