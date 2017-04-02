## Purpose : Capstone Project - stock beta w.r.t various parameters  
## Date    : 21st Mar 2017


## Setting the current working directory
#
library(data.table)
library(dplyr)
setwd("C:/Users/user/Documents/GL-Classes/Capstone/Project Data/Equities")
getwd()

stock_data.all <-fread("stocks_data.csv", stringsAsFactors = FALSE,header = TRUE)

calc_beta <- function(df) {
  
  df <- as.data.frame(df)
  beta <- matrix(NA,nrow = 1,ncol = length(df)-1)
  stock <- df[,1]
  for (i in 1:(length(df)-1)) {
    beta[,i] <- cov(df[,1],df[,i+1])/var(df[,i+1])
  }
  
  return(beta)
}

cust_rolling_apply <- function(df,period) {
  
  betaList <- data.frame(matrix(double(),ncol = length(df)-1))
  
  for ( i in 1:nrow(df))
  {
    subDf <- df[(max(i-period,0)+1):i,]
    if (nrow(subDf) >= period) {
      betaList <- rbind(betaList,calc_beta(subDf))
    } else {
      betaList<- rbind(betaList,matrix(NA,ncol =length(subDf)-1 ))
    }
  }
  #View(betList)
  return(betaList)
}

betaColumns <- c("Dow.DailyReturn","HengSeng.DailyReturn","Nasdaq.DailyReturn","Nikkei.DailyReturn","Nifty.DailyReturn","NSEIT.DailyReturn","NSEBANK.DailyReturn","EUR.DailyReturn","GBP.DailyReturn","JPY.DailyReturn","USD.DailyReturn")
stock_data.all <- as.data.frame(stock_data.all)
stock_data.all[betaColumns][is.na(stock_data.all[betaColumns])] <- 0


stock_data.all$TIMESTAMP <- as.Date(stock_data.all$TIMESTAMP)
stock_data.2012 <- filter(stock_data.all,TIMESTAMP >= as.Date('2012-01-01') & TIMESTAMP <= (as.Date('2012-12-31')) )
stock_data.2013 <- filter(stock_data.all,TIMESTAMP >= (as.Date('2013-01-01')-40) & TIMESTAMP <= (as.Date('2013-12-31')) )
stock_data.2014 <- filter(stock_data.all,TIMESTAMP >= (as.Date('2014-01-01')-40) & TIMESTAMP <= (as.Date('2014-12-31')) )
stock_data.2015 <- filter(stock_data.all,TIMESTAMP >= (as.Date('2015-01-01')-40) & TIMESTAMP <= (as.Date('2015-12-31')) )
stock_data.2016 <- filter(stock_data.all,TIMESTAMP >= (as.Date('2016-01-01')-40) & TIMESTAMP <= (as.Date('2016-12-31')) )
stock_data.2017 <- filter(stock_data.all,TIMESTAMP >= (as.Date('2017-01-01')-40) & TIMESTAMP <= (as.Date('2017-12-31')) )

stock_data.2012 <- as.data.table(stock_data.2012)
stock_data.2013 <- as.data.table(stock_data.2013)
stock_data.2014 <- as.data.table(stock_data.2014)
stock_data.2015 <- as.data.table(stock_data.2015)
stock_data.2016 <- as.data.table(stock_data.2016)
stock_data.2017 <- as.data.table(stock_data.2017)


Sys.time()
stock_data.2012 <- stock_data.2012[,c("betaDow","betaHK","betaNasDaq","betaNikkei",
                                      "betaNifty","betaNSEIT","betaNSEBANK",
                                      "betaEUR","BetaGBP","betaJPY","betaUSD"):=.(as.numeric(cust_rolling_apply(.SD,30)[,1]),
                                                                                       as.numeric(cust_rolling_apply(.SD,30)[,2]),
                                                                                       as.numeric(cust_rolling_apply(.SD,30)[,3]),
                                                                                       as.numeric(cust_rolling_apply(.SD,30)[,4]),
                                                                                       as.numeric(cust_rolling_apply(.SD,30)[,5]),
                                                                                       as.numeric(cust_rolling_apply(.SD,30)[,6]),
                                                                                       as.numeric(cust_rolling_apply(.SD,30)[,7]),
                                                                                       as.numeric(cust_rolling_apply(.SD,30)[,8]),
                                                                                       as.numeric(cust_rolling_apply(.SD,30)[,9]),
                                                                                       as.numeric(cust_rolling_apply(.SD,30)[,10]),
                                                                                       as.numeric(cust_rolling_apply(.SD,30)[,11])
                                                                                       ),by=.(SYMBOL),.SDcols=c("Daily_Return",
                                                                                                                "Dow.DailyReturn",
                                                                                                                "HengSeng.DailyReturn",
                                                                                                                "Nasdaq.DailyReturn",
                                                                                                                "Nikkei.DailyReturn",
                                                                                                                "Nifty.DailyReturn",
                                                                                                                "NSEIT.DailyReturn",
                                                                                                                "NSEBANK.DailyReturn",
                                                                                                                "EUR.DailyReturn",
                                                                                                                "GBP.DailyReturn",
                                                                                                                "JPY.DailyReturn",
                                                                                                                "USD.DailyReturn")]
                                                                                       
Sys.time()

Sys.time()
stock_data.2013 <- stock_data.2013[,c("betaDow","betaHK","betaNasDaq","betaNikkei",
                                      "betaNifty","betaNSEIT","betaNSEBANK",
                                      "betaEUR","BetaGBP","betaJPY","betaUSD"):=.(as.numeric(cust_rolling_apply(.SD,30)[,1]),
                                                                                  as.numeric(cust_rolling_apply(.SD,30)[,2]),
                                                                                  as.numeric(cust_rolling_apply(.SD,30)[,3]),
                                                                                  as.numeric(cust_rolling_apply(.SD,30)[,4]),
                                                                                  as.numeric(cust_rolling_apply(.SD,30)[,5]),
                                                                                  as.numeric(cust_rolling_apply(.SD,30)[,6]),
                                                                                  as.numeric(cust_rolling_apply(.SD,30)[,7]),
                                                                                  as.numeric(cust_rolling_apply(.SD,30)[,8]),
                                                                                  as.numeric(cust_rolling_apply(.SD,30)[,9]),
                                                                                  as.numeric(cust_rolling_apply(.SD,30)[,10]),
                                                                                  as.numeric(cust_rolling_apply(.SD,30)[,11])
                                      ),by=.(SYMBOL),.SDcols=c("Daily_Return",
                                                               "Dow.DailyReturn",
                                                               "HengSeng.DailyReturn",
                                                               "Nasdaq.DailyReturn",
                                                               "Nikkei.DailyReturn",
                                                               "Nifty.DailyReturn",
                                                               "NSEIT.DailyReturn",
                                                               "NSEBANK.DailyReturn",
                                                               "EUR.DailyReturn",
                                                               "GBP.DailyReturn",
                                                               "JPY.DailyReturn",
                                                               "USD.DailyReturn")]




Sys.time()

Sys.time()
stock_data.2014 <- stock_data.2014[,c("betaDow","betaHK","betaNasDaq","betaNikkei",
                                      "betaNifty","betaNSEIT","betaNSEBANK",
                                      "betaEUR","BetaGBP","betaJPY","betaUSD"):=.(as.numeric(cust_rolling_apply(.SD,30)[,1]),
                                                                                  as.numeric(cust_rolling_apply(.SD,30)[,2]),
                                                                                  as.numeric(cust_rolling_apply(.SD,30)[,3]),
                                                                                  as.numeric(cust_rolling_apply(.SD,30)[,4]),
                                                                                  as.numeric(cust_rolling_apply(.SD,30)[,5]),
                                                                                  as.numeric(cust_rolling_apply(.SD,30)[,6]),
                                                                                  as.numeric(cust_rolling_apply(.SD,30)[,7]),
                                                                                  as.numeric(cust_rolling_apply(.SD,30)[,8]),
                                                                                  as.numeric(cust_rolling_apply(.SD,30)[,9]),
                                                                                  as.numeric(cust_rolling_apply(.SD,30)[,10]),
                                                                                  as.numeric(cust_rolling_apply(.SD,30)[,11])
                                      ),by=.(SYMBOL),.SDcols=c("Daily_Return",
                                                               "Dow.DailyReturn",
                                                               "HengSeng.DailyReturn",
                                                               "Nasdaq.DailyReturn",
                                                               "Nikkei.DailyReturn",
                                                               "Nifty.DailyReturn",
                                                               "NSEIT.DailyReturn",
                                                               "NSEBANK.DailyReturn",
                                                               "EUR.DailyReturn",
                                                               "GBP.DailyReturn",
                                                               "JPY.DailyReturn",
                                                               "USD.DailyReturn")]




Sys.time()

Sys.time()
stock_data.2015 <- stock_data.2015[,c("betaDow","betaHK","betaNasDaq","betaNikkei",
                                      "betaNifty","betaNSEIT","betaNSEBANK",
                                      "betaEUR","BetaGBP","betaJPY","betaUSD"):=.(as.numeric(cust_rolling_apply(.SD,30)[,1]),
                                                                                  as.numeric(cust_rolling_apply(.SD,30)[,2]),
                                                                                  as.numeric(cust_rolling_apply(.SD,30)[,3]),
                                                                                  as.numeric(cust_rolling_apply(.SD,30)[,4]),
                                                                                  as.numeric(cust_rolling_apply(.SD,30)[,5]),
                                                                                  as.numeric(cust_rolling_apply(.SD,30)[,6]),
                                                                                  as.numeric(cust_rolling_apply(.SD,30)[,7]),
                                                                                  as.numeric(cust_rolling_apply(.SD,30)[,8]),
                                                                                  as.numeric(cust_rolling_apply(.SD,30)[,9]),
                                                                                  as.numeric(cust_rolling_apply(.SD,30)[,10]),
                                                                                  as.numeric(cust_rolling_apply(.SD,30)[,11])
                                      ),by=.(SYMBOL),.SDcols=c("Daily_Return",
                                                               "Dow.DailyReturn",
                                                               "HengSeng.DailyReturn",
                                                               "Nasdaq.DailyReturn",
                                                               "Nikkei.DailyReturn",
                                                               "Nifty.DailyReturn",
                                                               "NSEIT.DailyReturn",
                                                               "NSEBANK.DailyReturn",
                                                               "EUR.DailyReturn",
                                                               "GBP.DailyReturn",
                                                               "JPY.DailyReturn",
                                                               "USD.DailyReturn")]




Sys.time()

Sys.time()
stock_data.2016 <- stock_data.2016[,c("betaDow","betaHK","betaNasDaq","betaNikkei",
                                      "betaNifty","betaNSEIT","betaNSEBANK",
                                      "betaEUR","BetaGBP","betaJPY","betaUSD"):=.(as.numeric(cust_rolling_apply(.SD,30)[,1]),
                                                                                  as.numeric(cust_rolling_apply(.SD,30)[,2]),
                                                                                  as.numeric(cust_rolling_apply(.SD,30)[,3]),
                                                                                  as.numeric(cust_rolling_apply(.SD,30)[,4]),
                                                                                  as.numeric(cust_rolling_apply(.SD,30)[,5]),
                                                                                  as.numeric(cust_rolling_apply(.SD,30)[,6]),
                                                                                  as.numeric(cust_rolling_apply(.SD,30)[,7]),
                                                                                  as.numeric(cust_rolling_apply(.SD,30)[,8]),
                                                                                  as.numeric(cust_rolling_apply(.SD,30)[,9]),
                                                                                  as.numeric(cust_rolling_apply(.SD,30)[,10]),
                                                                                  as.numeric(cust_rolling_apply(.SD,30)[,11])
                                      ),by=.(SYMBOL),.SDcols=c("Daily_Return",
                                                               "Dow.DailyReturn",
                                                               "HengSeng.DailyReturn",
                                                               "Nasdaq.DailyReturn",
                                                               "Nikkei.DailyReturn",
                                                               "Nifty.DailyReturn",
                                                               "NSEIT.DailyReturn",
                                                               "NSEBANK.DailyReturn",
                                                               "EUR.DailyReturn",
                                                               "GBP.DailyReturn",
                                                               "JPY.DailyReturn",
                                                               "USD.DailyReturn")]




Sys.time()

Sys.time()
stock_data.2017 <- stock_data.2017[,c("betaDow","betaHK","betaNasDaq","betaNikkei",
                                      "betaNifty","betaNSEIT","betaNSEBANK",
                                      "betaEUR","BetaGBP","betaJPY","betaUSD"):=.(as.numeric(cust_rolling_apply(.SD,30)[,1]),
                                                                                  as.numeric(cust_rolling_apply(.SD,30)[,2]),
                                                                                  as.numeric(cust_rolling_apply(.SD,30)[,3]),
                                                                                  as.numeric(cust_rolling_apply(.SD,30)[,4]),
                                                                                  as.numeric(cust_rolling_apply(.SD,30)[,5]),
                                                                                  as.numeric(cust_rolling_apply(.SD,30)[,6]),
                                                                                  as.numeric(cust_rolling_apply(.SD,30)[,7]),
                                                                                  as.numeric(cust_rolling_apply(.SD,30)[,8]),
                                                                                  as.numeric(cust_rolling_apply(.SD,30)[,9]),
                                                                                  as.numeric(cust_rolling_apply(.SD,30)[,10]),
                                                                                  as.numeric(cust_rolling_apply(.SD,30)[,11])
                                      ),by=.(SYMBOL),.SDcols=c("Daily_Return",
                                                               "Dow.DailyReturn",
                                                               "HengSeng.DailyReturn",
                                                               "Nasdaq.DailyReturn",
                                                               "Nikkei.DailyReturn",
                                                               "Nifty.DailyReturn",
                                                               "NSEIT.DailyReturn",
                                                               "NSEBANK.DailyReturn",
                                                               "EUR.DailyReturn",
                                                               "GBP.DailyReturn",
                                                               "JPY.DailyReturn",
                                                               "USD.DailyReturn")]




Sys.time()

# merge data for each year into a single file 

beta_data.2017 <-fread("beta2017.csv", stringsAsFactors = FALSE,header = TRUE)
beta_data.2017$V1 <- NULL
beta_data.2017$V1 <- NULL
beta_data.2017 <- filter(beta_data.2017,substr(TIMESTAMP,1,4) != '2016')

beta_data.2016 <-fread("beta2016.csv", stringsAsFactors = FALSE,header = TRUE)
beta_data.2016$V1 <- NULL
beta_data.2016$V1 <- NULL
beta_data.2016 <- filter(beta_data.2016,substr(TIMESTAMP,1,4) != '2015')

beta_data.2015 <-fread("beta2015.csv", stringsAsFactors = FALSE,header = TRUE)
beta_data.2015$V1 <- NULL
beta_data.2015$V1 <- NULL
beta_data.2015 <- filter(beta_data.2015,substr(TIMESTAMP,1,4) != '2014')

beta_data.2014 <-fread("beta2014.csv", stringsAsFactors = FALSE,header = TRUE)
beta_data.2014$V1 <- NULL
beta_data.2014$V1 <- NULL
beta_data.2014 <- filter(beta_data.2014,substr(TIMESTAMP,1,4) != '2013')


beta_data.2013 <-fread("beta2013.csv", stringsAsFactors = FALSE,header = TRUE)
beta_data.2013$V1 <- NULL
beta_data.2013$V1 <- NULL
beta_data.2013 <- filter(beta_data.2013,substr(TIMESTAMP,1,4) != '2012')

beta_data.2012 <-fread("beta2012.csv", stringsAsFactors = FALSE,header = TRUE)
beta_data.2012$V1 <- NULL
beta_data.2012$V1 <- NULL

beta_data_all <- rbind(beta_data.2012,beta_data.2013)
beta_data_all <- rbind(beta_data_all,beta_data.2014)
beta_data_all <- rbind(beta_data_all,beta_data.2015)
beta_data_all <- rbind(beta_data_all,beta_data.2016)
beta_data_all <- rbind(beta_data_all,beta_data.2017)

write.csv(beta_data_all,"stock_beta_all.csv")
