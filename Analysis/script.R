setwd("Desktop/drawdown/Analysis/csv_data/")

######################### read in the data file #########################

assetsList <- c("AGG", "HYG", "TIP",
                "BCOM", "BUHY", "G0O1", "LTP5TRUU", "MXEA", "MXEF", "RAY", "RMZ", "SPX", "USGG10YR")

setAs("character","myDate", function(from) as.Date(from, format="%m/%d/%y") )
for (i in 1:3){
  assign(assetsList[i], 
         read.csv(paste(assetsList[i], "_US_Equity.csv", sep = ''), 
                  colClasses=c("myDate", "numeric")))
} # US equity * 3
for (i in 4:13){
  assign(assetsList[i], 
         read.csv(paste(assetsList[i], "_Index.csv", sep = ''), 
                  colClasses=c("myDate", "numeric")))
} # index * 10

# revise year from 20XX to 19XX
library(lubridate)
index = which(year(SPX$Date) > 2020)
year(SPX$Date[index]) = year(SPX$Date[index]) - 100
# plot(SPX)
index = which(year(USGG10YR$Date) > 2020)
year(USGG10YR$Date[index]) = year(USGG10YR$Date[index]) - 100
# plot(USGG10YR)

######################### calculate the returns #########################

for (i in assetsList){
  val <- get(i)
  len <- nrow(val)
  # daily returns
  retrn_dl <- c(0,(AGG$PX_LAST[2:len]-AGG$PX_LAST[1:(len-1)])/AGG$PX_LAST[1:(len-1)])
  # annalized returnss
  retrn_an <- (1+retrn_dl)^252-1
  assign(i, cbind(val, retrn_dl, retrn_an)) 
}


