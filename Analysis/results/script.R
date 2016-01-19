setwd("Desktop/drawdown/Analysis/csv_data/")
library("PerformanceAnalytics")

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

# annual returns are based on 252 bussiness days

for (i in assetsList){
  val <- get(i)
  len <- nrow(val)
  # daily returns
  retrn_dl <- c(0,(val$PX_LAST[2:len]-val$PX_LAST[1:(len-1)])/val$PX_LAST[1:(len-1)])
  # annalized returnss
  retrn_an <- (1+retrn_dl)^252-1
  assign(i, cbind(val, retrn_dl, retrn_an)) 
}

######################### calculate the VaR & ES #########################

lvs <- c(.9, .95, .99)
resVaR <- matrix(rep(0, length(lvs)*length(assetsList)), ncol=length(lvs))
colnames(resVaR) <- lvs
rownames(resVaR) <- assetsList

resES <- matrix(rep(0, length(lvs)*length(assetsList)), ncol=length(lvs))
colnames(resES) <- lvs
rownames(resES) <- assetsList

count <- 1

for (i in assetsList){
  val <- get(i)
  resVaR[count, ] <- sapply(lvs, function(lv)VaR(val$retrn_an, p=lv, method="historical"))
  resES[count, ] <- sapply(lvs, function(lv)ES(val$retrn_an, p=lv, method="historical"))
  count = count + 1
}

######################### calculate the CDAR #########################

# 3 month: 63 days
# 6 month: 126 days
# 1 year: 252 days
# 2 years: 504 days
# 5 years: 1260 days
# calculation are based on annualized returns

lvs <- c(.9, .95, .99)
prds <- c(63, 126, 252, 504, 1260)
names(prds) <- c("mon3", "mon6", "yr1", "yr2", "yr5")

calcCDAR <- function(lv, prd){
  sapply(2:(nrow(val)-prd+1), function(x){
    VaR(val$retrn_an[x:(x+prd-1)], p=lv, method="historical")
  })
}

for (i in assetsList){
  val <- get(i)
  resVaRs <- lapply(prds, function(y){
    VaRs <- sapply(lvs, function(x)calcVaR(x, y))
    colnames(VaRs) <- lvs
    return(as.data.frame(VaRs))
  })
  WriteXLS(resVaRs, ExcelFileName = paste(i, ".xls", sep=''))
}

resVaRs <- lapply(prds, function(y){
  VaRs <- sapply(lvs, function(x)calcVaR(x, y))
  colnames(VaRs) <- lvs
  return(as.data.frame(VaRs))
})
WriteXLS(resVaRs, ExcelFileName = paste(i, ".xls", sep=''))

# VaRs <- apply(expand.grid(lv = lvs, prd = prds), 1, 
#               function(xx)do.call(calcVaR, as.list(xx)))
# lstNm <- apply(expand.grid(lvs, names(prds)), 1, 
#                function(x)paste(x[1], x[2], sep = "_"))
# names(VaRs) <- lstNm
