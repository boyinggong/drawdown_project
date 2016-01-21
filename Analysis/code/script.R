setwd("/Users/jorothygong/Desktop/drawdown/Analysis/csv_data/")
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
#plot(USGG10YR)

G0O1 <- G0O1[which(G0O1$Date == as.Date("1992-04-01")):nrow(G0O1), ]
rownames(G0O1) <- NULL
LTP5TRUU <- LTP5TRUU[which(LTP5TRUU$Date == as.Date("2010-06-03")):nrow(LTP5TRUU), ]
rownames(LTP5TRUU) <- NULL

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

for (i in assetsList){
  val <- get(i)
  print(i)
  print(which(val$PX_LAST == 0))    # check if there were missing values
}

######################### plot the returns #########################

par(mfrow=c(3,5))

for (i in assetsList){
  val <- get(i)
  plot(val$Date[2:length(val$Date)], val$retrn_dl[2:length(val$Date)],
       main = i,
       xlab = "Date",
       ylab = "Return")
}

dev.off()

######################### calculate the Sharpe Ratio, #########################
################### standard deviation, skewness, kurtosis ####################

statSmmr <- matrix(rep(0, 4*length(assetsList)), nrow=length(assetsList))
rownames(statSmmr) <- assetsList
colnames(statSmmr) <- c("SR","sd","skewness","kurtosis")

for (i in assetsList){
  val <- get(i)
  dt <- as.data.frame(val$retrn_dl)
  rownames(dt) <- val$Date
  # statSmmr[i, 1] <- SharpeRatio(dt, Rf = 0)
  statSmmr[i, 2] <- sd(val$retrn_dl)
  statSmmr[i, 3] <- skewness(val$retrn_dl) # method?
  statSmmr[i, 4] <- kurtosis(val$retrn_dl)
  Rf = 0
  statSmmr[i, 1] <- (mean(val$retrn_dl) - Rf)/ statSmmr[i, 2]
} 

######################### calculate the VaR & ES #########################

VaR <- function(R, p = 0.95){
  quantile(R, probs = 1-p)
}

ES <- function(R, p = 0.95){
  mean(R[R < quantile(R, probs = 1-p)])
}

microbenchmark(VaR(AGG$retrn_dl, p = 0.95), 
               PerformanceAnalytics::VaR(AGG$retrn_dl, p = 0.95))

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
  resVaR[count, ] <- sapply(lvs, function(lv)VaR(val$retrn_dl, p=lv, method="historical"))
  resES[count, ] <- sapply(lvs, function(lv)ES(val$retrn_dl, p=lv, method="historical"))
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

calcCDD <- function(lv, prd){
  sapply(2:(nrow(val)-prd+1), function(x){
    dt <- as.data.frame(val$retrn_dl[x:(x+prd-1)])
    rownames(dt) <- val$Date[x:(x+prd-1)] 
    CDD(dt, p=lv)
  })
}

for (i in assetsList){
  val <- get(i)
  resCDDs <- lapply(prds, function(y){
    CDDs <- sapply(lvs, function(x)calcCDD(x, y))
    colnames(VaRs) <- lvs
    return(as.data.frame(CDDs))
  })
  WriteXLS(resCDDs, ExcelFileName = paste(i, ".xls", sep=''))
}

######################### calculate the CED #########################

# 3 month: 63 days
# 6 month: 126 days
# 1 year: 252 days
# 2 years: 504 days
# 5 years: 1260 days
# calculation are based on annualized returns

lvs <- c(.9, .95, .99)
prds <- c(63, 126, 252, 504, 1260)
names(prds) <- c("mon3", "mon6", "yr1", "yr2", "yr5")

calcCED <- function(val, prd){
  sapply(2:(nrow(val)-prd+1), function(x){
    dt <- as.data.frame(val$retrn_dl[x:(x+prd-1)])
    rownames(dt) <- val$Date[x:(x+prd-1)] 
    maxDrawdown(dt)
  })
}

prd = prds[1]

for (prd in prds){
  resCEDs <- matrix(rep(0, length(assetsList)*length(lvs)), ncol = length(lvs))
  resCEDs <- as.data.frame(resCEDs)
  rownames(resCEDs) <- assetsList
  colnames(resCEDs) <- lvs
  
  for (i in assetsList){
    val <- get(i)
    mxd <- calcCED(val, prd)
    resCEDs[i, ] <- sapply(lvs, function(lv)ES(-mxd, p=lv, method="historical"))
    print(i)
  }
  write.csv(resCEDs, file = paste(names(prd), "_CED.csv", sep = ''))
}

################# time varying volatilities and sharpe ratio #################

######################### time varying ES and VaR #########################

