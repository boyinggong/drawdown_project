library(plyr)
library("PerformanceAnalytics")
library(ggplot2)
library(grid)
library(gridExtra)
lvs <- c(.9, .95, .99)
prds <- c(63, 126, 252, 504, 1260)
names(prds) <- c("3mon", "6mon", "1yr", "2yr", "5yr")
AR_para <- seq(0,5)
drift <- seq(0,1)
MA_para <- seq(0,5)

######################### Find the best model given a set of parameter ############################ 
paras_arima <- as.matrix(expand.grid(AR = AR_para, DRIFT = drift, MA = MA_para))[-1, ]
paras_arma <- as.matrix(expand.grid(AR =AR_para, DRIFT = 0, MA = MA_para))[-1, ]

FindBestModel <- function(vec,paras){
  aics <-apply(paras, 1, function(i){arima(vec, order =unlist(i))$aic})
  return(paras[which.min(aics),])
}
best_orders_arima = lapply(assetsList, function(i){FindBestModel(get(i)$retrn_dl, paras_arima)})
best_orders_arma = lapply(assetsList, function(i){FindBestModel(get(i)$retrn_dl, paras_arma)})
FindBestModel(AGG$retrn_dl, paras_arma)

# we find the that we have to take difference to make the series to be stationary.
############ Calculate the serial Correlation based on best parameters #############
### RMZ
par(mfrow= c(2,2))
acf(RMZ$retrn_dl)
pacf(RMZ$retrn_dl)
acf(diff(RMZ$retrn_dl))
pacf(diff(RMZ$retrn_dl))
# From the above plots, I will think ARIMA(0,1,1) or ARMA(1,0,1) is the best for fitting
# Well, I will first fit AR(1) here to be consistent with paper.
#draw plots for short term ts for RMZ
par(mfrow = c(3,2))
for (i in 1:10){
  acf(AGG[(i*265-26:i*265),3])
  pacf (AGG[(i*265-26:i*265),3])
}
# I find it is reasonable to fit MA(1) Model in short terms(like 1year).

# Therefore, for this specific asset, I will fit ARIMA(0,1,1), ARMA(1,1), MA(1). I will
# fit AR(1) model, trying to consistent with the plot on the paper.


## fit AR(1) for all data. See the type
CalcSerial <- function(indx,paras=c(1,0,0)){
  return_vec <- ts(indx$retrn_dl)
  fit <- arima(return_vec, order = unlist(paras))
  return(fit$coef[1])
}
ser_RMZ3m_ALL <- CalcSerial(RMZ)
par(mfrow = c(1,1))
plot(ser_RMZ3m,cex=.2, type = "l")


tmp <- arima(RMZ$retrn_dl, c(2,0,3))
ARMAacf()
#################### Find serial correlation theratically #########################
#suppose we have known the best model
calcRolling <- function(val, prd, FUN, ...){
  res <- sapply(2:(nrow(val)-prd+1), function(x){
    #browser()
    dt <- as.data.frame(val$retrn_dl[x:(x+prd-1)])
    rownames(dt) <- val$Date[x:(x+prd-1)] 
    do.call(FUN, list(dt[, 1], ...))
  })
}

CalcTrtcAcf <- function(vec, best_para) {
  return_vec <- ts(vec)
  fit <- arima(return_vec, order = unlist( best_para), method="ML")
  if (best_para[1] == 0){
    ar_vec = numeric()
  } else {
    ar_vec = fit$coef[1:best_para[1]]
  } 
  if (best_para[3] == 0){
    ma_vec = numeric()
  }else {
    ma_vec = fit$coef[(best_para[1]+1):(best_para[1]+best_para[2])]
  }
  return (ARMAacf(ar_vec,ma_vec))
}

SerCol<- function(vec,best_para){
  return(CalcTrtcAcf(vec,  best_para)[2])
}

# ser_RMZ3m<-calcRolling(RMZ,prds[1],SerCol,best_para = c(0,0,1))
# AR(1) for all the indices
prds <- c(63, 126, 252, 504, 1260,1323)
names(prds) <- c("3mon", "6mon", "1yr", "2yr", "5yr","5yr3mon")

FUN = "SerCol"
windw = "mon3"
paras = "AR1"
assign(paste(FUN, windw,paras, sep = '-'), 
       lapply(assetsList, function(i){
         calcRolling(get(i),prds[windw],SerCol,best_para = c(1,0,0))}))

FUN = "SerCol"
windw = "5yr3mon"
paras = "AR1"

FUN = "SerCol"
windw = "5yr"
paras = "MA1"

FUN = "SerCol"
windw = "5yr3mon"
paras = "MA1"

assign(paste(FUN, windw,paras, sep = '-'), 
       lapply(assetsList, function(i){
         calcRolling(get(i),prds[windw],SerCol,best_para = c(0,0,1))}))

FUN = "SerCol"
windw = "5yr"
paras = "ARMA11"

FUN = "SerCol"
windw = "5yr3mon"
paras = "ARMA11"
assign(paste(FUN, windw,paras, sep = '-'), 
       lapply(assetsList, function(i){
         calcRolling(get(i),prds[windw],SerCol,best_para = c(1,0,1))}))

############### just a individual model selection for RMZ #########################
# find best model within the potential parameter sets
par(mfrow = c(2,2))
acf(RMZ$retrn_dl, main = "RMZ: ACF of Return")
acf(RMZ$retrn_dl, main = "RMZ: PACF of Return")
acf(diff(RMZ$retrn_dl), main = "RMZ: ACF of First-order Differencing Return")
acf(diff(RMZ$retrn_dl), main = "RMZ: PACF of First-order Differencing Return")

fitRMZ_MA <- arima(RMZ$retrn_dl, c(0,0,1))
tsdiag(fitRMZ_MA)

RMZ_potential_para <- matrix(c(1,0,0,0,0,1,1,0,1,0,1,1), byrow = TRUE, ncol = 3)
FindBestModel(RMZ$retrn_dl, RMZ_potential_para)

#what do we care for models other than AR(1)
# MA(1)
ser_RMZ3m<-calcRolling(RMZ,prds[1],ColSer)

# ARMA(1,1)
ser_RMZ3m<-calcRolling(RMZ,prds[1],ColSer)

# ARIMA(0,1,1)
ser_RMZ3m<-calcRolling(RMZ,prds[1],ColSer)

par(mfrow = c(1,1))
plot(ser_RMZ3m,cex=.2, type = "l")

#################### 5yr with VaR, ES, and CED #################
library(ggplot2)
windw = "5yr"
RiskMs = "VaR"
paras = "ARMA11"
count = 1
png(paste("Analysis/results/SerCol-",RiskMs, windw, paras, ".png", sep = ''), width = 1600, height = 1600)
plots = list()
for (i in assetsList){
  plt <- data.frame(y_axis = get(paste("SerCol", windw, paras, sep = '-'))[[count]],
                    x_axis = get(paste(RiskMs, windw ,sep = ''))[[count]])
  plots[[i]] <- ggplot(plt, aes(x=x_axis, y=y_axis)) +
    geom_point() +
    ggtitle(i) + 
    labs(y = paste("Serial Correlation")) + 
    #ylim(0, 1) +
    labs(x = RiskMs)
  count = count+1
}
multiplot(plotlist = plots, cols = 3)
dev.off()

windw = "5yr"
RiskMs = "ES"
count = 1
png(paste("Analysis/results/SerCol-", RiskMs, windw, paras,".png", sep = ''), width = 1600, height = 1600)
plots = list()
for (i in assetsList){
  plt <- data.frame(y_axis = get(paste("SerCol", windw, paras, sep = '-'))[[count]],
                    x_axis = get(paste(RiskMs, windw ,sep = ''))[[count]])
  plots[[i]] <- ggplot(plt, aes(x=x_axis, y=y_axis)) +
    geom_point() +
    ggtitle(i) + 
    labs(y = paste("Serial Correlation")) + 
    #ylim(0, 1) +
    labs(x = RiskMs)
  count = count+1
}
multiplot(plotlist = plots, cols = 3)
dev.off()


#calc CED 5y
tmp <- seq(1,11)
maxDrawdownmon3 <- lapply(tmp,
                          function(i)read.csv(paste("/Users/Xinyue_star/drawdown_project/Analysis/results/maxDrawdowns/",i, "_mon3.csv",sep=""), header=TRUE))

maxDrawdownmon3_date <- lapply(maxDrawdownmon3, function(i) as.data.frame(i)$Date)
maxDrawdownmon3_Dd <- lapply(maxDrawdownmon3, function(i) as.data.frame(i)$Dd)


windw = "5yr"
FUN = "ES"
calcCED <- function(val, prd, p = 0.9){
  res <- sapply(1:(length(val)-prd+1), function(x){
    dt <- val[x:(x+prd-1)]
    mean(dt[dt > quantile(dt, probs = p)])
  })
}


# calcCED <- function(val, lv, prd, FUN, ...){
#   res <- sapply(2:(length(val)-prd+1), function(x){
#     dt <- val[x:(x+prd-1)]
#     do.call(FUN, list(dt, lv))
#   })
# }

CED5yr3mon <- lapply(maxDrawdownmon3_Dd, function(i)-calcCED(i, prd = prds[windw]))
CED5yr3mon_date <- lapply(maxDrawdownmon3_date, function(i)i[(1+prds[windw]):(length(i))])



windw = "5yr3mon"
RiskMs = "CED"
count = 1
png(paste("Analysis/results/SerCol-",RiskMs, windw, paras, ".png", sep = ''), width = 1600, height = 1600)
plots = list()
for (i in assetsList){
  plt <- data.frame(y_axis = get(paste("SerCol", windw, paras, sep = '-'))[[count]],
                    x_axis = get(paste(RiskMs, windw ,sep = ''))[[count]])
  plots[[i]] <- ggplot(plt, aes(x=x_axis, y=y_axis)) +
    geom_point() +
    ggtitle(i) + 
    labs(y = paste("Serial Correlation")) + 
    #ylim(0, 1) +
    labs(x = RiskMs)
  count = count+1
}
multiplot(plotlist = plots, cols = 3)
dev.off()


length(get(paste(RiskMs, windw ,sep = ''))[[count]])
length(get(paste("SerCol", windw, paras, sep = '-'))[[count]])



################ calculate the correlations ##################
calc_corr <- function(measure1, measure2, period1, period2){
  len_asset_list <- length(get(paste(measure1, period1, sep = '')))
  corr_vec <- rep(0, len_asset_list)
  for (i in 1:len_asset_list){
    corr_vec[i] = cor(get(paste(measure1, period1, sep = ''))[[i]], 
                      get(paste(measure2, period2, sep = ''))[[i]])
  }
  return(corr_vec)
}

# calc_corr("VaR", "ES", "1yr", "1yr")[c(1:4, 6:11)]
calc_corr("VaR", "SerCol-", "5yr", "5yr-AR1")[c(1:4, 6:11)]
calc_corr("ES", "SerCol-", "5yr", "5yr-AR1")[c(1:4, 6:11)]
calc_corr("CED", "SerCol-", "5yr3mon", "5yr3mon-AR1")[c(1:4, 6:11)]

calc_corr("VaR", "SerCol-", "5yr", "5yr-MA1")[c(1:4, 6:11)]
calc_corr("ES", "SerCol-", "5yr", "5yr-MA1")[c(1:4, 6:11)]
calc_corr("CED", "SerCol-", "5yr3mon", "5yr3mon-MA1")[c(1:4, 6:11)]


# 
# windw = "3mon"
# FUN = "ColSer"
# 
# tmp<- function(i){
#   calcRolling(get(i),prds[1],ColSer,para = best_para[i])
# }
# assign(paste(FUN, windw, sep = ''), 
#        lapply(assetsList, function(i)sqrt(prds["1yr"])*sqrt(calcRolling(get(i), prd = prds[windw], FUN))))
# 
# ser_agg3m<-calcRolling(AGG,prds[1],ColSer)
# CalcRollingSerial(AGG$retrn_dl)
# 
# windw = "yr5"
# FUN = "kappa"
# assign(paste(FUN, windw, sep = ''), 
#        lapply(assetsList, function(i)calcRolling(get(i), prd = prds[windw],CalcSerial)))
# 
# 
# 
# #suppose df is our dataframe of CED, with the same time period
# 
# par(mfrow=c(2,2))
# tmp<- read.csv("/Users/Xinyue_star/drawdown_project/Analysis/results/maxDrawdowns/1_mon3.csv", header=TRUE)
# max_agg3m <- tmp[,2]
# 
# 
# date_agg <- AGG["Date"][-(1:prds[1]),]
# plot(date_agg,ser_agg3m,type="l",main="Kappa of AGG, 3 month", xlab="Date",ylab="kappa",col="pink")
# plot(max_agg3m,ser_agg3m, main="kappa and Maximum Drawdown", ylab="kappa", xlab="Maximum Drawdown",cex=.2,col="pink")
# cor(agg3m, max_agg3m)
# 
# #Var3m
# var_agg3m <-unlist(VaR3mon[1])
# plot(var_agg3m,ser_agg3m, main="kappa and VaR", ylab="kappa", xlab="VaR",cex=.2,col="pink")
# 
# #ES3m
# es_agg3m <-unlist(ES3mon[1])
# plot(es_agg3m,ser_agg3m, main="kappa and ES", ylab="kappa", xlab="ES",cex=.2,col="pink")
# 
# #CED
# c(length(ced_agg3m), length(agg3m))
# plot(agg3m, log(ced_agg3m), main="serial and CED", xlab="Serial", ylab="CED")
# cor(agg3m, ced_agg3m)
# 
# 
# ################ calculate the ACF without fitting the model ################
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# ser3m(returns_AGG)
# 
# returns_AGG = AGG$retrn_dl
# date_AGG = as.Date(AGG$Date)
# return_ts <- ts(returns_AGG)
# fit<- arima(return_ts, order=c(1,0,0))
# summary(fit)
# return_ts[1:20]
# typeof(return_ts)