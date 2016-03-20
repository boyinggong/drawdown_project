library(plyr)
library("PerformanceAnalytics")
library(ggplot2)
library(grid)
library(gridExtra)
#install.packages("Rmis")
library(Rmisc)
library(forecast)
lvs <- c(.9, .95, .99)
names(prds) <- c("3mon", "6mon", "1yr", "2yr", "5yr")
################################
#### The best order of ARMA ####
################################
bestorders_single <- function(asset){
  para = auto.arima(asset$retrn_dl)$arma
  return(c(para[1],para[6], para[2]))
}

bestorders_all<- function(asset_list){
  return(lapply(asset_list, bestorders_single))
}


#bestorders_single(assetData[[1]])
bestorders = bestorders_all(assetData)
###############################################
#### Self Defined Func Only looking at AIC ####
###############################################
AR_para <- seq(0,5)
drift <- seq(0,1)
MA_para <- seq(0,5)
paras_arima <- as.matrix(expand.grid(AR = AR_para, DRIFT = drift, MA = MA_para))[-1, ]
paras_arma <- as.matrix(expand.grid(AR =AR_para, DRIFT = 0, MA = MA_para))[-1, ]

FindBestModel <- function(vec,paras){
  aics <-apply(paras, 1, function(i){arima(vec, order =unlist(i))$aic})
  return(paras[which.min(aics),])
}
best_orders_arima = lapply(assetsList, function(i){FindBestModel(get(i)$retrn_dl, paras_arima)})
best_orders_arma = lapply(assetsList, function(i){FindBestModel(get(i)$retrn_dl, paras_arma)})
FindBestModel(AGG$retrn_dl, paras_arma)

######################################
#### Calculate Serial Correlation ####
######################################
CalcTrtcAcf <- function(vec, best_para) {
  if (best_para[1]== 0 & best_para[3] ==0){ return (0)}
  return_vec <- ts(vec)
  fit <- arima(return_vec, order = unlist(best_para), method="ML")
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


SerCol_single_period<- function(vec,best_para,ser_order ){
  return(CalcTrtcAcf(vec,  best_para)[ser_order+1])
}

# SerCol_single_period(assetData$RMZ$retrn_dl,c(0,0,1),ser_order = 1)

SerCol_single <- function(asset,windw ,bestorder, order = 1){
  calcRolling(asset, prds[windw], SerCol_single_period, best_para = bestorder, ser_order = order)
}

SerCol_all <- function(assetList, windw, bestorders, order = 1){
  ret = lapply(names(assetList), function(i){
    print(i)
    myorder = bestorders[[i]]
    if (myorder[1]==0 & myorder[3]==0){
      myorder = c(1,0,0)
    }
    #browser()
    print(myorder)
    SerCol_single(assetList[[i]],windw, myorder, order = order)})
  return(ret)
}


# there is error in bestorder of MXEF, I find the second best one based on AIC. it is (3,0,0)
bestorders$MXEF = c(3,0,0)
# Moreover it is just impossible for G0O1 to compute sercol using (1,1,1), so I just calculate (1,0,0)
bestorders$G0O1 = c(1,0,0)
bestorders$MXEA = c(0,0,2)
bestorders$SPX = c(0,0,2)
#   
# a1 = SerCol_all(assetData[1], "2yr", bestorders[1], order = 1)
# 
# a11 = SerCol_all(assetData[11], "2yr", bestorders[11], order = 1)
# 
# a2_4 = SerCol_all(assetData[1:4], "2yr", bestorders[1:4], order = 1)
# a5 = SerCol_all(assetData[5], "2yr", bestorders[5], order = 1)
# 
# a6 = SerCol_all(assetData[6], "2yr", bestorders[6], order = 1)
# a7 = SerCol_all(assetData[7], "2yr", bestorders[7], order = 1)
# a8 = SerCol_all(assetData[8], "2yr", bestorders[8], order = 1)
# a9 = SerCol_all(assetData[9], "2yr", bestorders[9], order = 1)
# a10 = SerCol_all(assetData[10], "2yr", bestorders[10], order = 1)
# 
# SerCol_2y = a2_4
# SerCol_2y[5] = a5
# SerCol_2y[6] = a6
# SerCol_2y[7] = a7
# SerCol_2y[8] = a8
# SerCol_2y[9] = a9
# SerCol_2y[10] = a10
# SerCol_2y[11] = a11
# 
# length(SerCol_2y)
# # names(SerCol_2y)
# names(SerCol_2y) = names(assetData)

SerCol2yr = SerCol_2y
#rm(SerCol2y)
sapply(SerCol2y,length)
sapply(ES2yr,length)
# Calculate 2 year VaR ES
assign(paste(FUN = "ES", windw = "2yr", sep = ''), 
       getRollingDataFrame(windw = "2yr", FUN = "ES", assetData = assetData , p = 0.95))
assign(paste(FUN = "VaR", windw = "2yr", sep = ''), 
       getRollingDataFrame(windw = "2yr", FUN = "VaR", assetData = assetData, p = 0.95))



storeit = SerCol3mon2yr
length(SerCol3mon2yr)
SerCol3mon2yr = SerCol_all(assetData[1:5], "3mon2yr", bestorders[1:5],order = 1)
b6 = SerCol_all(assetData[6], "3mon2yr", bestorders[6],order = 1)
b7 = SerCol_all(assetData[7], "3mon2yr", bestorders[7],order = 1)
b8 = SerCol_all(assetData[8], "3mon2yr", bestorders[8],order = 1)
b9 = SerCol_all(assetData[9], "3mon2yr", bestorders[9],order = 1)
b10 = SerCol_all(assetData[10], "3mon2yr", bestorders[10],order = 1)
b11 = SerCol_all(assetData[11], "3mon2yr", bestorders[11],order = 1)
SerCol3mon2yr[6] = b6
SerCol3mon2yr[7] = b7
SerCol3mon2yr[8] = b8
SerCol3mon2yr[9] = b9
SerCol3mon2yr[10] = b10
SerCol3mon2yr[11] = b11
names(SerCol3mon2yr) = names(assetData)

sapply(SerCol3mon2yr, length)
sapply(CED3mon2yr,nrow)
# see = ES2yr[[1]]
# rm(see)
#plot 
windw = "3mon2yr"
RiskMs = "CED"
paras = bestorders
count = 1
png(paste("Analysis/figures/SerCol-",RiskMs, windw, ".png", sep = ''), width = 1600, height = 1600)
plots = list()
for (i in names(assetData)){
  plt <- data.frame(y_axis = get(paste("SerCol", windw, sep = ''))[[i]],
                    x_axis = get(paste(RiskMs, windw ,sep = ''))[[i]]$values)
  plots[[i]] <- ggplot(plt, aes(x=x_axis, y=y_axis)) +
    geom_point() +
    ggtitle(paste(i," (",bestorders[i],")",sep="")) + 
    labs(y = paste("Serial Correlation")) + 
    #ylim(0, 1) +
    labs(x = RiskMs)
  count = count+1
}
multiplot(plotlist = plots, cols = 3)
dev.off()




calc_corr <- function(measure1, measure2){
  len_asset_list <- length(measure1)
  corr_vec <- rep(0, len_asset_list)
  names(corr_vec) = names(assetData)
  for (asset in names(assetData)){
    corr_vec[asset] = cor(measure1[[asset]]$values, measure2[[asset]])
  }
  return(corr_vec)
}

calc_corr(VaR2yr, SerCol2yr)[c(1:4, 6:11)]
calc_corr(ES2yr, SerCol2yr)[c(1:4, 6:11)]
calc_corr(CED3mon2yr, SerCol3mon2yr)[c(1:4, 6:11)]

length(VaR2yr)
length(SerCol2yr)
# residual left after arima
resids = lapply(1:11,
                function(i) arima(get(assetsList[i])$retrn_dl, order=unlist(bestorders[i]), include.mean = FALSE)$residuals)


