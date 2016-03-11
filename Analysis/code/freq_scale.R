
library(ggplot2)
library(grid)
library(gridExtra)

source('read_data.R')

#######################################
## aggregate monthly and weekly data ##
#######################################

library(xts)

# aggregate data by week

assetDataWeekly = list()
for (asset in names(assetData)){
  data <- as.xts(assetData[[asset]]$retrn_dl, 
                 order.by=assetData[[asset]]$Date)
  temp = as.data.frame(
    apply.weekly(data, function(x){prod(1+x) - 1}))
  assetDataWeekly[[asset]] = data.frame(Date = as.Date(row.names(temp)),
                              retrn_dl = temp$V1)
}

# delete the first row in case of incomplete data

for (asset in names(assetDataWeekly)){
  assetDataWeekly[[asset]] = assetDataWeekly[[asset]][c(-1), ]
}

# aggregate data by month

assetDataMonthly = list()
for (asset in names(assetData)){
  data <- as.xts(assetData[[asset]]$retrn_dl, 
                 order.by=assetData[[asset]]$Date)
  temp = as.data.frame(
    apply.monthly(data, function(x){prod(1+x) - 1}))
  assetDataMonthly[[asset]] = data.frame(Date = as.Date(row.names(temp)),
                                        retrn_dl = temp$V1)
}

# delete the first row in case of incomplete data

for (asset in c("AGG", "HYG", "TIP", "G0O1", "MXEA", "RMZ")){
  assetDataMonthly[[asset]] = assetDataMonthly[[asset]][c(-1), ]
}

#############################
## analysis of weekly data ##
#############################

# ES & VaR

lvs <- c(.9, .95, .99)
resVaRWeekly <- matrix(rep(0, length(lvs)*length(assetDataWeekly)), ncol=length(lvs))
colnames(resVaRWeekly) <- lvs
rownames(resVaRWeekly) <- names(assetDataWeekly)

resESWeekly <- matrix(rep(0, length(lvs)*length(assetDataWeekly)), ncol=length(lvs))
colnames(resESWeekly) <- lvs
rownames(resESWeekly) <- names(assetDataWeekly)

for (asset in names(assetDataWeekly)){
  resVaRWeekly[asset, ] <- sapply(lvs, function(lv)VaR(assetDataWeekly[[asset]]$retrn_dl, p=lv))
  resESWeekly[asset, ] <- sapply(lvs, function(lv)ES(assetDataWeekly[[asset]]$retrn_dl, p=lv))
}

# define period length for weekly data
prds <- c(13, 26, 54, 108, 260, 13+260)
names(prds) <- c("3mon", "6mon", "1yr", "2yr", "5yr", "3mon5yr")

ES1yrWeekly = getRollingDataFrame(windw = "1yr", FUN = "ES", 
                                  assetData = assetDataWeekly , p = 0.95)
plotRollingStat(FUN = "ES", windw = "1yr", ymin = 0, ymax = 20, scale = 100, freq_option = "Weekly")

ES6monWeekly = getRollingDataFrame(windw = "6mon", FUN = "ES", 
                                   assetData = assetDataWeekly , p = 0.95)
plotRollingStat(FUN = "ES", windw = "6mon", ymin = 0, ymax = 20, scale = 100, freq_option = "Weekly")

VaR1yrWeekly = getRollingDataFrame(windw = "1yr", FUN = "VaR", 
                                   assetData = assetDataWeekly , p = 0.95)
plotRollingStat(FUN = "VaR", windw = "1yr", ymin = 0, ymax = 20, scale = 100, freq_option = "Weekly")

VaR6monWeekly = getRollingDataFrame(windw = "6mon", FUN = "VaR", 
                                    assetData = assetDataWeekly , p = 0.95)
plotRollingStat(FUN = "VaR", windw = "6mon", ymin = 0, ymax = 12, scale = 100, freq_option = "Weekly")

maxDrawdown3monWeekly = getMaxDdDataFrame(windw = "3mon", Data = assetDataWeekly)
plotMaxDrawdown(windw = "3mon", xmin = 0, xmax = 0.4, ymin = 0, ymax = 80, freq_option = "Weekly")










