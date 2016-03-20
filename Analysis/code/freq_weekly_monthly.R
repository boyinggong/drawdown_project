
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
plotRollingStat(FUN = "VaR", windw = "6mon", ymin = 0, ymax = 20, scale = 100, freq_option = "Weekly")

volatility1yrWeekly = getRollingDataFrame(windw = "1yr", FUN = "volatility", 
                                          assetData = assetDataWeekly, freq_option = "Weekly")
plotRollingStat(FUN = "volatility", windw = "1yr", ymin = 0, ymax = 1, freq_option = "Weekly")

volatility1yrWeekly = getRollingDataFrame(windw = "6mon", FUN = "volatility", 
                                          assetData = assetDataWeekly, freq_option = "Weekly")
plotRollingStat(FUN = "volatility", windw = "6mon", ymin = 0, ymax = 2, freq_option = "Weekly")

maxDrawdown3monWeekly = getMaxDdDataFrame(windw = "3mon", Data = assetDataWeekly)
plotMaxDrawdown(windw = "3mon", xmin = 0, xmax = 0.4, ymin = 0, ymax = 80, freq_option = "Weekly")

maxDrawdown6monWeekly = getMaxDdDataFrame(windw = "6mon", Data = assetDataWeekly)
plotMaxDrawdown(windw = "6mon", xmin = 0, xmax = 0.4, ymin = 0, ymax = 80, freq_option = "Weekly")

##############################
## analysis of monthly data ##
##############################

# ES & VaR

lvs <- c(.9, .95, .99)
resVaRMonthly <- matrix(rep(0, length(lvs)*length(assetDataMonthly)), ncol=length(lvs))
colnames(resVaRMonthly) <- lvs
rownames(resVaRMonthly) <- names(assetDataMonthly)

resESMonthly <- matrix(rep(0, length(lvs)*length(assetDataMonthly)), ncol=length(lvs))
colnames(resESMonthly) <- lvs
rownames(resESMonthly) <- names(assetDataMonthly)

for (asset in names(assetDataMonthly)){
  resVaRMonthly[asset, ] <- sapply(lvs, function(lv)VaR(assetDataMonthly[[asset]]$retrn_dl, p=lv))
  resESMonthly[asset, ] <- sapply(lvs, function(lv)ES(assetDataMonthly[[asset]]$retrn_dl, p=lv))
}


# ATTENTION: when we move to monthly data there are large possibilities that 
# the VaR and ES will be missing value!

# define period length for Monthly data
prds <- c(3, 6, 12, 24, 60, 3+60)
names(prds) <- c("3mon", "6mon", "1yr", "2yr", "5yr", "3mon5yr")

ES1yrMonthly = getRollingDataFrame(windw = "1yr", FUN = "ES", 
                                  assetData = assetDataMonthly , p = 0.95)
plotRollingStat(FUN = "ES", windw = "1yr", ymin = 0, ymax = 20, scale = 100, freq_option = "Monthly")

ES6monMonthly = getRollingDataFrame(windw = "6mon", FUN = "ES", 
                                   assetData = assetDataMonthly , p = 0.95)
plotRollingStat(FUN = "ES", windw = "6mon", ymin = 0, ymax = 20, scale = 100, freq_option = "Monthly")

VaR1yrMonthly = getRollingDataFrame(windw = "1yr", FUN = "VaR", 
                                   assetData = assetDataMonthly , p = 0.95)
plotRollingStat(FUN = "VaR", windw = "1yr", ymin = 0, ymax = 20, scale = 100, freq_option = "Monthly")

VaR6monMonthly = getRollingDataFrame(windw = "6mon", FUN = "VaR", 
                                    assetData = assetDataMonthly , p = 0.95)
plotRollingStat(FUN = "VaR", windw = "6mon", ymin = 0, ymax = 20, scale = 100, freq_option = "Monthly")

volatility1yrMonthly = getRollingDataFrame(windw = "1yr", FUN = "volatility", 
                                          assetData = assetDataMonthly, freq_option = "Monthly")
plotRollingStat(FUN = "volatility", windw = "1yr", ymin = 0, ymax = 1, freq_option = "Monthly")

volatility1yrMonthly = getRollingDataFrame(windw = "6mon", FUN = "volatility", 
                                          assetData = assetDataMonthly, freq_option = "Monthly")
plotRollingStat(FUN = "volatility", windw = "6mon", ymin = 0, ymax = 2, freq_option = "Monthly")

maxDrawdown3monMonthly = getMaxDdDataFrame(windw = "3mon", Data = assetDataMonthly)
plotMaxDrawdown(windw = "3mon", xmin = 0, xmax = 0.4, ymin = 0, ymax = 80, freq_option = "Monthly")

maxDrawdown6monMonthly = getMaxDdDataFrame(windw = "6mon", Data = assetDataMonthly)
plotMaxDrawdown(windw = "6mon", xmin = 0, xmax = 0.4, ymin = 0, ymax = 80, freq_option = "Monthly")

#################################
## serial correlation anslysis ##
#################################

serialCorOrder1 = rep(0, length(assetData))
names(serialCorOrder1 ) = names(assetData)
for (asset in names(assetData)){
  serialCorOrder1[asset] = serialCorrelation(assetData[[asset]]$retrn_dl, order = 1)
}

serialCorOrder2 = rep(0, length(assetData))
names(serialCorOrder2 ) = names(assetData)
for (asset in names(assetData)){
  serialCorOrder2[asset] = serialCorrelation(assetData[[asset]]$retrn_dl, order = 2)
}

serialCorOrder1Weekly = rep(0, length(assetData))
names(serialCorOrder1Weekly ) = names(assetData)
for (asset in names(assetData)){
  serialCorOrder1Weekly[asset] = serialCorrelation(assetDataWeekly[[asset]]$retrn_dl, order = 1)
}

serialCorOrder2Weekly = rep(0, length(assetData))
names(serialCorOrder2Weekly ) = names(assetData)
for (asset in names(assetData)){
  serialCorOrder2Weekly[asset] = serialCorrelation(assetDataWeekly[[asset]]$retrn_dl, order = 2)
}

serialCor_df = cbind(serialCorOrder1, serialCorOrder2, 
                     serialCorOrder1Weekly, serialCorOrder2Weekly )

