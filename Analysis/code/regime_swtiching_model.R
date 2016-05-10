
source("read_data.R")

library(dynlm)
library(MSwM)

########################################
####### using lag term in msmFit #######
########################################

val = "SPX"
lm.val = lm(retrn_dl ~ 1, assetData[[val]])
mod.mswm=msmFit(lm.val, k=2, p=1, sw=c(F, T, T), control=list(parallel=F))
summary(mod.mswm)
class(mod.mswm)

plotProb(mod.mswm,which=1)
plotProb(mod.mswm,which=2)
intervals(mod.mswm)
AIC(mod.mswm)
plotDiag(mod.mswm)

mod.mswm["Fit"]["smoProb"][12500:13500, 1] > 0.5

########################################
########### Fit all data set ###########
########################################

models = list()
for (asset in names(assetData)){
  lm.val = lm(retrn_dl ~ 1, assetData[[asset]])
  models[[asset]] = msmFit(lm.val, k=2, p=1, sw=rep(T, 3), control=list(parallel=F))
}

# print the summary of every asset

for (asset in names(assetData)){
  models[[asset]]
}

models[["SPX"]]

# print the coefficient of the model

for (asset in names(assetData)){
  cat(paste(asset, format(round(models[[asset]]@Coef$retrn_dl_1, 3), nsmall = 3),
            sep = " & ", collapse = " "))
  cat(" \\\\ \n")
}

###########################################
#### plot the regimes for every asset #####
###########################################

for (asset in names(assetData)){
  png(paste("../figures/regime_switching/", asset, ".png", sep = ''), width = 600, height = 600)
  plotProb(models[[asset]],which=2)
  dev.off()
}

###########################################
#### create two list for returns of  ######
##### regime 1 and regimw 2 separately ####
###########################################

# boolean vector of regime
# AGG GO01 RAY USGG10YR

regime.ind = list()
for (asset in names(assetData)){
  if (asset %in% c("MXEF", "G0O1", "RMZ")){
    regime.ind[[asset]] = (models[[asset]]["Fit"]["smoProb"][, 1] < 0.5)
  }else{
    regime.ind[[asset]] = (models[[asset]]["Fit"]["smoProb"][, 1] > 0.5)    
  }
}

regime1 = list()
for (asset in names(assetData)){
  regime1[[asset]] = assetData[[asset]]$retrn_dl[regime.ind[[asset]] == TRUE]
}

regime2 = list()
for (asset in names(assetData)){
  regime2[[asset]] = assetData[[asset]]$retrn_dl[regime.ind[[asset]] == FALSE]
}

###########################################
######### basic summary of ################
##### regime 1 and regimw 2 separately ####
###########################################

# calculate the Sharpe ratio, standard deviation, skewness, kurtosis

statSmmr_regime1 <- matrix(rep(0, 4*length(assetData)), nrow=length(assetData))
rownames(statSmmr_regime1) <- names(assetData)
colnames(statSmmr_regime1) <- c("SR","sd","skewness","kurtosis")

for (asset in names(assetData)){
  dt <- as.data.frame(assetData[[asset]]$retrn_dl[regime.ind[[asset]]])
  # rownames(dt) <- asset$Date
  # statSmmr_regime1[asset, 1] <- SharpeRatio(dt, Rf = 0)
  statSmmr_regime1[asset, 2] <- sd(dt[,1]) * sqrt(252)
  statSmmr_regime1[asset, 3] <- skewness(dt[,1])
  statSmmr_regime1[asset, 4] <- kurtosis(dt[,1])
  Rf = 0
  statSmmr_regime1[asset, 1] <- (mean(dt[,1]) - Rf)/ statSmmr_regime1[asset, 2] * sqrt(252)
} 

# calculate the Sharpe ratio, standard deviation, skewness, kurtosis

statSmmr_regime2 <- matrix(rep(0, 4*length(assetData)), nrow=length(assetData))
rownames(statSmmr_regime2) <- names(assetData)
colnames(statSmmr_regime2) <- c("SR","sd","skewness","kurtosis")

for (asset in names(assetData)){
  dt <- as.data.frame(assetData[[asset]]$retrn_dl[regime.ind[[asset]] == FALSE])
  # rownames(dt) <- asset$Date
  # statSmmr_regime2[asset, 1] <- SharpeRatio(dt, Rf = 0)
  statSmmr_regime2[asset, 2] <- sd(dt[,1]) * sqrt(252)
  statSmmr_regime2[asset, 3] <- skewness(dt[,1])
  statSmmr_regime2[asset, 4] <- kurtosis(dt[,1])
  Rf = 0
  statSmmr_regime2[asset, 1] <- (mean(dt[,1]) - Rf)/ statSmmr_regime2[asset, 2] * sqrt(252)
} 

#################################################
### get basic statistics of two regimes (RMZ) ###
#################################################

asset = "RMZ"

mean(regime1[[asset]])
mean(regime2[[asset]])

length(regime1[[asset]])
length(regime2[[asset]])
length(regime1[[asset]]) / (length(regime1[[asset]]) + length(regime2[[asset]]) )
length(regime2[[asset]]) / (length(regime1[[asset]]) + length(regime2[[asset]]) )

VaR(regime1[[asset]], p = 0.95)
VaR(regime2[[asset]], p = 0.95)
ES(regime1[[asset]], p = 0.95)
ES(regime2[[asset]], p = 0.95)

max(regime1[[asset]])
max(regime2[[asset]])
min(regime1[[asset]])
min(regime2[[asset]])

# time point of changing regime
which(regime.ind[[asset]][1:(length(regime.ind[[asset]])-1)] != regime.ind[[asset]][2:length(regime.ind[[asset]])])

##############################
### analysis for asset RMZ ###
##############################


plot(assetData[["RMZ"]]$retrn_dl)

# regime1: 2015-2545 regime2: 596-1126
assetData[["RMZ"]]$Date[596]
assetData[["RMZ"]]$Date[1126]
assetData[["RMZ"]]$Date[2015]
assetData[["RMZ"]]$Date[2545]

VaR(assetData[["RMZ"]]$retrn_dl[596:1126], p = 0.95)
ES(assetData[["RMZ"]]$retrn_dl[596:1126], p = 0.95)
VaR(assetData[["RMZ"]]$retrn_dl[2015:2545], p = 0.95)
ES(assetData[["RMZ"]]$retrn_dl[2015:2545], p = 0.95)

acf_1 = acf(assetData[["RMZ"]]$retrn_dl[596:1126])
acf_1$acf[2]
acf_2 = acf(assetData[["RMZ"]]$retrn_dl[2015:2545])
acf_2$acf[2]

RMZ_dd <- read.csv("../results/maxDrawdowns/9_mon3.csv")
plot(density(RMZ_dd[(2015+63):2545, 2]))
lines(density(RMZ_dd[(596+63):1126, 2]))

# calculate the monthly maximum drawdown

RMZ_dd_mon1 <- calcMaxDd(assetData[["RMZ"]], prd = 21)

# plot the maximum drawdown distribution of two regimes

png("../figures/regime_switching/RMZ_mon1_mdd.png", width = 500, height = 500)
plot(density(RMZ_dd_mon1[(2015+21):2545]), 
     xlim = c(0, 0.5), 
     xlab = "Maximum Drawdown",
     col = "red",
     main = "Density of maximum drawdown distribution of two regimes")
lines(density(RMZ_dd_mon1[(596+21):1126]), col = "blue")
legend('topright',
       c("Regime 1", "Regime 2"),
       bty='n', cex=1,
       lwd=c(2.5,2.5),col=c("red", "blue"))
dev.off()

# calculate the CED for two regimes

CED1 = calcCED(RMZ_dd_mon1[(596+20):1126], prd = 511)
CED2 = calcCED(RMZ_dd_mon1[(2015+20):2545], prd = 511)

##############################################
### rolling risk diagnostics for asset RMZ ###
##############################################

regime1_RMZ_ES1mon = calcRolling(assetData[["RMZ"]][596:1126, ], 
                                 prd = 21, FUN = "ES", p = 0.95)
regime2_RMZ_ES1mon = calcRolling(assetData[["RMZ"]][2015:2545, ], 
                                 prd = 21, FUN = "ES", p = 0.95)

regime1_RMZ_VaR1mon = calcRolling(assetData[["RMZ"]][596:1126, ], 
                                 prd = 21, FUN = "VaR", p = 0.95)
regime2_RMZ_VaR1mon = calcRolling(assetData[["RMZ"]][2015:2545, ], 
                                 prd = 21, FUN = "VaR", p = 0.95)

regime1_RMZ_volatility1mon = calcRolling(assetData[["RMZ"]][596:1126, ], 
                                  prd = 21, FUN = "volatility")
regime2_RMZ_volatility1mon = calcRolling(assetData[["RMZ"]][2015:2545, ], 
                                  prd = 21, FUN = "volatility")

regime1_RMZ_MDD1mon = RMZ_dd_mon1[(596+20):1126]
regime2_RMZ_MDD1mon = RMZ_dd_mon1[(2015+20):2545]

get_acf = function(R){
  acf(R)$acf[2]
}

regime1_RMZ_rho1mon = calcRolling(assetData[["RMZ"]][596:1126, ], 
                                         prd = 21, FUN = "serialCorrelation", order = 1)
mean(regime2_RMZ_rho1mon)
regime2_RMZ_rho1mon = calcRolling(assetData[["RMZ"]][2015:2545, ], 
                                         prd = 21, FUN = "serialCorrelation", order = 1)


cor(regime1_RMZ_MDD1mon/regime1_RMZ_volatility1mon, regime1_RMZ_rho1mon)
cor(regime2_RMZ_MDD1mon/regime2_RMZ_volatility1mon, regime2_RMZ_rho1mon)


cor(regime1_RMZ_rho1mon, regime1_RMZ_MDD1mon)
cor(regime1_RMZ_rho1mon, regime1_RMZ_ES1mon)
cor(regime1_RMZ_rho1mon, regime1_RMZ_VaR1mon)
cor(regime1_RMZ_rho1mon, regime1_RMZ_volatility1mon)

cor(regime2_RMZ_rho1mon, regime2_RMZ_MDD1mon)
cor(regime2_RMZ_rho1mon, regime2_RMZ_ES1mon)
cor(regime2_RMZ_rho1mon, regime2_RMZ_VaR1mon)
cor(regime2_RMZ_rho1mon, regime2_RMZ_volatility1mon)

