library(dynlm)
library(MSwM)

# g.lm <- dynlm(retrn_dl ~ 1 + L(retrn_dl, 1), data = zooreg(data.frame(retrn_dl = AGG$retrn_dl)))
# g.lm
# class(g.lm)
# testmodel = lm(retrn_dl ~ 1, data = AGG)
# summary(testmodel)

########################################
######### using lag term in lm #########
########################################

series  = ts(RMZ$retrn_dl)
series_b1 = lag(series, -1)
y = cbind(series, series_b1)
testmodel <- lm(y[,1]~y[,2])
summary(testmodel)
class(testmodel)

mod.mswm=msmFit(testmodel,k=2,sw=rep(T, 3),control=list(parallel=F))
summary(mod.mswm)
class(mod.mswm)

plotProb(mod.mswm,which=1)
plotProb(mod.mswm,which=2)
intervals(mod.mswm)
AIC(mod.mswm)
plotDiag(mod.mswm)

mod.mswm@Fit@smoProb
mod.mswm["Fit"]["smoProb"][1000:1500, 1] > 0.5

########################################
####### using lag term in msmFit #######
########################################

val = "SPX"
lm.val = lm(retrn_dl ~ 1, get(val))
mod.mswm=msmFit(lm.val, k=2, p=1, sw=rep(T, 3), control=list(parallel=F))
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

assetsList <- c("AGG", "HYG", "TIP",
                "BCOM", "BUHY", "G0O1", "LTP5TRUU", "MXEA", "MXEF", "RAY", "RMZ", "SPX", "USGG10YR")

indexSub <- c(1:4, 6, 8:13)
assetsList <- assetsList[indexSub]

models = list()
for (val in assetsList){
  lm.val = lm(retrn_dl ~ 1, get(val))
  models[[val]] = msmFit(lm.val, k=2, p=1, sw=rep(T, 3), control=list(parallel=F))
}

# print the summary of every asset

for (val in assetsList){
  summary(models[[val]])
}

# print the coefficient of the model

for (val in assetsList){
  cat(paste(val, format(round(models[[val]]@Coef$retrn_dl_1, 3), nsmall = 3),
            sep = " & ", collapse = " "))
  cat(" \\\\ \n")
#   print(val)
#   print(models[[val]]@Coef$retrn_dl_1)
}

###########################################
#### plot the regimes for every asset #####
###########################################

for (val in assetsList){
  png(paste("../results/regime/", val, ".png", sep = ''), width = 600, height = 600)
  plotProb(models[[val]],which=2)
  dev.off()
}

###########################################
#### create two list for returns of  ######
##### regime 1 and regimw 2 separately ####
###########################################

# boolean vector of regime
# AGG GO01 RAY USGG10YR

regime.ind = list()
for (val in assetsList){
  if (val %in% c("AGG", "G0O1", "RAY", 'USGG10YR')){
    regime.ind[[val]] = (models[[val]]["Fit"]["smoProb"][, 1] < 0.5)
  }else{
    regime.ind[[val]] = (models[[val]]["Fit"]["smoProb"][, 1] > 0.5)    
  }
}s

regime1 = list()
for (val in assetsList){
  regime1[[val]] = get(val)$retrn_dl[regime.ind[[val]] == TRUE]
}

regime2 = list()
for (val in assetsList){
  regime2[[val]] = get(val)$retrn_dl[regime.ind[[val]] == FALSE]
}

###########################################
### get basic statistics of two regimes ###
###########################################

val = "RMZ"

mean(regime1[[val]])
mean(regime2[[val]])

length(regime1[[val]])
length(regime2[[val]])
length(regime1[[val]]) / (length(regime1[[val]]) + length(regime2[[val]]) )
length(regime2[[val]]) / (length(regime1[[val]]) + length(regime2[[val]]) )

VaR(regime1[[val]], p = 0.95)
VaR(regime2[[val]], p = 0.95)
ES(regime1[[val]], p = 0.95)
ES(regime2[[val]], p = 0.95)

max(regime1[[val]])
max(regime2[[val]])
min(regime1[[val]])
min(regime2[[val]])

which(regime.ind[[val]][1:(length(regime.ind[[val]])-1)] != regime.ind[[val]][2:length(regime.ind[[val]])])

# regime1: 2015-2545 regime2: 596-1126
RMZ$Date[596]
RMZ$Date[1126]
RMZ$Date[2015]
RMZ$Date[2545]

VaR(get(val)$retrn_dl[596:1126], p = 0.95)
ES(get(val)$retrn_dl[596:1126], p = 0.95)
VaR(get(val)$retrn_dl[2015:2545], p = 0.95)
ES(get(val)$retrn_dl[2015:2545], p = 0.95)

acf_1 = acf(get(val)$retrn_dl[596:1126])
acf_1
acf_2 = acf(get(val)$retrn_dl[2015:2545])
acf_2

RMZ_dd <- read.csv("../results/maxDrawdowns/9_mon3.csv")
plot(density(RMZ_dd[(2015+63):2545, 2]))
lines(density(RMZ_dd[(596+63):1126, 2]))

RMZ_dd_mon1 <- calcMaxDd(get(val), prd = 21)

png("../results/regime/RMZ_mon1_mdd.png", width = 400, height = 400)
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

ES(RMZ_dd_mon1[(596+21):1126], p = 0.1)
ES(RMZ_dd_mon1[(2015+21):2545], p = 0.1)



# calculate the Sharpe ratio, standard deviation, skewness, kurtosis

statSmmr_regime1 <- matrix(rep(0, 4*length(assetsList)), nrow=length(assetsList))
rownames(statSmmr_regime1) <- assetsList
colnames(statSmmr_regime1) <- c("SR","sd","skewness","kurtosis")

for (i in assetsList){
  val <- get(i)
  dt <- as.data.frame(val$retrn_dl[regime.ind[[i]]])
  # rownames(dt) <- val$Date
  # statSmmr_regime1[i, 1] <- SharpeRatio(dt, Rf = 0)
  statSmmr_regime1[i, 2] <- sd(dt[,1]) * sqrt(252)
  statSmmr_regime1[i, 3] <- skewness(dt[,1])
  statSmmr_regime1[i, 4] <- kurtosis(dt[,1])
  Rf = 0
  statSmmr_regime1[i, 1] <- (mean(dt[,1]) - Rf)/ statSmmr_regime1[i, 2] * sqrt(252)
} 

# calculate the Sharpe ratio, standard deviation, skewness, kurtosis

statSmmr_regime2 <- matrix(rep(0, 4*length(assetsList)), nrow=length(assetsList))
rownames(statSmmr_regime2) <- assetsList
colnames(statSmmr_regime2) <- c("SR","sd","skewness","kurtosis")

for (i in assetsList){
  val <- get(i)
  dt <- as.data.frame(val$retrn_dl[regime.ind[[i]] == FALSE])
  # rownames(dt) <- val$Date
  # statSmmr_regime2[i, 1] <- SharpeRatio(dt, Rf = 0)
  statSmmr_regime2[i, 2] <- sd(dt[,1]) * sqrt(252)
  statSmmr_regime2[i, 3] <- skewness(dt[,1])
  statSmmr_regime2[i, 4] <- kurtosis(dt[,1])
  Rf = 0
  statSmmr_regime2[i, 1] <- (mean(dt[,1]) - Rf)/ statSmmr_regime2[i, 2] * sqrt(252)
} 

sum(regime.ind[[i]])


i = "AGG"

val = "AGG"
regime.ind[[i]]




