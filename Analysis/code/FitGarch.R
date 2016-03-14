############# Find the best Garch Model ####################
# From the previous section, we have already found that MA(1) is probably the best model for 
# RMZ

library(forecast)
require(Rsafd)
library(tseries)
library(timeSeries)
library("quantmod")
require(astsa)
library(fGarch)

png("Analysis/results/clusterVariance", width = 1600, height = 1600)
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



##################### For RMZ ###########################
bestorder = auto.arima(RMZ$retrn_dl)
sarima(RMZ$retrn_dl, p=0,d=0,q=1)
FIT_RMZ_ARMA = arima(RMZ$retrn_dl, order=c(0,0,1), include.mean = FALSE)
summary(FIT_RMZ_ARMA)
plot(FIT_RMZ_ARMA$residuals)
#### fit Garch ###
FIT_RMZ_Garch = garchFit(formula=~arma(0,1)+garch(1,1),data=RMZ$retrn_dl,trace=FALSE,include.mean=FALSE)
summary(FIT_RMZ_Garch)
plot(FIT_RMZ_Garch)
par(mfrow=c(2,2))
plot(FIT_RMZ_Garch,which = 9)
plot(FIT_RMZ_Garch,which = 10)
plot(FIT_RMZ_Garch,which = 11)
plot(FIT_RMZ_Garch,which = 13)

#Fix the heavy tail using eneralized normal distribution
FIT_RMZ_Garch_ged = garchFit(formula=~arma(0,1)+garch(1,1),data=RMZ$retrn_dl,trace=FALSE,include.mean=FALSE,cond.dist="ged")
plot(FIT_RMZ_Garch_ged,which = 9)
plot(FIT_RMZ_Garch_ged,which = 10)
plot(FIT_RMZ_Garch_ged,which = 11)
plot(FIT_RMZ_Garch_ged,which = 13)

# #robust estimate (QML) ## the results are not that good 
# summary(FIT_RMZ_Garch_qmle <- garchFit(formula=~arma(0,1)+garch(1,1),data=RMZ$retrn_dl,trace=FALSE,include.mean=FALSE,cond.dist="ged"))
# ##"QMLE" stands for Quasi-Maximum Likelihood Estimation, which assumes normal distribution and uses robust standard errors for inference. Bollerslev and Wooldridge (1992)
# se_qmle=scale(FIT_RMZ_Garch_qmle@residuals)
# qqnorm(se_qmle)
# qqline(se_qmle,col="red")

## some prediction, using the data 60 days head
par(mfrow=c(1,1))
plot(RMZ[,1],FIT_RMZ_Garch_ged@sigma.t,type="l", xlab = "time", ylab = "standard deviation")
n_RMZ = length(FIT_RMZ_Garch_ged@sigma.t)
emfit=rep(0,n_RMZ)
for(i in 31:n_RMZ){
  emfit[i]=sd(FIT_RMZ_ARMA$residuals[(i-30):i])
}
lines(RMZ[,1],emfit,type="l",col="red")
legend(2012,0.10, c("True", "Estimate"), lty=c(1,1),lwd=c(2.5,2.5),col=c("black","red"))

u = FIT_RMZ_Garch_ged@sigma.t
plot(window(FIT_RMZ_ARMA$residuals, start=1500, end=2000),type="l" ,ylim=c(-.22,.2), ylab="Residuals of AR(32)")
lines(window(FIT_RMZ_ARMA$residuals-2*u, start=1500, end=2000), lty=2, col=4)
lines(window(FIT_RMZ_ARMA$residuals+2*u, start=1500, end=2000), lty=2, col=4)


#prediction confidence
par(mfrow=c(1,1))
testdata=FIT_RMZ_ARMA$residuals[1:(n_RMZ-20)]
summary(see_pred <- garchFit(~garch(1,1), data=testdata,cond.dist="ged"))
predict(see_pred,n.ahead=20,plot=TRUE,conf=.9,nx=100)
lines(x = c(100:120),FIT_RMZ_ARMA$residuals[(n_RMZ-20):n_RMZ],col="black")

################## AGG #########################
AGG_find_arima = auto.arima(AGG$retrn_dl, max.p=8, max.q=8)
#it just non stationary no matter how large it is
plot(ts(AGG$retrn_dl))

##############
#Transform the data into a stationary time series
##############
#1.Remove trends
#2.Remove seasonal components
#3.Differentiate successively if needed
Xt1 <- stl(AGG$retrn_dl,s.window="period")
monthplot(AGG$retrn_dl)
#NO trend and seasonal components
pacf(AGG$retrn_dl)
#### fit Garch ### EVEN JUST GARCH IS GOOD ENOUGH TO CATCH ALL THE 
# again, I used "ged" for cond.dist
FIT_AGG_ARMA = arima(AGG$retrn_dl, order = c(1,0,3))
AGG_resid = FIT_AGG_ARMA$residuals
FIT_AGG_Garch = garchFit(formula=~garch(1,1),data=AGG_resid,trace=FALSE,include.mean=FALSE,cond.dist="ged")
summary(FIT_AGG_Garch)
par(mfrow=c(2,2))
plot(FIT_AGG_Garch,which = 9)
plot(FIT_AGG_Garch,which = 10)
plot(FIT_AGG_Garch,which = 11)
plot(FIT_AGG_Garch,which = 13)

## some prediction, using the data 60 days head
par(mfrow=c(1,1))
plot(FIT_AGG_Garch@sigma.t,type="l")
n_AGG = length(FIT_AGG_Garch@sigma.t)
emfit=rep(0,n_AGG)
for(i in 61:n_AGG){
  emfit[i]=sd(FIT_AGG_ARMA$residuals[(i-60):i])
}
lines(emfit,type="l",col="red")


u = FIT_AGG_Garch@sigma.t
plot(window(FIT_AGG_ARMA$residuals, start=1500, end=2000),type="l" ,ylim=c(-.02,.02), ylab="Residuals of AR(32)")
lines(window(FIT_AGG_ARMA$residuals-2*u, start=1500, end=2000), lty=2, col=4)
lines(window(FIT_AGG_ARMA$residuals+2*u, start=1500, end=2000), lty=2, col=4)


#prediction confidence
par(mfrow=c(1,1))
testdata=FIT_AGG_ARMA$residuals[1:(n_AGG-20)]
summary(see_pred <- garchFit(~garch(1,1), data=AGG_resid,cond.dist="ged"))
predict(see_pred,n.ahead=20,plot=TRUE,conf=.95,nx=100)
lines(x = c(100:120),FIT_AGG_ARMA$residuals[(n_AGG-20):n_AGG],col="black")





#################### SPX ######################
SPX_find_arima = auto.arima(SPX$retrn_dl, max.p=8, max.q=8)
#it just non stationary no matter how large it is
par(mfrow = c(1,1))
plot(ts(SPX$retrn_dl))

##############
#Transform the data into a stationary time series
##############
#1.Remove trends
#2.Remove seasonal components
#3.Differentiate successively if needed
Xt1 <- stl(SPX$retrn_dl,s.window="period")
monthplot(SPX$retrn_dl)
#NO trend and seasonal components
par(mfrow = c(1,2))
acf(SPX$retrn_dl)
pacf(SPX$retrn_dl)
par(mfrow = c(1,1))
#### fit Garch ### EVEN JUST GARCH IS GOOD ENOUGH TO CATCH ALL THE 
# again, I used "ged" for cond.dist
FIT_SPX_ARMA = arima(SPX$retrn_dl, order = c(1,0,1))
SPX_resid = FIT_SPX_ARMA$residuals
plot(SPX[,1], SPX_resid, xlab = "Time", ylab = "Residual",type = "l")

FIT_SPX_Garch = garchFit(formula=~garch(1,1),data=SPX_resid,trace=FALSE,include.mean=FALSE,cond.dist="std")
summary(FIT_SPX_Garch)
par(mfrow=c(2,2))
plot(FIT_SPX_Garch,which = 9)
plot(FIT_SPX_Garch,which = 10)
plot(FIT_SPX_Garch,which = 11)
plot(FIT_SPX_Garch,which = 13)

## some prediction, using the data 60 days head
par(mfrow=c(1,1))
plot(FIT_SPX_Garch@sigma.t,type="l",xlab = "time", ylab = "standard deviation")
n_SPX = length(FIT_SPX_Garch@sigma.t)
emfit=rep(0,n_SPX)
for(i in 61:n_SPX){
  emfit[i]=sd(FIT_SPX_ARMA$residuals[(i-60):i])
}
lines(emfit,type="l",col="red")


u = FIT_SPX_Garch@sigma.t
plot(window(FIT_SPX_ARMA$residuals, start=1500, end=2000),type="l" ,ylim=c(-.06,.06), ylab="Residuals of ARMA(2,2)")
lines(window(FIT_SPX_ARMA$residuals-2*u, start=1500, end=2000), lty=2, col=4)
lines(window(FIT_SPX_ARMA$residuals+2*u, start=1500, end=2000), lty=2, col=4)


#prediction confidence
par(mfrow=c(1,1))
testdata=FIT_AGG_ARMA$residuals[1:(n_AGG-20)]
summary(see_pred <- garchFit(~garch(1,1), data=AGG_resid,cond.dist="ged"))
predict(see_pred,n.ahead=20,plot=TRUE,conf=.95,nx=100)
lines(x = c(100:120),FIT_SPX_ARMA$residuals[(n_SPX-20):n_SPX],col="black")


# automatically find the best ARIMA models
for (name in assetsList){
  assign(paste(name,"_find", "_arima", sep=""),
         auto.arima(get(name)$retrn_dl, max.p=5, max.q=5))
}

for (name in assetsList){
  print(get(paste(name,"_find", "_arima", sep=""))$arma)
}


FIT_AGG_ARMA = arima(AGG$retrn_dl, order = c(5,0,5))
AGG_resid = FIT_AGG_ARMA$residuals

FIT_AGG_Garch = garchFit(formula=~garch(1,1),data=AGG_resid,trace=FALSE,include.mean=FALSE,cond.dist="ged")
FIT_AGG_ARMA$coef
summary(FIT_AGG_ARMA)
summary(FIT_AGG_Garch)
par(mfrow=c(2,2))
plot(FIT_AGG_Garch,which = 9)
plot(FIT_AGG_Garch,which = 10)
plot(FIT_AGG_Garch,which = 11)
plot(FIT_AGG_Garch,which = 13)

# HYG
FIT_HYG_ARMA = arima(HYG$retrn_dl, order = c(3,0,1))
HYG_resid = FIT_HYG_ARMA$residuals

FIT_HYG_Garch = garchFit(formula=~garch(1,1),data=HYG_resid,trace=FALSE,include.mean=FALSE,cond.dist="std")
summary(FIT_HYG_ARMA)
summary(FIT_HYG_Garch)

#TIP
FIT_TIP_ARMA = arima(TIP$retrn_dl, order = c(0,0,0))
TIP_resid = FIT_TIP_ARMA$residuals

FIT_TIP_Garch = garchFit(formula=~garch(1,1),data=TIP_resid,trace=FALSE,include.mean=FALSE,cond.dist="ged")
summary(FIT_TIP_ARMA)
summary(FIT_TIP_Garch)

#BCOM
FIT_BCOM_ARMA = arima(BCOM$retrn_dl, order = c(0,0,0))
BCOM_resid = FIT_BCOM_ARMA$residuals

FIT_BCOM_Garch = garchFit(formula=~garch(1,1),data=BCOM_resid,trace=FALSE,include.mean=FALSE,cond.dist="ged")
summary(FIT_BCOM_ARMA)
summary(FIT_BCOM_Garch)

#MXEA
FIT_MXEA_ARMA = arima(MXEA$retrn_dl, order = c(2,0,4))
MXEA_resid = FIT_MXEA_ARMA$residuals

FIT_MXEA_Garch = garchFit(formula=~garch(1,2),data=MXEA_resid,trace=FALSE,include.mean=FALSE,cond.dist="ged")
summary(FIT_MXEA_ARMA)
summary(FIT_MXEA_Garch)

#MXEF
FIT_MXEF_ARMA = arima(MXEF$retrn_dl, order = c(4,0,2))
MXEF_resid = FIT_MXEF_ARMA$residuals

FIT_MXEF_Garch = garchFit(formula=~garch(1,1),data=MXEF_resid,trace=FALSE,include.mean=FALSE,cond.dist="ged")
summary(FIT_MXEF_ARMA)
summary(FIT_MXEF_Garch)

#RAY
FIT_RAY_ARMA = arima(RAY$retrn_dl, order = c(2,0,2))
RAY_resid = FIT_RAY_ARMA$residuals

FIT_RAY_Garch = garchFit(formula=~garch(1,1),data=RAY_resid,trace=FALSE,include.mean=FALSE,cond.dist="ged")
summary(FIT_RAY_ARMA)
summary(FIT_RAY_Garch)

#SPX
FIT_SPX_ARMA = arima(SPX$retrn_dl, order = c(2,0,2))
SPX_resid = FIT_SPX_ARMA$residuals

FIT_SPX_Garch = garchFit(formula=~garch(1,1),data=SPX_resid,trace=FALSE,include.mean=FALSE,cond.dist="std")
summary(FIT_SPX_ARMA)
summary(FIT_SPX_Garch)

#USGG10YR
FIT_USGG10YR_ARMA = arima(USGG10YR$retrn_dl, order = c(0,0,0))
USGG10YR_resid = FIT_USGG10YR_ARMA$residuals

FIT_USGG10YR_Garch = garchFit(formula=~garch(1,3),data=USGG10YR_resid,trace=FALSE,include.mean=FALSE,cond.dist="std")
summary(FIT_USGG10YR_ARMA)
summary(FIT_USGG10YR_Garch)

######################## The best order of ARMA ############################
#bestorders = lapply(assetsList,function(i) auto.arima(get(i)$retrn_dl)$arma)

bestorders = lapply(assetsList,function(i) {
  para = auto.arima(get(i)$retrn_dl)$arma
  return (c(para[1],para[6], para[2]))}
)

# residual left after arima
resids = lapply(1:11,
                function(i) arima(get(assetsList[i])$retrn_dl, order=unlist(bestorders[i]), include.mean = FALSE)$residuals)


######################## Clustered Variance ##########################
png("Analysis/results/resids.png", width = 1600, height = 1600)
plots = list()
for (i in 1:11){
  plt <- data.frame(y_axis = unlist(resids[i]),
                    x_axis = c(1:length(unlist(resids[i]))))
  plots[[i]] <- ggplot(plt, aes(x=x_axis, y=y_axis)) +
    geom_line() +
    ggtitle(paste(assetsList[i], ": Residuals after Fitting ARMA", sep = "")) + 
    ylab("Residual") + 
    #ylim(0, 1) +
    xlab("Period")
}
multiplot(plotlist = plots, cols = 3)
dev.off()




