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


##################### For RMZ ###########################
bestorder = auto.arima(RMZ$retrn_dl)
sarima(RMZ$retrn_dl, p=0,d=0,q=1)
FIT_RMZ_ARMA = arima(RMZ$retrn_dl, order=c(0,0,1), include.mean = FALSE)
summary(FIT_RMZ_ARMA)
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


FIT_AGG_ARMA = arima(SPX$retrn_dl, order = c(5,0,5))
AGG_resid = FIT_AGG_ARMA$residuals

FIT_AGG_Garch = garchFit(formula=~garch(1,1),data=AGG_resid,trace=FALSE,include.mean=FALSE,cond.dist="std")
summary(FIT_AGG_Garch)
par(mfrow=c(2,2))
plot(FIT_AGG_Garch,which = 9)
plot(FIT_AGG_Garch,which = 10)
plot(FIT_AGG_Garch,which = 11)
plot(FIT_AGG_Garch,which = 13)
