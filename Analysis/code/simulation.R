
source("risk_diagostics_function.R")

ts.param = seq(from = -0.9, to = 0.9, by = 0.05)
ts.param = ts.param[ts.param != 0]

# # AR(1)
# 
# ar.sim = lapply(ts.param, function(x){
#   arima.sim(model=list(ar=x), n=1000, sd = 0.01)
# })
# 
# names(ar.sim) = ts.param
# 
# for (asset in names(ar.sim)){
#   ar.sim[[asset]] = as.xts(ar.sim[[asset]])
# }
# 
# # Risk diagnostic
# 
# risk_df <- function(Data, FUN, ...){
#   res = rep(0, length(Data))
#   names(res) = names(Data)
#   for (i in names(Data)){
#     res[i] <- do.call(FUN, list(Data[[i]], ...))
#   }
#   return(res)
# }
# 
# ES.ar.sim.0.95 = risk_df(ar.sim, FUN = 'ES', p = 0.95)
# VaR.ar.sim.0.95 =  risk_df(ar.sim, FUN = 'VaR', p = 0.95)
# volatility.ar.sim = risk_df(ar.sim, FUN = 'volatility')
# maxDrawdown.ar.sim = risk_df(ar.sim, FUN = 'maxDrawdown')




sim_ts <- function(x, rep = 1000, p = 0.95, sd = 0.0001){
  ar.sim = arima.sim(model=list(ar=x), n=rep)
  ar.sim = as.xts(ar.sim)
  res = c(ES(ar.sim, p = p),
          VaR(ar.sim, p = p),
          sd(ar.sim),
          maxDrawdown(ar.sim))
  names(res) = c("ES", "VaR", "volatility", "maxDrawdown")
  return(res)
}

rep_sim_ts <- function(x, rep = 1000, p = 0.95){
  df = replicate(rep, sim_ts(x))
  sim.maxDrawdown = df['maxDrawdown', ] 
  res = c(mean( df['ES', ] ),
          mean( df['VaR', ] ),
          mean( df['volatility', ] ),
          mean(sim.maxDrawdown[sim.maxDrawdown > quantile(sim.maxDrawdown, probs = p)]))
  names(res) = c("ES", "VaR", "sd", "CED")
  return(res)
}

### plot(density(sim.maxDrawdown))

sim.risk.ar.1 = sim.risk
sim.risk.ar.1 = sapply(ts.param, function(x){rep_sim_ts(x)})
sim.risk.ar.1 = data.frame(cbind(t(sim.risk.ar.1), ts.param))
rownames(sim.risk.ar.1) = ts.param

plot(ts.param,sim.risk.ar.1['CED',])

library(ggplot2)
library(cowplot)
plot_grid(ggplot(sim.risk.ar.1, aes(x = ts.param, y = ES)) + geom_point() + theme_bw() + 
            xlab("AR(1) Coefficient"),
          ggplot(sim.risk.ar.1, aes(x = ts.param, y = VaR)) + geom_point() + theme_bw() + 
            xlab("AR(1) Coefficient"),
          ggplot(sim.risk.ar.1, aes(x = ts.param, y = sd)) + geom_point() + theme_bw() + 
            xlab("AR(1) Coefficient"),
          ggplot(sim.risk.ar.1, aes(x = ts.param, y = CED)) + geom_point() + theme_bw() + 
            xlab("AR(1) Coefficient"),
          ncol = 2, align = 'v')










# for (asset in names(ar.sim)){
#   ar.sim[[asset]] = data.frame(Date = rownnames(as.xts(ar.sim[[asset]])),
#                                retrn_dl = as.vector(ar.sim[[asset]]))
# }


# normality test

library(Matching)

ks.test_res = rep(0, length(ar.sim))
names(ks.test_res) = ts.param
for (asset in names(ar.sim)){
  x = ar.sim[[asset]]
  ks.test_res[asset] = ks.boot(x, rnorm(10000, mean(x), sd(x)))$ks.boot.pvalue
}



















ar.sim<-arima.sim(model=list(ar=c(.9,-.2)),n=100)
ts.plot(ar.sim) 
ar.acf<-acf(ar.sim,type="correlation",plot=T) 
ar.acf
ar.pacf<-acf(ar.sim,type="partial",plot=T)
ar.pacf


ma.sim<-arima.sim(model=list(ma=c(-.7,.1)),n=100) 
ma.sim 
ts.plot(ma.sim) 
ma.acf<-acf(ma.sim,type="correlation",plot=T) 
ma.acf 
ma.pacf<-acf(ma.sim,type="partial",plot=T)
ma.pacf 

arma.sim<-arima.sim(model=list(ar=c(.9,-.2),ma=c(-.7,.1)),n=100)
arma.sim 
ts.plot(arma.sim) 
arma.acf<-acf(arma.sim,type="correlation",plot=T) 
arma.acf 
arma.pacf<-acf(arma.sim,type="partial",plot=T)
arma.pacf

# mildly long-tailed
arima.sim(n = 63, list(ar = c(0.8897, -0.4858), ma = c(-0.2279, 0.2488)),
          rand.gen = function(n, ...) sqrt(0.1796) * rt(n, df = 5))








