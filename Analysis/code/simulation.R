
library(ggplot2)
library(cowplot)

source("risk_diagostics_function.R")

ts.param = seq(from = -0.9, to = 0.9, by = 0.05)
ts.param = ts.param[ts.param != 0]

#############
## example ##
#############

sim_ts <- function(p = 0.95, model, n, rand.gen){
  ar.sim = arima.sim(model = model, n = n, rand.gen = rand.gen)
  ar.sim = as.xts(ar.sim)
  res = c(ES(ar.sim, p = p),
          VaR(ar.sim, p = p),
          sd(ar.sim),
          maxDrawdown(ar.sim))
  names(res) = c("ES", "VaR", "volatility", "maxDrawdown")
  return(res)
}

rep_sim_ts <- function(rep = 1000, p = 0.95, model, n, rand.gen){
  df = replicate(rep, sim_ts(p = 0.95, model = model, n = n, rand.gen = rand.gen))
  sim.maxDrawdown = df['maxDrawdown', ] 
  res = c(mean( df['ES', ] ),
          mean( df['VaR', ] ),
          mean( df['volatility', ] ),
          mean(sim.maxDrawdown[sim.maxDrawdown > quantile(sim.maxDrawdown, probs = p)]))
  names(res) = c("ES", "VaR", "sd", "CED")
  return(res)
}

######################
## function example ##
######################

# x=0.2
# test_ts = sim_ts(p = 0.95, model=list(ar=x), n=1000, 
#                  rand.gen = function(n) rnorm(n, sd = 0.01))
# rep_sim_ts(model=list(ar=x), n=1000, 
#            rand.gen = function(n) rnorm(n, sd = 0.01))

### plot(density(sim.maxDrawdown))

## AR(1) ##
###########

sim.risk.ar.1 = sapply(ts.param, 
                       function(x){rep_sim_ts(model=list(ar=x), n=1000, 
                                              rand.gen = function(n) rnorm(n, sd = 0.01))})
sim.risk.ar.1 = data.frame(cbind(t(sim.risk.ar.1), ts.param))

plot_grid(ggplot(sim.risk.ar.1, aes(x = ts.param, y = ES)) + geom_point() + theme_bw() + 
            xlab("AR(1) Coefficient"),
          ggplot(sim.risk.ar.1, aes(x = ts.param, y = VaR)) + geom_point() + theme_bw() + 
            xlab("AR(1) Coefficient"),
          ggplot(sim.risk.ar.1, aes(x = ts.param, y = sd)) + geom_point() + theme_bw() + 
            xlab("AR(1) Coefficient"),
          ggplot(sim.risk.ar.1, aes(x = ts.param, y = CED)) + geom_point() + theme_bw() + 
            xlab("AR(1) Coefficient"),
          ncol = 2, align = 'v')

## AR(2) ##
###########

ts.param.2 = expand.grid(seq(from = -0.9, to = 0.9, by = 0.1), 
                         seq(from = -0.9, to = 0.9, by = 0.1))
ts.param.2 = ts.param.2[ts.param.2[, 1] != 0, ]
ts.param.2 = ts.param.2[ts.param.2[, 2] != 0, ]
ts.param.2 = ts.param.2[-which(abs(abs(ts.param.2[, 1]) + abs(ts.param.2[, 2]) - 1)<=1e-4), ]
rownames(ts.param.2) = NULL

# check stationary
roots.ts.param.2 = t(apply(ts.param.2, 1,
                           function(x) polyroot(c(-rev(x), 1))))
ts.param.2 = ts.param.2[-which(rowSums(abs(roots.ts.param.2) > 1) != 0), ]
rownames(ts.param.2) = NULL
# calculate the autocorrelation
sim.ar.2.acf = t(apply(ts.param.2, 1,
                 function(x) ARMAacf(ar = x, lag.max = 3)[2:4]))
colnames(sim.ar.2.acf) = c("ac1", "ac2", "ac3")
# simulation
sim.risk.ar.2 = apply(ts.param.2, 1, 
                       function(x){rep_sim_ts(model=list(ar=x), n=1000, 
                                              rand.gen = function(n) rnorm(n, sd = 0.01))})
sim.risk.ar.2 = data.frame(cbind(t(sim.risk.ar.2), sim.ar.2.acf))

## MA(1) ##
###########

sim.risk.ma.1 = sapply(ts.param, 
                       function(x){rep_sim_ts(model=list(ma=x), n=1000, 
                                              rand.gen = function(n) rnorm(n, sd = 0.01))})
sim.risk.ma.1 = data.frame(cbind(t(sim.risk.ma.1), ts.param))
rownames(sim.risk.ma.1) = ts.param

plot_grid(ggplot(sim.risk.ma.1, aes(x = ts.param, y = ES)) + geom_point() + theme_bw() + 
            xlab("MA(1) Coefficient"),
          ggplot(sim.risk.ma.1, aes(x = ts.param, y = VaR)) + geom_point() + theme_bw() + 
            xlab("MA(1) Coefficient"),
          ggplot(sim.risk.ma.1, aes(x = ts.param, y = sd)) + geom_point() + theme_bw() + 
            xlab("MA(1) Coefficient"),
          ggplot(sim.risk.ma.1, aes(x = ts.param, y = CED)) + geom_point() + theme_bw() + 
            xlab("MA(1) Coefficient"),
          ncol = 2, align = 'v')

## MA(2) ##
###########

ts.param.2 = expand.grid(seq(from = -0.9, to = 0.9, by = 0.1), 
                         seq(from = -0.9, to = 0.9, by = 0.1))
# calculate the autocorrelation
sim.ma.2.acf = t(apply(ts.param.2, 1,
                       function(x) ARMAacf(ma = x, lag.max = 2)[2:3]))
colnames(sim.ma.2.acf) = c("ac1", "ac2")
# simulation
sim.risk.ma.2 = apply(ts.param.2, 1, 
                      function(x){rep_sim_ts(model=list(ma=x), n=1000, 
                                             rand.gen = function(n) rnorm(n, sd = 0.01))})
sim.risk.ma.2 = data.frame(cbind(t(sim.risk.ma.2), sim.ma.2.acf))

##################
## rolling stat ##
##################

# for (asset in names(ar.sim)){
#   ar.sim[[asset]] = data.frame(Date = rownnames(as.xts(ar.sim[[asset]])),
#                                retrn_dl = as.vector(ar.sim[[asset]]))
# }

##################
# normality test #
##################

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








