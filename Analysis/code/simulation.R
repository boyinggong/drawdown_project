
library(ggplot2)
library(cowplot)
library(xts)

source("risk_diagostics_function.R")

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

rep_sim_ts <- function(rep = 1000, p = 0.95, model, n, rand.gen, return_df = FALSE){
  df = replicate(rep, sim_ts(p = 0.95, model = model, n = n, rand.gen = rand.gen))
  sim.maxDrawdown = df['maxDrawdown', ] 
  res = c(mean( df['ES', ] ),
          mean( df['VaR', ] ),
          mean( df['volatility', ] ),
          mean(sim.maxDrawdown[sim.maxDrawdown > quantile(sim.maxDrawdown, probs = p)]))
  names(res) = c("ES", "VaR", "sd", "CED")
  if (return_df == TRUE){
    return(df)
  }else return(res)
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

###########
###########
## AR(1) ##
###########
###########

ts.param.ar.1 = seq(from = -0.9, to = 0.9, by = 0.05)
ts.param.ar.1 = ts.param.ar.1[ts.param.ar.1 != 0]
sim.risk.ar.1 = sapply(ts.param.ar.1, 
                       function(x){rep_sim_ts(model=list(ar=x), n=1000, 
                                              rand.gen = function(n) rnorm(n, sd = 0.01))})
sim.risk.ar.1 = data.frame(cbind(t(sim.risk.ar.1), ts.param.ar.1))
sim.risk.ar.1.df = lapply(ts.param.ar.1, 
                          function(x){rep_sim_ts(model=list(ar=x), n=1000, 
                                                 rand.gen = function(n) rnorm(n, sd = 0.01), return_df = TRUE)})

## return distribution
######################

return_dist = lapply(c(-0.75, -0.5, -0.25, 0.25, 0.5, 0.75), 
                     function(x) as.vector(replicate(100, arima.sim(model=list(ar=c(x)), n = 1000, 
                                                                     rand.gen = function(n) rnorm(n, sd = 0.01)))) )

return_dist_stat = lapply(return_dist, function(x){c(mean(x), sd(x), skewness(x), kurtosis(x))})

format(round(return_dist_stat[[1]], 3), nsmall = 3)
format(round(return_dist_stat[[2]], 3), nsmall = 3)
format(round(return_dist_stat[[3]], 3), nsmall = 3)
format(round(return_dist_stat[[4]], 3), nsmall = 3)
format(round(return_dist_stat[[5]], 3), nsmall = 3)
format(round(return_dist_stat[[6]], 3), nsmall = 3)


png("../figures/simulation/AR1_risk_measures.png", width = 800, height = 400)
plot_grid(ggplot(sim.risk.ar.1, aes(x = ts.param.ar.1, y = ES)) + geom_point() + theme_bw() + 
            xlab("AR(1) Coefficient"),
          ggplot(sim.risk.ar.1, aes(x = ts.param.ar.1, y = VaR)) + geom_point() + theme_bw() + 
            xlab("AR(1) Coefficient"),
          ggplot(sim.risk.ar.1, aes(x = ts.param.ar.1, y = sd)) + geom_point() + theme_bw() + 
            xlab("AR(1) Coefficient") + ylab("Volatility"),
          ggplot(sim.risk.ar.1, aes(x = ts.param.ar.1, y = CED)) + geom_point() + theme_bw() + 
            xlab("AR(1) Coefficient"),
          ncol = 2, align = 'v')
dev.off()

png("../figures/simulation/AR1_maxDrawdown_dist.png", width = 800, height = 400)
sim.risk.ar.1.densityList <- list()
for (i in c(4, 9, 14, 18, 19, 23, 28, 33)){
  sim.risk.ar.1.densityList[[toString(i)]] = 
    ggplot(df <- data.frame(maxDrawdown = sim.risk.ar.1.df[[i]]['maxDrawdown',]), 
           aes(x = maxDrawdown)) + geom_density(fill = 'snow3', colour = 'snow3') + theme_bw() + 
    xlab(substitute(paste(kappa[1], "=", m), list(m = ts.param.ar.1[i]))) +
    xlim(0, 1) + ylim(0, 5)
}
do.call("plot_grid", c(sim.risk.ar.1.densityList,
                        ncol = 4, align = 'v'))
dev.off()

###########
###########
## AR(2) ##
###########
###########

ts.param.ar2 = expand.grid(seq(from = -0.9, to = 0.9, by = 0.1), 
                         seq(from = -0.9, to = 0.9, by = 0.1))
ts.param.ar2 = ts.param.ar2[ts.param.ar2[, 1] != 0, ]
ts.param.ar2 = ts.param.ar2[ts.param.ar2[, 2] != 0, ]
ts.param.ar2 = ts.param.ar2[-which(abs(abs(ts.param.ar2[, 1]) + abs(ts.param.ar2[, 2]) - 1)<=1e-4), ]
rownames(ts.param.ar2) = NULL

# check stationary
roots.ts.param.2 = t(apply(ts.param.ar2, 1,
                           function(x) polyroot(c(-rev(x), 1))))
ts.param.ar2 = ts.param.ar2[-which(rowSums(abs(roots.ts.param.2) > 1) != 0), ]
rownames(ts.param.ar2) = NULL
colnames(ts.param.ar2) = c("AR1", "AR2")
# calculate the autocorrelation
sim.ar.2.acf = t(apply(ts.param.ar2, 1,
                 function(x) ARMAacf(ar = x, lag.max = 3)[2:4]))
colnames(sim.ar.2.acf) = c("ac1", "ac2", "ac3")
# simulation
sim.risk.ar.2 = apply(ts.param.ar2, 1, 
                       function(x){rep_sim_ts(model=list(ar=x), n=1000, 
                                              rand.gen = function(n) rnorm(n, sd = 0.01))})
sim.risk.ar.2 = data.frame(cbind(t(sim.risk.ar.2), sim.ar.2.acf))

plot_df = data.frame(cbind(ts.param.ar2, sim.risk.ar.2))
plot_df[abs(plot_df$Var2 - 0.1) < 1e-3, ]


## return distribution
######################

return_dist = lapply(c(-0.75, -0.5, -0.25, 0.25, 0.5, 0.75), 
                     function(x) as.vector(replicate(100, arima.sim(model=list(ar=c(x, 0.2)), n = 1000, 
                                                                    rand.gen = function(n) rnorm(n, sd = 0.01)))) )

return_dist_stat = lapply(return_dist, function(x){c(mean(x), sd(x), skewness(x), kurtosis(x))})

format(round(return_dist_stat[[1]], 3), nsmall = 3)
format(round(return_dist_stat[[2]], 3), nsmall = 3)
format(round(return_dist_stat[[3]], 3), nsmall = 3)
format(round(return_dist_stat[[4]], 3), nsmall = 3)
format(round(return_dist_stat[[5]], 3), nsmall = 3)
format(round(return_dist_stat[[6]], 3), nsmall = 3)

# aggregated plot
png("../figures/simulation/AR2_risk_measures_pos.png", width = 800, height = 400)
subset = ts.param.ar2[, 2] > 0
plot_grid(ggplot(data.frame(cbind(ts.param.ar2[subset, ], sim.risk.ar.2[subset, ])), 
                 aes(x = ac1, y = ES, group = factor(AR2))) + geom_point(aes(colour = AR2)) + theme_light() +
            xlab(expression(rho[1])) +
            scale_colour_gradient2(high="steelblue", mid="darksalmon") ,
          ggplot(data.frame(cbind(ts.param.ar2[subset, ], sim.risk.ar.2[subset, ])), 
                 aes(x = ac1, y = VaR, group = factor(AR2))) + geom_point(aes(colour = AR2)) + theme_light() +
            xlab(expression(rho[1])) +
            scale_colour_gradient2(high="steelblue", mid="darksalmon"),
          ggplot(data.frame(cbind(ts.param.ar2[subset, ], sim.risk.ar.2[subset, ])), 
                 aes(x = ac1, y = sd, group = factor(AR2))) + geom_point(aes(colour = AR2)) + theme_light() +
            xlab(expression(rho[1])) + ylab("Volatility") + 
            scale_colour_gradient2(high="steelblue", mid="darksalmon"),
          ggplot(data.frame(cbind(ts.param.ar2[subset, ], sim.risk.ar.2[subset, ])), 
                 aes(x = ac1, y = CED, group = factor(AR2))) + geom_line(aes(colour = AR2)) + theme_light() +
            xlab(expression(rho[1])) +
            scale_colour_gradient2(high="steelblue", mid="darksalmon"),
          ncol = 2, align = 'v')
dev.off()

png("../figures/simulation/AR2_risk_measures_neg.png", width = 800, height = 400)
subset = ts.param.ar2[, 2] < 0
plot_grid(ggplot(data.frame(cbind(ts.param.ar2[subset, ], sim.risk.ar.2[subset, ])), 
                 aes(x = ac1, y = ES, group = factor(AR2))) + geom_point(aes(colour = AR2)) + theme_light() +
            xlab(expression(rho[1])) +
            scale_colour_gradient2(mid="darksalmon", low="steelblue"),
          ggplot(data.frame(cbind(ts.param.ar2[subset, ], sim.risk.ar.2[subset, ])), 
                 aes(x = ac1, y = VaR, group = factor(AR2))) + geom_point(aes(colour = AR2)) + theme_light() +
            xlab(expression(rho[1])) +
            scale_colour_gradient2(mid="darksalmon", low="steelblue"),
          ggplot(data.frame(cbind(ts.param.ar2[subset, ], sim.risk.ar.2[subset, ])), 
                 aes(x = ac1, y = sd, group = factor(AR2))) + geom_point(aes(colour = AR2)) + theme_light() +
            xlab(expression(rho[1])) + ylab("Volatility") + 
            scale_colour_gradient2(mid="darksalmon", low="steelblue"),
          ggplot(data.frame(cbind(ts.param.ar2[subset, ], sim.risk.ar.2[subset, ])), 
                 aes(x = ac1, y = CED, group = factor(AR2))) + geom_line(aes(colour = AR2)) + theme_light() +
            xlab(expression(rho[1])) +
            scale_colour_gradient2(mid="darksalmon", low="steelblue"),
          ncol = 2, align = 'v')
dev.off()

png("../figures/simulation/AR2_risk_measures_neg_coef.png", width = 800, height = 400)
subset = ts.param.ar2[, 2] < 0
plot_grid(ggplot(data.frame(cbind(ts.param.ar2[subset, ], sim.risk.ar.2[subset, ])), 
                 aes(x = AR1, y = ES, group = factor(AR2))) + geom_point(aes(colour = AR2)) + theme_light() +
            xlab(expression(kappa[1])) +
            scale_colour_gradient2(mid="darksalmon", low="steelblue"),
          ggplot(data.frame(cbind(ts.param.ar2[subset, ], sim.risk.ar.2[subset, ])), 
                 aes(x = AR1, y = VaR, group = factor(AR2))) + geom_point(aes(colour = AR2)) + theme_light() +
            xlab(expression(kappa[1])) +
            scale_colour_gradient2(mid="darksalmon", low="steelblue"),
          ggplot(data.frame(cbind(ts.param.ar2[subset, ], sim.risk.ar.2[subset, ])), 
                 aes(x = AR1, y = sd, group = factor(AR2))) + geom_point(aes(colour = AR2)) + theme_light() +
            xlab(expression(kappa[1])) + ylab("Volatility") + 
            scale_colour_gradient2(mid="darksalmon", low="steelblue"),
          ggplot(data.frame(cbind(ts.param.ar2[subset, ], sim.risk.ar.2[subset, ])), 
                 aes(x = AR1, y = CED, group = factor(AR2))) + geom_line(aes(colour = AR2)) + theme_light() +
            xlab(expression(kappa[1])) +
            scale_colour_gradient2(mid="darksalmon", low="steelblue"),
          ncol = 2, align = 'v')
dev.off()

png("../figures/simulation/AR2_risk_measures_pos_coef.png", width = 800, height = 400)
subset = ts.param.ar2[, 2] > 0
plot_grid(ggplot(data.frame(cbind(ts.param.ar2[subset, ], sim.risk.ar.2[subset, ])), 
                 aes(x = AR1, y = ES, group = factor(AR2))) + geom_point(aes(colour = AR2)) + theme_light() +
            xlab(expression(kappa[1])) +
            scale_colour_gradient2(high="steelblue", mid="darksalmon"),
          ggplot(data.frame(cbind(ts.param.ar2[subset, ], sim.risk.ar.2[subset, ])), 
                 aes(x = AR1, y = VaR, group = factor(AR2))) + geom_point(aes(colour = AR2)) + theme_light() +
            xlab(expression(kappa[1])) +
            scale_colour_gradient2(high="steelblue", mid="darksalmon"),
          ggplot(data.frame(cbind(ts.param.ar2[subset, ], sim.risk.ar.2[subset, ])), 
                 aes(x = AR1, y = sd, group = factor(AR2))) + geom_point(aes(colour = AR2)) + theme_light() +
            xlab(expression(kappa[1])) + ylab("Volatility") + 
            scale_colour_gradient2(high="steelblue", mid="darksalmon"),
          ggplot(data.frame(cbind(ts.param.ar2[subset, ], sim.risk.ar.2[subset, ])), 
                 aes(x = AR1, y = CED, group = factor(AR2))) + geom_line(aes(colour = AR2)) + theme_light() +
            xlab(expression(kappa[1])) +
            scale_colour_gradient2(high="steelblue", mid="darksalmon"),
          ncol = 2, align = 'v')
dev.off()
## maxDrawdown distribution ##
##############################

ts.param.ar2[abs(ts.param.ar2[, 1] - 0.2)<0.001,]
subset.ar2 = seq(from = -0.7, to = 0.7, by = 0.1)
subset.ar2 = subset.ar2[abs(subset.ar2 - 0.2) > 0.001]
sim.risk.ar.2.df = lapply(subset.ar2, 
                          function(x){rep_sim_ts(model=list(ar=c(0.2, x)), n=1000, 
                                                 rand.gen = function(n) rnorm(n, sd = 0.01), return_df = TRUE)})

png("../figures/simulation/AR2_maxDrawdown_dist_kappa1_02.png", width = 800, height = 400)
sim.risk.ar.2.densityList <- list()
for (i in 2:13){
  sim.risk.ar.2.densityList[[toString(i)]] = 
    ggplot(df <- data.frame(maxDrawdown = sim.risk.ar.2.df[[i]]['maxDrawdown',]), 
           aes(x = maxDrawdown)) + geom_density(fill = 'snow3', colour = 'snow3') + theme_bw() + 
    xlab(substitute(paste(kappa[1], "= 0.2, ", kappa[2], "=", m), list(m = subset.ar2[i]))) +
    xlim(0, 1) + ylim(0, 5.1)
}
do.call("plot_grid", c(sim.risk.ar.2.densityList,
                       ncol = 4, align = 'v'))
dev.off()

# kappa1 = -0.2

ts.param.ar2[abs(ts.param.ar2[, 1] + 0.2)<0.001,]
subset.ar2 = seq(from = -0.7, to = 0.7, by = 0.1)
subset.ar2 = subset.ar2[abs(subset.ar2 - 0) > 0.001]
sim.risk.ar.2.df = lapply(subset.ar2, 
                          function(x){rep_sim_ts(model=list(ar=c(-0.2, x)), n=1000, 
                                                 rand.gen = function(n) rnorm(n, sd = 0.01), return_df = TRUE)})

png("../figures/simulation/AR2_maxDrawdown_dist_kappa1_-02.png", width = 800, height = 400)
sim.risk.ar.2.densityList <- list()
for (i in 2:13){
  sim.risk.ar.2.densityList[[toString(i)]] = 
    ggplot(df <- data.frame(maxDrawdown = sim.risk.ar.2.df[[i]]['maxDrawdown',]), 
           aes(x = maxDrawdown)) + geom_density(fill = 'snow3', colour = 'snow3') + theme_bw() + 
    xlab(substitute(paste(kappa[1], "= -0.2, ", kappa[2], "=", m), list(m = subset.ar2[i]))) +
    xlim(0, 1) + ylim(0, 6)
}
do.call("plot_grid", c(sim.risk.ar.2.densityList,
                       ncol = 4, align = 'v'))
dev.off()

# kappa2 = 0.2

ts.param.ar2[abs(ts.param.ar2[, 2] - 0.2)<0.001,]
subset.ar2 = seq(from = -0.7, to = 0.7, by = 0.1)
subset.ar2 = subset.ar2[abs(subset.ar2 - 0) > 0.001]
sim.risk.ar.2.df = lapply(subset.ar2, 
                          function(x){rep_sim_ts(model=list(ar=c(x, 0.2)), n=1000, 
                                                 rand.gen = function(n) rnorm(n, sd = 0.01), return_df = TRUE)})

png("../figures/simulation/AR2_maxDrawdown_dist_kappa2_02.png", width = 800, height = 400)
sim.risk.ar.2.densityList <- list()
for (i in 2:13){
  sim.risk.ar.2.densityList[[toString(i)]] = 
    ggplot(df <- data.frame(maxDrawdown = sim.risk.ar.2.df[[i]]['maxDrawdown',]), 
           aes(x = maxDrawdown)) + geom_density(fill = 'snow3', colour = 'snow3') + theme_bw() + 
    xlab(substitute(paste(kappa[2], "= 0.2, ", kappa[1], "=", m), list(m = subset.ar2[i]))) +
    xlim(0, 1) + ylim(0, 5)
}
do.call("plot_grid", c(sim.risk.ar.2.densityList,
                       ncol = 4, align = 'v'))
dev.off()

# kappa2 = -0.2

ts.param.ar2[abs(ts.param.ar2[, 2] + 0.2)<0.001,]
subset.ar2 = seq(from = -0.7, to = 0.7, by = 0.1)
subset.ar2 = subset.ar2[abs(subset.ar2 - 0) > 0.001]
sim.risk.ar.2.df = lapply(subset.ar2, 
                          function(x){rep_sim_ts(model=list(ar=c(x, -0.2)), n=1000, 
                                                 rand.gen = function(n) rnorm(n, sd = 0.01), return_df = TRUE)})

png("../figures/simulation/AR2_maxDrawdown_dist_kappa2_-02.png", width = 800, height = 400)
sim.risk.ar.2.densityList <- list()
for (i in 2:13){
  sim.risk.ar.2.densityList[[toString(i)]] = 
    ggplot(df <- data.frame(maxDrawdown = sim.risk.ar.2.df[[i]]['maxDrawdown',]), 
           aes(x = maxDrawdown)) + geom_density(fill = 'snow3', colour = 'snow3') + theme_bw() + 
    xlab(substitute(paste(kappa[2], "= -0.2, ", kappa[1], "=", m), list(m = subset.ar2[i]))) +
    xlim(0, 1) + ylim(0, 6)
}
do.call("plot_grid", c(sim.risk.ar.2.densityList,
                       ncol = 4, align = 'v'))
dev.off()

###########
###########
## MA(1) ##
###########
###########

ts.param.ma.1 = seq(from = -0.9, to = 0.9, by = 0.05)
ts.param.ma.1 = ts.param.ma.1[ts.param.ma.1 != 0]

sim.risk.ma.1 = sapply(ts.param.ma.1, 
                       function(x){rep_sim_ts(model=list(ma=x), n=1000, 
                                              rand.gen = function(n) rnorm(n, sd = 0.01))})
sim.risk.ma.1 = data.frame(cbind(t(sim.risk.ma.1), ts.param.ma.1))
rownames(sim.risk.ma.1) = ts.param.ma.1

sim.ma.1.acf = t(sapply(ts.param.ma.1,
                       function(x) ARMAacf(ma = c(x), lag.max = 3)[2:3]))
colnames(sim.ma.1.acf) = c("ac1", "ac2")

## return distribution
######################

return_dist = lapply(c(-0.75, -0.5, -0.25, 0.25, 0.5, 0.75), 
                     function(x) as.vector(replicate(100, arima.sim(model=list(ma = x), n = 1000, 
                                                                    rand.gen = function(n) rnorm(n, sd = 0.01)))) )

return_dist_stat = lapply(return_dist, function(x){c(mean(x), sd(x), skewness(x), kurtosis(x))})

format(round(return_dist_stat[[1]], 3), nsmall = 3)
format(round(return_dist_stat[[2]], 3), nsmall = 3)
format(round(return_dist_stat[[3]], 3), nsmall = 3)
format(round(return_dist_stat[[4]], 3), nsmall = 3)
format(round(return_dist_stat[[5]], 3), nsmall = 3)
format(round(return_dist_stat[[6]], 3), nsmall = 3)

###########################

png("../figures/simulation/MA1_risk_measures_coefficient.png", width = 800, height = 400)
plot_grid(ggplot(data.frame(cbind(sim.risk.ma.1, sim.ma.1.acf)), 
                            aes(x = ts.param.ma.1, y = ES)) + geom_point() + theme_bw() + 
            xlab("MA(1) Coefficient"),
          ggplot(data.frame(cbind(sim.risk.ma.1, sim.ma.1.acf)), 
                 aes(x = ts.param.ma.1, y = VaR)) + geom_point() + theme_bw() + 
            xlab("MA(1) Coefficient"),
          ggplot(data.frame(cbind(sim.risk.ma.1, sim.ma.1.acf)), 
                 aes(x = ts.param.ma.1, y = sd)) + geom_point() + theme_bw() + 
            xlab("MA(1) Coefficient"),
          ggplot(data.frame(cbind(sim.risk.ma.1, sim.ma.1.acf)), 
                 aes(x = ts.param.ma.1, y = CED)) + geom_point() + theme_bw() + 
            xlab("MA(1) Coefficient"),
          ncol = 2, align = 'v')
dev.off()

png("../figures/simulation/MA1_risk_measures_acf1.png", width = 800, height = 400)
plot_grid(ggplot(data.frame(cbind(sim.risk.ma.1, sim.ma.1.acf)), 
                 aes(x = ac1, y = ES)) + geom_point() + theme_bw() + 
            xlab(expression(rho[1])),
          ggplot(data.frame(cbind(sim.risk.ma.1, sim.ma.1.acf)), 
                 aes(x = ac1, y = VaR)) + geom_point() + theme_bw() + 
            xlab(expression(rho[1])),
          ggplot(data.frame(cbind(sim.risk.ma.1, sim.ma.1.acf)), 
                 aes(x = ac1, y = sd)) + geom_point() + theme_bw() + 
            xlab(expression(rho[1])),
          ggplot(data.frame(cbind(sim.risk.ma.1, sim.ma.1.acf)), 
                 aes(x = ac1, y = CED)) + geom_point() + theme_bw() + 
            xlab(expression(rho[1])),
          ncol = 2, align = 'v')
dev.off()

## maxDrawdown distribution ##
##############################

m1.param.subset = c(-0.75, -0.5, -0.25, -0.05, 0.05, 0.25, 0.5, 0.75)
sim.risk.ma.1.df = lapply(m1.param.subset, 
                          function(x){rep_sim_ts(model=list(ma=c(x)), n=1000, 
                                                 rand.gen = function(n) rnorm(n, sd = 0.01), return_df = TRUE)})

png("../figures/simulation/MA1_maxDrawdown_dist.png", width = 800, height = 400)
sim.risk.ma.1.densityList <- list()
for (i in 1:8){
  sim.risk.ma.1.densityList[[toString(i)]] = 
    ggplot(df <- data.frame(maxDrawdown = sim.risk.ma.1.df[[i]]['maxDrawdown',]), 
           aes(x = maxDrawdown)) + geom_density(fill = 'snow3', colour = 'snow3') + theme_bw() + 
    xlab(substitute(paste(theta[1], "=", m, ", ", rho[1], "=", r), 
                    list(m = m1.param.subset[i], r = ARMAacf(ma = c(m1.param.subset[i]), lag.max = 2)[2]))) +
    xlim(0, 1) + ylim(0, 9)
}
do.call("plot_grid", c(sim.risk.ma.1.densityList,
                       ncol = 4, align = 'v'))
dev.off()

###########
###########
## MA(2) ##
###########
###########

ts.param.ma2 = expand.grid(seq(from = -0.9, to = 0.9, by = 0.1), 
                         seq(from = -0.9, to = 0.9, by = 0.1))
ts.param.ma2 = ts.param.ma2[ts.param.ma2[, 1] != 0, ]
ts.param.ma2 = ts.param.ma2[ts.param.ma2[, 2] != 0, ]
colnames(ts.param.ma2) = c("MA1", "MA2")

# calculate the autocorrelation
sim.ma.2.acf = t(apply(ts.param.ma2, 1,
                       function(x) ARMAacf(ma = x, lag.max = 2)[2:3]))
colnames(sim.ma.2.acf) = c("ac1", "ac2")

# simulation
sim.risk.ma.2 = apply(ts.param.ma2, 1, 
                      function(x){rep_sim_ts(model=list(ma=x), n=1000, 
                                             rand.gen = function(n) rnorm(n, sd = 0.01))})
sim.risk.ma.2 = data.frame(cbind(t(sim.risk.ma.2), sim.ma.2.acf))

## return distribution
######################

return_dist = lapply(c(-0.75, -0.5, -0.25, 0.25, 0.5, 0.75), 
                     function(x) as.vector(replicate(100, arima.sim(model=list(ma = c(x, 0.2)), n = 1000, 
                                                                    rand.gen = function(n) rnorm(n, sd = 0.01)))) )

return_dist_stat = lapply(return_dist, function(x){c(mean(x), sd(x), skewness(x), kurtosis(x))})

format(round(return_dist_stat[[1]], 3), nsmall = 3)
format(round(return_dist_stat[[2]], 3), nsmall = 3)
format(round(return_dist_stat[[3]], 3), nsmall = 3)
format(round(return_dist_stat[[4]], 3), nsmall = 3)
format(round(return_dist_stat[[5]], 3), nsmall = 3)
format(round(return_dist_stat[[6]], 3), nsmall = 3)

png("../figures/simulation/MA2_risk_measures_pos.png", width = 800, height = 400)
subset = ts.param.ma2[, 2] > 0
plot_grid(ggplot(data.frame(cbind(ts.param.ma2[subset, ], sim.risk.ma.2[subset, ])), 
                 aes(x = ac1, y = ES, group = factor(MA2))) + geom_point(aes(colour = MA2)) + theme_light() +
            xlab(expression(rho[1])) +
            scale_colour_gradient2(high="steelblue", mid="darksalmon") ,
          ggplot(data.frame(cbind(ts.param.ma2[subset, ], sim.risk.ma.2[subset, ])), 
                 aes(x = ac1, y = VaR, group = factor(MA2))) + geom_point(aes(colour = MA2)) + theme_light() +
            xlab(expression(rho[1])) +
            scale_colour_gradient2(high="steelblue", mid="darksalmon"),
          ggplot(data.frame(cbind(ts.param.ma2[subset, ], sim.risk.ma.2[subset, ])), 
                 aes(x = ac1, y = sd, group = factor(MA2))) + geom_point(aes(colour = MA2)) + theme_light() +
            xlab(expression(rho[1])) + ylab("Volatility") + 
            scale_colour_gradient2(high="steelblue", mid="darksalmon"),
          ggplot(data.frame(cbind(ts.param.ma2[subset, ], sim.risk.ma.2[subset, ])), 
                 aes(x = ac1, y = CED, group = factor(MA2))) + geom_line(aes(colour = MA2)) + theme_light() +
            xlab(expression(rho[1])) +
            scale_colour_gradient2(high="steelblue", mid="darksalmon"),
          ncol = 2, align = 'v')
dev.off()

png("../figures/simulation/MA2_risk_measures_neg.png", width = 800, height = 400)
subset = ts.param.ma2[, 2] < 0
plot_grid(ggplot(data.frame(cbind(ts.param.ma2[subset, ], sim.risk.ma.2[subset, ])), 
                 aes(x = ac1, y = ES, group = factor(MA2))) + geom_point(aes(colour = MA2)) + theme_light() +
            xlab(expression(rho[1])) +
            scale_colour_gradient2(mid="darksalmon", low="steelblue") ,
          ggplot(data.frame(cbind(ts.param.ma2[subset, ], sim.risk.ma.2[subset, ])), 
                 aes(x = ac1, y = VaR, group = factor(MA2))) + geom_point(aes(colour = MA2)) + theme_light() +
            xlab(expression(rho[1])) +
            scale_colour_gradient2(mid="darksalmon", low="steelblue"),
          ggplot(data.frame(cbind(ts.param.ma2[subset, ], sim.risk.ma.2[subset, ])), 
                 aes(x = ac1, y = sd, group = factor(MA2))) + geom_point(aes(colour = MA2)) + theme_light() +
            xlab(expression(rho[1])) + ylab("Volatility") + 
            scale_colour_gradient2(mid="darksalmon", low="steelblue"),
          ggplot(data.frame(cbind(ts.param.ma2[subset, ], sim.risk.ma.2[subset, ])), 
                 aes(x = ac1, y = CED, group = factor(MA2))) + geom_line(aes(colour = MA2)) + theme_light() +
            xlab(expression(rho[1])) +
            scale_colour_gradient2(mid="darksalmon", low="steelblue"),
          ncol = 2, align = 'v')
dev.off()

png("../figures/simulation/MA2_risk_measures_pos_coef.png", width = 800, height = 400)
subset = ts.param.ma2[, 2] > 0
plot_grid(ggplot(data.frame(cbind(ts.param.ma2[subset, ], sim.risk.ma.2[subset, ])), 
                 aes(x = MA1, y = ES, group = factor(MA2))) + geom_point(aes(colour = MA2)) + theme_light() +
            xlab(expression(theta[1])) +
            scale_colour_gradient2(high="steelblue", mid="darksalmon") ,
          ggplot(data.frame(cbind(ts.param.ma2[subset, ], sim.risk.ma.2[subset, ])), 
                 aes(x = MA1, y = VaR, group = factor(MA2))) + geom_point(aes(colour = MA2)) + theme_light() +
            xlab(expression(theta[1])) +
            scale_colour_gradient2(high="steelblue", mid="darksalmon"),
          ggplot(data.frame(cbind(ts.param.ma2[subset, ], sim.risk.ma.2[subset, ])), 
                 aes(x = MA1, y = sd, group = factor(MA2))) + geom_point(aes(colour = MA2)) + theme_light() +
            xlab(expression(theta[1])) + ylab("Volatility") + 
            scale_colour_gradient2(high="steelblue", mid="darksalmon"),
          ggplot(data.frame(cbind(ts.param.ma2[subset, ], sim.risk.ma.2[subset, ])), 
                 aes(x = MA1, y = CED, group = factor(MA2))) + geom_line(aes(colour = MA2)) + theme_light() +
            xlab(expression(theta[1])) +
            scale_colour_gradient2(high="steelblue", mid="darksalmon"),
          ncol = 2, align = 'v')
dev.off()

png("../figures/simulation/MA2_risk_measures_neg_coef.png", width = 800, height = 400)
subset = ts.param.ma2[, 2] < 0
plot_grid(ggplot(data.frame(cbind(ts.param.ma2[subset, ], sim.risk.ma.2[subset, ])), 
                 aes(x = MA1, y = ES, group = factor(MA2))) + geom_point(aes(colour = MA2)) + theme_light() +
            xlab(expression(theta[1])) +
            scale_colour_gradient2(mid="darksalmon", low="steelblue") ,
          ggplot(data.frame(cbind(ts.param.ma2[subset, ], sim.risk.ma.2[subset, ])), 
                 aes(x = MA1, y = VaR, group = factor(MA2))) + geom_point(aes(colour = MA2)) + theme_light() +
            xlab(expression(theta[1])) +
            scale_colour_gradient2(mid="darksalmon", low="steelblue"),
          ggplot(data.frame(cbind(ts.param.ma2[subset, ], sim.risk.ma.2[subset, ])), 
                 aes(x = MA1, y = sd, group = factor(MA2))) + geom_point(aes(colour = MA2)) + theme_light() +
            xlab(expression(theta[1])) + ylab("Volatility") + 
            scale_colour_gradient2(mid="darksalmon", low="steelblue"),
          ggplot(data.frame(cbind(ts.param.ma2[subset, ], sim.risk.ma.2[subset, ])), 
                 aes(x = MA1, y = CED, group = factor(MA2))) + geom_line(aes(colour = MA2)) + theme_light() +
            xlab(expression(theta[1])) +
            scale_colour_gradient2(mid="darksalmon", low="steelblue"),
          ncol = 2, align = 'v')
dev.off()

## simulation ##
################

subset.ma2 = seq(from = -0.9, to = 0.9, by = 0.1)
subset.ma2 = subset.ar2[abs(subset.ar2 - 0) > 0.001]
sim.risk.ma.2.df = lapply(subset.ma2, 
                          function(x){rep_sim_ts(model=list(ma=c(0.2, x)), n=1000, 
                                                 rand.gen = function(n) rnorm(n, sd = 0.01), return_df = TRUE)})

png("../figures/simulation/MA2_maxDrawdown_dist_theta1_02.png", width = 800, height = 400)
sim.risk.ma.2.densityList <- list()
for (i in 2:13){
  sim.risk.ma.2.densityList[[toString(i)]] = 
    ggplot(df <- data.frame(maxDrawdown = sim.risk.ma.2.df[[i]]['maxDrawdown',]), 
           aes(x = maxDrawdown)) + geom_density(fill = 'snow3', colour = 'snow3') + theme_bw() + 
    xlab(substitute(paste(theta[1], "= 0.2, ", theta[2], "=", m), list(m = subset.ma2[i]))) +
    xlim(0, 1) + ylim(0, 5.5)
}
do.call("plot_grid", c(sim.risk.ma.2.densityList,
                       ncol = 4, align = 'v'))
dev.off()


subset.ma2 = seq(from = -0.9, to = 0.9, by = 0.1)
subset.ma2 = subset.ar2[abs(subset.ar2 - 0) > 0.001]
sim.risk.ma.2.df = lapply(subset.ma2, 
                          function(x){rep_sim_ts(model=list(ma=c(-0.2, x)), n=1000, 
                                                 rand.gen = function(n) rnorm(n, sd = 0.01), return_df = TRUE)})

png("../figures/simulation/MA2_maxDrawdown_dist_theta1_-02.png", width = 800, height = 400)
sim.risk.ma.2.densityList <- list()
for (i in 2:13){
  sim.risk.ma.2.densityList[[toString(i)]] = 
    ggplot(df <- data.frame(maxDrawdown = sim.risk.ma.2.df[[i]]['maxDrawdown',]), 
           aes(x = maxDrawdown)) + geom_density(fill = 'snow3', colour = 'snow3') + theme_bw() + 
    xlab(substitute(paste(theta[1], "= -0.2, ", theta[2], "=", m), list(m = subset.ma2[i]))) +
    xlim(0, 1) + ylim(0, 12)
}
do.call("plot_grid", c(sim.risk.ma.2.densityList,
                       ncol = 4, align = 'v'))
dev.off()


subset.ma2 = seq(from = -0.9, to = 0.9, by = 0.1)
subset.ma2 = subset.ar2[abs(subset.ar2 - 0) > 0.001]
sim.risk.ma.2.df = lapply(subset.ma2, 
                          function(x){rep_sim_ts(model=list(ma=c(x, 0.2)), n=1000, 
                                                 rand.gen = function(n) rnorm(n, sd = 0.01), return_df = TRUE)})

png("../figures/simulation/MA2_maxDrawdown_dist_theta2_02.png", width = 800, height = 400)
sim.risk.ma.2.densityList <- list()
for (i in 2:13){
  sim.risk.ma.2.densityList[[toString(i)]] = 
    ggplot(df <- data.frame(maxDrawdown = sim.risk.ma.2.df[[i]]['maxDrawdown',]), 
           aes(x = maxDrawdown)) + geom_density(fill = 'snow3', colour = 'snow3') + theme_bw() + 
    xlab(substitute(paste(theta[2], "= 0.2, ", theta[1], "=", m), list(m = subset.ma2[i]))) +
    xlim(0, 1) + ylim(0, 5.1)
}
do.call("plot_grid", c(sim.risk.ma.2.densityList,
                       ncol = 4, align = 'v'))
dev.off()


subset.ma2 = seq(from = -0.9, to = 0.9, by = 0.1)
subset.ma2 = subset.ar2[abs(subset.ar2 - 0) > 0.001]
sim.risk.ma.2.df = lapply(subset.ma2, 
                          function(x){rep_sim_ts(model=list(ma=c(x, -0.2)), n=1000, 
                                                 rand.gen = function(n) rnorm(n, sd = 0.01), return_df = TRUE)})

png("../figures/simulation/MA2_maxDrawdown_dist_theta2_-02.png", width = 800, height = 400)
sim.risk.ma.2.densityList <- list()
for (i in 2:13){
  sim.risk.ma.2.densityList[[toString(i)]] = 
    ggplot(df <- data.frame(maxDrawdown = sim.risk.ma.2.df[[i]]['maxDrawdown',]), 
           aes(x = maxDrawdown)) + geom_density(fill = 'snow3', colour = 'snow3') + theme_bw() + 
    xlab(substitute(paste(theta[2], "= 0.2, ", theta[1], "=", m), list(m = subset.ma2[i]))) +
    xlim(0, 1) + ylim(0, 10)
}
do.call("plot_grid", c(sim.risk.ma.2.densityList,
                       ncol = 4, align = 'v'))
dev.off()

################
################
## ARMA(1, 1) ##
################
################

ts.param.ar1.ma1 = expand.grid(seq(from = -0.9, to = 0.9, by = 0.1), 
                            seq(from = -0.9, to = 0.9, by = 0.1))
ts.param.ar1.ma1 = ts.param.ar1.ma1[ts.param.ar1.ma1[, 1] != 0, ]
ts.param.ar1.ma1 = ts.param.ar1.ma1[ts.param.ar1.ma1[, 2] != 0, ]
colnames(ts.param.ar1.ma1) = c("AR1", "MA1")

# calculate the autocorrelation
sim.ar1.ma1.acf = t(apply(ts.param.ar1.ma1, 1,
                       function(x) ARMAacf(ar = x[1], ma = x[2], lag.max = 3)[2:4]))
colnames(sim.ar1.ma1.acf) = c("ac1", "ac2", "ac3")

# simulation
sim.risk.ar1.ma1 = apply(ts.param.ar1.ma1, 1, 
                      function(x){rep_sim_ts(model=list(ar = x[1], ma = x[2]), n=1000, 
                                             rand.gen = function(n) rnorm(n, sd = 0.01))})
sim.risk.ar1.ma1 = data.frame(cbind(t(sim.risk.ar1.ma1), sim.ar1.ma1.acf))

png("../figures/simulation/AR1MA1_risk_measures_pos.png", width = 800, height = 400)
subset = ts.param.ar1.ma1[, 2] > 0
plot_grid(ggplot(data.frame(cbind(ts.param.ar1.ma1[subset, ], sim.risk.ar1.ma1[subset, ])), 
                 aes(x = ac1, y = ES, group = factor(MA1))) + geom_point(aes(colour = MA1)) + theme_light() +
            xlab(expression(rho[1])) +
            scale_colour_gradient2(high="steelblue", mid="darksalmon") ,
          ggplot(data.frame(cbind(ts.param.ar1.ma1[subset, ], sim.risk.ar1.ma1[subset, ])), 
                 aes(x = ac1, y = VaR, group = factor(MA1))) + geom_point(aes(colour = MA1)) + theme_light() +
            xlab(expression(rho[1])) +
            scale_colour_gradient2(high="steelblue", mid="darksalmon"),
          ggplot(data.frame(cbind(ts.param.ar1.ma1[subset, ], sim.risk.ar1.ma1[subset, ])), 
                 aes(x = ac1, y = sd, group = factor(MA1))) + geom_point(aes(colour = MA1)) + theme_light() +
            xlab(expression(rho[1])) + ylab("Volatility") + 
            scale_colour_gradient2(high="steelblue", mid="darksalmon"),
          ggplot(data.frame(cbind(ts.param.ar1.ma1[subset, ], sim.risk.ar1.ma1[subset, ])), 
                 aes(x = ac1, y = CED, group = factor(MA1))) + geom_line(aes(colour = MA1)) + theme_light() +
            xlab(expression(rho[1])) +
            scale_colour_gradient2(high="steelblue", mid="darksalmon"),
          ncol = 2, align = 'v')
dev.off()


png("../figures/simulation/AR1MA1_risk_measures_neg.png", width = 800, height = 400)
subset = ts.param.ar1.ma1[, 2] < 0
plot_grid(ggplot(data.frame(cbind(ts.param.ar1.ma1[subset, ], sim.risk.ar1.ma1[subset, ])), 
                 aes(x = ac1, y = ES, group = factor(MA1))) + geom_point(aes(colour = MA1)) + theme_light() +
            xlab(expression(rho[1])) +
            scale_colour_gradient2(mid="steelblue", low="darksalmon") ,
          ggplot(data.frame(cbind(ts.param.ar1.ma1[subset, ], sim.risk.ar1.ma1[subset, ])), 
                 aes(x = ac1, y = VaR, group = factor(MA1))) + geom_point(aes(colour = MA1)) + theme_light() +
            xlab(expression(rho[1])) +
            scale_colour_gradient2(mid="steelblue", low="darksalmon"),
          ggplot(data.frame(cbind(ts.param.ar1.ma1[subset, ], sim.risk.ar1.ma1[subset, ])), 
                 aes(x = ac1, y = sd, group = factor(MA1))) + geom_point(aes(colour = MA1)) + theme_light() +
            xlab(expression(rho[1])) + ylab("Volatility") + 
            scale_colour_gradient2(mid="steelblue", low="darksalmon"),
          ggplot(data.frame(cbind(ts.param.ar1.ma1[subset, ], sim.risk.ar1.ma1[subset, ])), 
                 aes(x = ac1, y = CED, group = factor(MA1))) + geom_line(aes(colour = MA1)) + theme_light() +
            xlab(expression(rho[1])) +
            scale_colour_gradient2(mid="steelblue", low="darksalmon"),
          ncol = 2, align = 'v')
dev.off()

png("../figures/simulation/AR1MA1_risk_measures_pos_coef.png", width = 800, height = 400)
subset = ts.param.ar1.ma1[, 2] > 0
plot_grid(ggplot(data.frame(cbind(ts.param.ar1.ma1[subset, ], sim.risk.ar1.ma1[subset, ])), 
                 aes(x = AR1, y = ES, group = factor(MA1))) + geom_point(aes(colour = MA1)) + theme_light() +
            xlab(expression(kappa[1])) +
            scale_colour_gradient2(high="steelblue", mid="darksalmon") ,
          ggplot(data.frame(cbind(ts.param.ar1.ma1[subset, ], sim.risk.ar1.ma1[subset, ])), 
                 aes(x = AR1, y = VaR, group = factor(MA1))) + geom_point(aes(colour = MA1)) + theme_light() +
            xlab(expression(kappa[1])) +
            scale_colour_gradient2(high="steelblue", mid="darksalmon"),
          ggplot(data.frame(cbind(ts.param.ar1.ma1[subset, ], sim.risk.ar1.ma1[subset, ])), 
                 aes(x = AR1, y = sd, group = factor(MA1))) + geom_point(aes(colour = MA1)) + theme_light() +
            xlab(expression(kappa[1])) + ylab("Volatility") + 
            scale_colour_gradient2(high="steelblue", mid="darksalmon"),
          ggplot(data.frame(cbind(ts.param.ar1.ma1[subset, ], sim.risk.ar1.ma1[subset, ])), 
                 aes(x = AR1, y = CED, group = factor(MA1))) + geom_line(aes(colour = MA1)) + theme_light() +
            xlab(expression(kappa[1])) +
            scale_colour_gradient2(high="steelblue", mid="darksalmon"),
          ncol = 2, align = 'v')
dev.off()

png("../figures/simulation/AR1MA1_risk_measures_neg_coef.png", width = 800, height = 400)
subset = ts.param.ar1.ma1[, 2] < 0
plot_grid(ggplot(data.frame(cbind(ts.param.ar1.ma1[subset, ], sim.risk.ar1.ma1[subset, ])), 
                 aes(x = AR1, y = ES, group = factor(MA1))) + geom_point(aes(colour = MA1)) + theme_light() +
            xlab(expression(kappa[1])) +
            scale_colour_gradient2(mid="steelblue", low="darksalmon") ,
          ggplot(data.frame(cbind(ts.param.ar1.ma1[subset, ], sim.risk.ar1.ma1[subset, ])), 
                 aes(x = AR1, y = VaR, group = factor(MA1))) + geom_point(aes(colour = MA1)) + theme_light() +
            xlab(expression(kappa[1])) +
            scale_colour_gradient2(mid="steelblue", low="darksalmon"),
          ggplot(data.frame(cbind(ts.param.ar1.ma1[subset, ], sim.risk.ar1.ma1[subset, ])), 
                 aes(x = AR1, y = sd, group = factor(MA1))) + geom_point(aes(colour = MA1)) + theme_light() +
            xlab(expression(kappa[1])) + ylab("Volatility") + 
            scale_colour_gradient2(mid="steelblue", low="darksalmon"),
          ggplot(data.frame(cbind(ts.param.ar1.ma1[subset, ], sim.risk.ar1.ma1[subset, ])), 
                 aes(x = AR1, y = CED, group = factor(MA1))) + geom_line(aes(colour = MA1)) + theme_light() +
            xlab(expression(kappa[1])) +
            scale_colour_gradient2(mid="steelblue", low="darksalmon"),
          ncol = 2, align = 'v')
dev.off()

## simulation ##
################






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








