
library(ggplot2)
library(cowplot)

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

###########
###########
## AR(1) ##
###########
###########

test = arima.sim(model = list(ar = 0.3), n = 63, 
          rand.gen = function(n) 0.01*rt(n, df = 4))

mean(test)
sd(test)
skewness(test)
kurtosis(test)
maxDrawdown(as.xts(test))
ES(test)
VaR(test)
plot(test)

ts.param.ar.1 = seq(from = -0.3, to = 0.3, by = 0.02)
ts.param.ar.1 = ts.param.ar.1[ts.param.ar.1 != 0]
sim.risk.ar.1 = sapply(ts.param.ar.1, 
                       function(x){rep_sim_ts(rep = 1000, model=list(ar=x), n=63, 
                                              rand.gen = function(n) 0.01*rt(n, df = 4))})
sim.risk.ar.1 = data.frame(cbind(t(sim.risk.ar.1), ts.param.ar.1))
sim.risk.ar.1.df = lapply(ts.param.ar.1, 
                          function(x){rep_sim_ts(rep = 1000, model=list(ar=x), n=63, 
                                                 rand.gen = function(n) 0.01*rt(n, df = 4), return_df = TRUE)})

png("../figures/simulation/T_dist_AR1_risk_measures.png", width = 800, height = 400)
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

png("../figures/simulation/T_dist_AR1_maxDrawdown_dist.png", width = 800, height = 400)
sim.risk.ar.1.densityList <- list()
for (i in c(1, 6, 11, 20, 25, 30)){
  sim.risk.ar.1.densityList[[toString(i)]] = 
    ggplot(df <- data.frame(maxDrawdown = sim.risk.ar.1.df[[i]]['maxDrawdown',]), 
           aes(x = maxDrawdown)) + geom_density(fill = 'snow3', colour = 'snow3') + theme_bw() + 
    xlab(substitute(paste(kappa[1], "=", m), list(m = ts.param.ar.1[i]))) +
    xlim(0, 1) + ylim(0, 11)
}
do.call("plot_grid", c(sim.risk.ar.1.densityList,
                       ncol = 3, align = 'v'))
dev.off()

###########
###########
## AR(2) ##
###########
###########

ts.param.ar2 = expand.grid(seq(from = -0.3, to = 0.3, by = 0.05), 
                           seq(from = -0.3, to = 0.3, by = 0.05))
ts.param.ar2 = ts.param.ar2[ts.param.ar2[, 1] != 0, ]
ts.param.ar2 = ts.param.ar2[ts.param.ar2[, 2] != 0, ]
rownames(ts.param.ar2) = NULL

# calculate the autocorrelation
sim.ar.2.acf = t(apply(ts.param.ar2, 1,
                       function(x) ARMAacf(ar = x, lag.max = 3)[2:4]))
colnames(sim.ar.2.acf) = c("ac1", "ac2", "ac3")

# simulation
sim.risk.ar.2 = apply(ts.param.ar2, 1, 
                      function(x){rep_sim_ts(rep = 1000, model=list(ar=x), n=63, 
                                             rand.gen = function(n) 0.01*rt(n, df = 4))})
sim.risk.ar.2 = data.frame(cbind(t(sim.risk.ar.2), sim.ar.2.acf))

plot_df = data.frame(cbind(ts.param.ar2, sim.risk.ar.2))
plot_df[abs(plot_df$Var2 - 0.1) < 1e-3, ]

png("../figures/simulation/T_dist_AR2_risk_measures.png", width = 800, height = 400)
plot_grid(ggplot(data.frame(cbind(ts.param.ar2, sim.risk.ar.2)), 
                 aes(x = ac1, y = ES, group = factor(Var2))) + geom_point(aes(colour = Var2)) + theme_light() +
            xlab(expression(rho[1])) +
            scale_colour_gradient2(mid="darksalmon", low="steelblue", high="steelblue"),
          ggplot(data.frame(cbind(ts.param.ar2, sim.risk.ar.2)), 
                 aes(x = ac1, y = VaR, group = factor(Var2))) + geom_point(aes(colour = Var2)) + theme_light() +
            xlab(expression(rho[1])) +
            scale_colour_gradient2(mid="darksalmon", low="steelblue", high="steelblue"),
          ggplot(data.frame(cbind(ts.param.ar2, sim.risk.ar.2)), 
                 aes(x = ac1, y = sd, group = factor(Var2))) + geom_point(aes(colour = Var2)) + theme_light() +
            xlab(expression(rho[1])) + ylab("Volatility") + 
            scale_colour_gradient2(mid="darksalmon", low="steelblue", high="steelblue"),
          ggplot(data.frame(cbind(ts.param.ar2, sim.risk.ar.2)), 
                 aes(x = ac1, y = CED, group = factor(Var2))) + geom_line(aes(colour = Var2)) + theme_light() +
            xlab(expression(rho[1])) +
            scale_colour_gradient2(mid="darksalmon", low="steelblue", high="steelblue"),
          ncol = 2, align = 'v')
dev.off()

## maxDrawdown distribution ##
##############################

subset.ar2 = seq(from = -0.3, to = 0.3, by = 0.02)
subset.ar2 = subset.ar2[abs(subset.ar2 - 0.2) > 0.001]
sim.risk.ar.2.df = lapply(subset.ar2, 
                          function(x){rep_sim_ts(model=list(ar=c(0.2, x)), n=63, 
                                                 rand.gen = function(n) 0.01*rt(n, df = 4), return_df = TRUE)})

png("../figures/simulation/T_dist_AR2_maxDrawdown_dist_kappa1_02.png", width = 800, height = 400)
sim.risk.ar.2.densityList <- list()
for (i in 2:13){
  sim.risk.ar.2.densityList[[toString(i)]] = 
    ggplot(df <- data.frame(maxDrawdown = sim.risk.ar.2.df[[i]]['maxDrawdown',]), 
           aes(x = maxDrawdown)) + geom_density(fill = 'snow3', colour = 'snow3') + theme_bw() + 
    xlab(substitute(paste(kappa[1], "= 0.2, ", kappa[2], "=", m), list(m = subset.ar2[i]))) +
    xlim(0, 1) + ylim(0, 10)
}
do.call("plot_grid", c(sim.risk.ar.2.densityList,
                       ncol = 4, align = 'v'))
dev.off()

###########
###########
## MA(1) ##
###########
###########

ts.param.ma.1 = seq(from = -0.3, to = 0.3, by = 0.02)
ts.param.ma.1 = ts.param.ma.1[ts.param.ma.1 != 0]

sim.risk.ma.1 = sapply(ts.param.ma.1, 
                       function(x){rep_sim_ts(model=list(ma=x), n=63, 
                                              rand.gen = function(n)  0.01*rt(n, df = 4))})
sim.risk.ma.1 = data.frame(cbind(t(sim.risk.ma.1), ts.param.ma.1))
rownames(sim.risk.ma.1) = ts.param.ma.1

sim.ma.1.acf = t(sapply(ts.param.ma.1,
                        function(x) ARMAacf(ma = c(x), lag.max = 3)[2:3]))
colnames(sim.ma.1.acf) = c("ac1", "ac2")

png("../figures/simulation/T_dist_MA1_risk_measures_coefficient.png", width = 800, height = 400)
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

png("../figures/simulation/T_dist_MA1_risk_measures_acf1.png", width = 800, height = 400)
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

m1.param.subset = c(-0.3, -0.2, -0.1, 0.1, 0.2, 0.3)
sim.risk.ma.1.df = lapply(m1.param.subset, 
                          function(x){rep_sim_ts(model=list(ma=c(x)), n=63, 
                                                 rand.gen = function(n) 0.01*rt(n, df = 4), return_df = TRUE)})

png("../figures/simulation/T_dist_MA1_maxDrawdown_dist.png", width = 800, height = 400)
sim.risk.ma.1.densityList <- list()
for (i in 1:6){
  sim.risk.ma.1.densityList[[toString(i)]] = 
    ggplot(df <- data.frame(maxDrawdown = sim.risk.ma.1.df[[i]]['maxDrawdown',]), 
           aes(x = maxDrawdown)) + geom_density(fill = 'snow3', colour = 'snow3') + theme_bw() + 
    xlab(substitute(paste(theta[1], "=", m, ", ", rho[1], "=", r), 
                    list(m = m1.param.subset[i], r = ARMAacf(ma = c(m1.param.subset[i]), lag.max = 2)[2]))) +
    xlim(0, 1) + ylim(0, 12)
}
do.call("plot_grid", c(sim.risk.ma.1.densityList,
                       ncol = 3, align = 'v'))
dev.off()

