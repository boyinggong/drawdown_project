
library(ggplot2)
library(cowplot)
library(xts)

source("risk_diagostics_function.R")

x=0.1
test_ts = sim_ts(p = 0.95, model=list(ar=x), n=1000, 
                 rand.gen = function(n) rnorm(n, sd = 0.01))

###################################################################
##### comparative of negative and positive serial correlation #####
###################################################################

pos_example = as.vector(arima.sim(model = list(ar=0.5), n = 100, 
                                  rand.gen = function(n) rnorm(n, sd = 0.01)))
neg_example = as.vector(arima.sim(model = list(ar=-0.5), n = 100, 
                                  rand.gen = function(n) rnorm(n, sd = 0.01)))
pos_example_p = cumprod(1+pos_example)
neg_example_p = cumprod(1+neg_example)
maxDrawdown(pos_example)
maxDrawdown(neg_example)

png("../figures/simulation/Comparison_pos_neg_autocorrelation.png", width = 800, height = 400)
plot_grid(ggplot(data.frame(x = 1:100, 
                  y = pos_example), aes(x = x, y = y)) + 
  geom_line() + theme_light() + ylim(-0.05, 0.05) + ylab("Return") + xlab("Time"),
  ggplot(data.frame(x = 1:100, 
                    y = pos_example_p), aes(x = x, y = y)) + 
    geom_line() + theme_light() + ylim(0.75, 1.25) + ylab("Price") + xlab("Time"),
  ggplot(data.frame(x = 1:100, 
                    y = neg_example), aes(x = x, y = y)) + 
    geom_line() + theme_light() + ylim(-0.05, 0.05) + ylab("Return") + xlab("Time"),
  ggplot(data.frame(x = 1:100, 
                    y = neg_example_p), aes(x = x, y = y)) + 
    geom_line() + theme_light() + ylim(0.75, 1.25) + ylab("Price") + xlab("Time"),
  nrow = 2)
dev.off()


###################################################################
###################################################################


# AR1
var.ar1 = 0.0001/(1 - ts.param.ar.1^2)

calc_ar1_sd = function(ts.param.ar.1){
  sqrt(0.0001/(1 - ts.param.ar.1^2))
}
calc_ar1_sd(ts.param.ar.1)
0.0001/calc_ar1_sd(ts.param.ar.1)

# AR2
head(ts.param.ar2)

var.ar2 = 0.0001 / (1-ts.param.ar2["Var1"]^2-ts.param.ar2["Var2"]^2-
                 2*ts.param.ar2["Var1"]^2*ts.param.ar2["Var2"]/(1-ts.param.ar2["Var2"]))

plot(sim.ar.2.acf[, 1], t(sqrt(var.ar2)))

calc_ar2_sd = function(ts.param.ar2){
  return(sqrt(0.0001 / (1-ts.param.ar2["Var1"]^2-ts.param.ar2["Var2"]^2-
                          2*ts.param.ar2["Var1"]^2*ts.param.ar2["Var2"]/(1-ts.param.ar2["Var2"]))))
}

plot(sim.ar.2.acf[, 1], t(calc_ar2_sd(ts.param.ar2)))
0.0001/calc_ar2_sd(ts.param.ar2)

# MA1
var.ma1 = (ts.param.ma.1^2 + 1) * 0.0001

plot(sim.ma.1.acf[, 1], t(sqrt(var.ma1)))

calc_ma1_sd = function(ts.param.ma.1){
  sqrt((ts.param.ma.1^2 + 1) * 0.0001)
}

plot(sim.ma.1.acf[, 1], t(calc_ma1_sd(ts.param.ma.1)))
calc_ma1_sd(ts.param.ma.1[1])

# MA2
var.ma2 = (ts.param.ma2[, 1]^2 + ts.param.ma2[, 2]^2 + 1) * 0.0001

plot(sim.ma.2.acf[, 1], t(sqrt(var.ma2)))

calc_ma2_sd = function(ts.param.ma2){
  sqrt((ts.param.ma2[1]^2 + ts.param.ma2[2]^2 + 1) * 0.0001)
}

plot(sim.ma.2.acf[, 1], t(calc_ma2_sd(ts.param.ma2)))
calc_ma2_sd(ts.param.ma2[1, ])

# ARMA(1, 1)
var.ar1.ma1 = 0.0001 * (ts.param.ar1.ma1[, 2]^2 + 2*ts.param.ar1.ma1[, 1]*ts.param.ar1.ma1[, 2] + 1) / 
  (1-ts.param.ar1.ma1[, 1]^2)

plot(sim.ar1.ma1.acf[, 1], t(sqrt(var.ar1.ma1)))

calc_arma11_sd = function(ts.param.ar1.ma1){
  sqrt(0.0001 * (ts.param.ar1.ma1[2]^2 + 2*ts.param.ar1.ma1[1]*ts.param.ar1.ma1[2] + 1) / 
         (1-ts.param.ar1.ma1[1]^2))
}

plot(sim.ar1.ma1.acf[, 1], t(calc_arma11_sd(ts.param.ar1.ma1)))

########################
###### simulation ######
########################

######################## AR1 ########################

sim.risk.ar.1.adjsd = sapply(ts.param.ar.1, 
                       function(x){rep_sim_ts(model=list(ar=x), n=1000, 
                                              rand.gen = function(n) rnorm(n, sd = 0.0001/calc_ar1_sd(x)))})
sim.risk.ar.1.adjsd = data.frame(cbind(t(sim.risk.ar.1.adjsd), ts.param.ar.1))

plot_grid(ggplot(sim.risk.ar.1.adjsd, aes(x = ts.param.ar.1, y = ES)) + geom_point() + theme_bw() + 
            xlab("AR(1) Coefficient"),
          ggplot(sim.risk.ar.1.adjsd, aes(x = ts.param.ar.1, y = VaR)) + geom_point() + theme_bw() + 
            xlab("AR(1) Coefficient"),
          ggplot(sim.risk.ar.1.adjsd, aes(x = ts.param.ar.1, y = sd)) + geom_point() + theme_bw() + 
            xlab("AR(1) Coefficient") + ylab("Volatility"),
          ggplot(sim.risk.ar.1.adjsd, aes(x = ts.param.ar.1, y = CED)) + geom_point() + theme_bw() + 
            xlab("AR(1) Coefficient"),
          ncol = 2, align = 'v')

######################## AR2 ########################

sim.risk.ar.2.adjsd = apply(ts.param.ar2, 1, 
                      function(x){rep_sim_ts(model=list(ar=x), n=1000, 
                                             rand.gen = function(n) rnorm(n, sd = 0.0001/calc_ar2_sd(x)))})
sim.risk.ar.2.adjsd = data.frame(cbind(t(sim.risk.ar.2.adjsd), sim.ar.2.acf))

plot_grid(ggplot(data.frame(cbind(ts.param.ar2, sim.risk.ar.2.adjsd)), 
                 aes(x = ac1, y = ES, group = factor(AR2))) + geom_point(aes(colour = AR2)) + theme_light() +
            xlab(expression(rho[1])) +
            scale_colour_gradient2(high="steelblue", mid="darksalmon") ,
          ggplot(data.frame(cbind(ts.param.ar2, sim.risk.ar.2.adjsd)), 
                 aes(x = ac1, y = VaR, group = factor(AR2))) + geom_point(aes(colour = AR2)) + theme_light() +
            xlab(expression(rho[1])) +
            scale_colour_gradient2(high="steelblue", mid="darksalmon"),
          ggplot(data.frame(cbind(ts.param.ar2, sim.risk.ar.2.adjsd)), 
                 aes(x = ac1, y = sd, group = factor(AR2))) + geom_point(aes(colour = AR2)) + theme_light() +
            xlab(expression(rho[1])) + ylab("Volatility") + 
            scale_colour_gradient2(high="steelblue", mid="darksalmon"),
          ggplot(data.frame(cbind(ts.param.ar2, sim.risk.ar.2.adjsd)), 
                 aes(x = ac1, y = CED, group = factor(AR2))) + geom_line(aes(colour = AR2)) + theme_light() +
            xlab(expression(rho[1])) +
            scale_colour_gradient2(high="steelblue", mid="darksalmon"),
          ncol = 2, align = 'v')

######################## MA1 ########################

sim.risk.ma.1.adjsd = sapply(ts.param.ma.1, 
                       function(x){rep_sim_ts(model=list(ma=x), n=1000, 
                                              rand.gen = function(n) rnorm(n, sd = 0.0001/calc_ma1_sd(x)))})
sim.risk.ma.1.adjsd = data.frame(cbind(t(sim.risk.ma.1.adjsd), ts.param.ma.1))

plot_grid(ggplot(data.frame(cbind(sim.risk.ma.1.adjsd, sim.ma.1.acf)), 
                 aes(x = ac1, y = ES)) + geom_point() + theme_bw() + 
            xlab(expression(rho[1])),
          ggplot(data.frame(cbind(sim.risk.ma.1.adjsd, sim.ma.1.acf)), 
                 aes(x = ac1, y = VaR)) + geom_point() + theme_bw() + 
            xlab(expression(rho[1])),
          ggplot(data.frame(cbind(sim.risk.ma.1.adjsd, sim.ma.1.acf)), 
                 aes(x = ac1, y = sd)) + geom_point() + theme_bw() + 
            xlab(expression(rho[1])),
          ggplot(data.frame(cbind(sim.risk.ma.1.adjsd, sim.ma.1.acf)), 
                 aes(x = ac1, y = CED)) + geom_point() + theme_bw() + 
            xlab(expression(rho[1])),
          ncol = 2, align = 'v')

######################## MA2 ########################

sim.risk.ma.2.adjsd = apply(ts.param.ma2, 1, 
                      function(x){rep_sim_ts(model=list(ma=x), n=1000, 
                                             rand.gen = function(n) rnorm(n, sd = 0.0001/calc_ma2_sd(x)))})
sim.risk.ma.2.adjsd = data.frame(cbind(t(sim.risk.ma.2.adjsd), sim.ma.2.acf))

plot_grid(ggplot(data.frame(cbind(ts.param.ma2, sim.risk.ma.2.adjsd)), 
                 aes(x = ac1, y = ES, group = factor(MA2))) + geom_point(aes(colour = MA2)) + theme_light() +
            xlab(expression(rho[1])) +
            scale_colour_gradient2(high="steelblue", mid="darksalmon", low = "steelblue") ,
          ggplot(data.frame(cbind(ts.param.ma2, sim.risk.ma.2.adjsd)), 
                 aes(x = ac1, y = VaR, group = factor(MA2))) + geom_point(aes(colour = MA2)) + theme_light() +
            xlab(expression(rho[1])) +
            scale_colour_gradient2(high="steelblue", mid="darksalmon", low = "steelblue"),
          ggplot(data.frame(cbind(ts.param.ma2, sim.risk.ma.2.adjsd)), 
                 aes(x = ac1, y = sd, group = factor(MA2))) + geom_point(aes(colour = MA2)) + theme_light() +
            xlab(expression(rho[1])) + ylab("Volatility") + 
            scale_colour_gradient2(high="steelblue", mid="darksalmon", low = "steelblue"),
          ggplot(data.frame(cbind(ts.param.ma2, sim.risk.ma.2.adjsd)), 
                 aes(x = ac1, y = CED, group = factor(MA2))) + geom_line(aes(colour = MA2)) + theme_light() +
            xlab(expression(rho[1])) +
            scale_colour_gradient2(high="steelblue", mid="darksalmon", low = "steelblue"),
          ncol = 2, align = 'v')

######################## AR1MA1 ########################

sim.risk.ar1.ma1.adjsd = apply(ts.param.ar1.ma1, 1, 
                         function(x){rep_sim_ts(model=list(ar = x[1], ma = x[2]), n=1000, 
                                                rand.gen = function(n) rnorm(n, sd = 0.0001/calc_arma11_sd(x)))})
sim.risk.ar1.ma1.adjsd = data.frame(cbind(t(sim.risk.ar1.ma1.adjsd), sim.ar1.ma1.acf))

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








