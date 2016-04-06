
library(ggplot2)
library(cowplot)
library(xts)

source("risk_diagostics_function.R")

######################
######################

sd.ar.1 = seq(from = 0.001, to = 0.02, by = 0.001)
sim.risk.ar.1.sd = sapply(sd.ar.1, 
                       function(x){rep_sim_ts(model=list(ar=0.2), n=63, 
                                              rand.gen = function(n) rnorm(n, sd = x))})
sim.risk.ar.1.sd = data.frame(cbind(t(sim.risk.ar.1.sd), sd.ar.1))

png("../figures/simulation/AR1_risk_measures_change_sd.png", width = 800, height = 400)
plot_grid(ggplot(sim.risk.ar.1.sd, aes(x = sd.ar.1, y = ES)) + geom_point() + theme_bw() + 
            xlab("Standard deviation of the noice term"),
          ggplot(sim.risk.ar.1.sd, aes(x = sd.ar.1, y = VaR)) + geom_point() + theme_bw() + 
            xlab("Standard deviation of the noice term"),
          ggplot(sim.risk.ar.1.sd, aes(x = sd.ar.1, y = sd)) + geom_point() + theme_bw() + 
            xlab("Standard deviation of the noice term") + ylab("Volatility"),
          ggplot(sim.risk.ar.1.sd, aes(x = sd.ar.1, y = CED)) + geom_point() + theme_bw() + 
            xlab("Standard deviation of the noice term"),
          ncol = 2, align = 'v')
dev.off()