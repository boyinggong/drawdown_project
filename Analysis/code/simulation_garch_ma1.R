##########################
### MA(1) ~ GARCH(1,1) ###
##########################
ts.param.garch_MA1 = expand.grid(c(-0.25,0.25), 
                                 c(1e-08, 1e-07),seq(from = 0.05, to = 0.2,by = 0.03),seq(0.1,0.9,0.2))
colnames(ts.param.garch_MA1) = c("ma1","omega","alpha","beta")
ts.param.garch_MA1 = ts.param.garch_MA1[ts.param.garch_MA1["alpha"]+ts.param.garch_MA1["beta"]<1,]

# simulation1
sim.risk.garch_MA1 = apply(ts.param.garch_MA1, 1, 
                           function(x){rep_sim_ts_garch(rep = 100, model=list(ma=x["ma1"],omega = x["omega"], alpha = x["alpha"],beta = x["beta"]),
                                                        n=1000, rand.gen = "norm")})

sim.risk.garch_MA1 = as.data.frame(t(sim.risk.garch_MA1))
# sim.risk.garch_MA1 = data.frame(cbind(t(sim.risk.garch_MA1), sim.garch_MA1.acf))


# simulation2
ma1_var_param = expand.grid(seq(0.1,0.8,0.1), 
                            c(1e-07),c(0.05,0.2),c(0.1,0.6))
colnames(ma1_var_param) = c("ma1","omega","alpha","beta")

sim.risk.garch_MA1.ma1 = apply(ma1_var_param, 1, 
                               function(x){rep_sim_ts_garch(rep = 500, model=list(ma=x["ma1"],omega = x["omega"], alpha = x["alpha"],beta = x["beta"]),
                                                            n=1000, rand.gen = "norm")})

sim.risk.garch_MA1.ma1 = as.data.frame(t(sim.risk.garch_MA1.ma1))



#################### The characteristics of the the sample empirical return distribution ###############
return_dist_garch_MA1 = lapply(seq(from = 0.05, to = 0.2,by = 0.03),function(s){
  spec = garchSpec(model = list(ma=0.2,omega = 1e-07, alpha = s,beta = 0.5), 
                   cond.dist = "norm")
  garch.sim = garchSim(spec, n = 1000)
}) 
return_dist_stat_garch_MA1 = lapply(return_dist_garch_MA1, function(x){c(mean(x), sd(x), skewness(x), kurtosis(x))})
format(round(return_dist_stat[[1]], 3), nsmall = 3)
format(round(return_dist_stat[[2]], 3), nsmall = 3)
format(round(return_dist_stat[[3]], 3), nsmall = 3)
format(round(return_dist_stat[[4]], 3), nsmall = 3)
format(round(return_dist_stat[[5]], 3), nsmall = 3)
format(round(return_dist_stat[[6]], 3), nsmall = 3)

##################################
## Plots: 1) vs alpha or 2) ma1 ##
##################################

### alpha ###
# postive/negative ma, they have a similar effect.
# with alpha:
png("Analysis/figures/simulation_garch/garch_MA1_risk_measures_neg_alpha.png", width = 800, height = 600)
subset = (ts.param.garch_MA1["ma1"]  == -0.25 & ts.param.garch_MA1["beta"]< 0.9)
# & ts.param.garch_MA1["omega"]==1e-07)
plot_grid(ggplot(data.frame(cbind(ts.param.garch_MA1[subset, ], sim.risk.garch_MA1[subset, ])), 
                 aes(x = alpha, y = ES, group = interaction(beta,omega))) + 
            geom_line(aes(colour = beta)) +
            geom_point(aes(shape = factor(omega)))+
            theme_light() +
            xlab(expression(alpha)) +
            scale_colour_gradient2(high="steelblue", mid="darksalmon") ,
          
          ggplot(data.frame(cbind(ts.param.garch_MA1[subset, ], sim.risk.garch_MA1[subset, ])), 
                 aes(x = alpha, y = VaR, group = interaction(beta,omega))) + 
            geom_line(aes(colour = beta)) +
            geom_point(aes(shape = factor(omega)))+
            theme_light() +
            xlab(expression(alpha)) +
            scale_colour_gradient2(high="steelblue", mid="darksalmon") ,
          
          ggplot(data.frame(cbind(ts.param.garch_MA1[subset, ], sim.risk.garch_MA1[subset, ])), 
                 aes(x = alpha, y = sd, group = interaction(beta,omega))) + 
            geom_line(aes(colour = beta)) +
            geom_point(aes(shape = factor(omega)))+
            theme_light() +
            xlab(expression(alpha)) +
            scale_colour_gradient2(high="steelblue", mid="darksalmon") ,
          
          ggplot(data.frame(cbind(ts.param.garch_MA1[subset, ], sim.risk.garch_MA1[subset, ])), 
                 aes(x = alpha, y = CED, group = interaction(beta,omega))) + 
            geom_line(aes(colour = beta)) +
            geom_point(aes(shape = factor(omega)))+
            theme_light() +
            xlab(expression(alpha)) +
            scale_colour_gradient2(high="steelblue", mid="darksalmon") ,      
          ncol = 2, align = 'v')
dev.off()


################################# vs MA coefficients ##########################################
# sim.risk.garch_MA1.ma1 ma1_var_param

png("Analysis/figures/simulation_garch/garch_MA1_risk_measures_ma1.png", width = 800, height = 600)
# subset = (ts.param.garch_MA1["ma1"]  == -0.25 & ts.param.garch_MA1["beta"]< 0.9)
# & ts.param.garch_MA1["omega"]==1e-07)
ma1_var_param = transform(ma1_var_param, alpha = as.factor(alpha),beta = as.factor(beta))

plot_grid(ggplot(data.frame(cbind(ma1_var_param, sim.risk.garch_MA1.ma1)), 
                 aes(x = ma1, y = ES, group = interaction(beta,alpha))) + 
            geom_line(aes(colour = beta)) +
            geom_point(aes(shape = alpha))+
            theme_light() +
            xlab(expression(theta)) ,
          
          ggplot(data.frame(cbind(ma1_var_param, sim.risk.garch_MA1.ma1)), 
                 aes(x = ma1, y = VaR, group = interaction(beta,alpha))) + 
            geom_line(aes(colour = beta)) +
            geom_point(aes(shape = alpha))+
            theme_light() +
            xlab(expression(theta)) ,
          
          ggplot(data.frame(cbind(ma1_var_param, sim.risk.garch_MA1.ma1)), 
                 aes(x = ma1, y = sd, group = interaction(beta,alpha))) + 
            geom_line(aes(colour = beta)) +
            geom_point(aes(shape = alpha))+
            theme_light() +
            xlab(expression(theta)) ,
          
          ggplot(data.frame(cbind(ma1_var_param, sim.risk.garch_MA1.ma1)), 
                 aes(x = ma1, y = CED, group = interaction(beta,alpha))) + 
            geom_line(aes(colour = beta)) +
            geom_point(aes(shape = alpha))+
            theme_light() +
            xlab(expression(theta)) ,     
          ncol = 2, align = 'v')
dev.off()


#################################### Plot distribution ##########################################

### Max drawdown Distr with alpha and beta ###
# just see ma1 = 0.25
dd_dist_garch_MA1 <- expand.grid(c(-0.25,0.25), 
                                 c(1e-07),seq(from = 0.05, to = 0.2,by = 0.05),seq(0.1,0.7,0.3))
colnames(dd_dist_garch_MA1) = c("ma1","omega","alpha","beta")
dd_dist_garch_MA1 = split(dd_dist_garch_MA1, seq(nrow(dd_dist_garch_MA1)))
dd_dist_garch_MA1 = lapply(dd_dist_garch_MA1,unlist)
# subset = dd_dist_garch_MA1["ma1"]==0.25

sim.risk.garch_MA1.df = lapply(dd_dist_garch_MA1,
                               function(x){rep_sim_ts_garch(rep = 500, model=list(ma=x["ma1"],omega = x["omega"], alpha = x["alpha"],beta = x["beta"]),
                                                            n=1000, rand.gen = "norm", return_df = TRUE)})


png("Analysis/figures/simulation_garch/garch_MA1_alpha_beta.png", width = 800, height = 400)
sim.risk.garch_MA1.densityList <- list()
for (i in seq(2, 24, 2)){
  temp = dd_dist_garch_MA1[[toString(i)]]
  sim.risk.garch_MA1.densityList[[toString(i)]] = 
    ggplot(df <- data.frame(maxDrawdown = sim.risk.garch_MA1.df[[i]]['maxDrawdown',]), 
           aes(x = maxDrawdown)) + geom_density(fill = 'snow3', colour = 'snow3') + theme_bw() + 
    xlab(paste("alpha = ",temp["alpha"], ";beta = ",temp["beta"] )) +
    xlim(0, 0.16) + ylim(0, 65)
}
do.call("plot_grid", c(sim.risk.garch_MA1.densityList,
                       ncol = 4, align = 'v'))
dev.off()

png("Analysis/figures/simulation_garch/garch_MA1_alpha_beta_neg.png", width = 800, height = 400)
sim.risk.garch_MA1.densityList <- list()
for (i in seq(1, 23, 2)){
  temp = dd_dist_garch_MA1[[toString(i)]]
  sim.risk.garch_MA1.densityList[[toString(i)]] = 
    ggplot(df <- data.frame(maxDrawdown = sim.risk.garch_MA1.df[[i]]['maxDrawdown',]), 
           aes(x = maxDrawdown)) + geom_density(fill = 'snow3', colour = 'snow3') + theme_bw() + 
    xlab(paste("alpha = ",temp["alpha"], ";beta = ",temp["beta"] ))+
    xlim(0, 0.08) + ylim(0, 120)
}
do.call("plot_grid", c(sim.risk.garch_MA1.densityList,
                       ncol = 4))
dev.off()
