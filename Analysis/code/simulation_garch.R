library(ggplot2)
library(cowplot)
library(xts)
library(fGarch)
source("Analysis/code/risk_diagostics_function.R")
# Notice in the empirical study, the GARCH(1,1) is always enough for 
# Simulation in GARCH
sim_ts_garch <- function(p = 0.95, model, n, rand.gen){
# There are 5 groups of parameters in the model. they are arma para:ar, ma
# and variance para, which are alpha, beta and omega
  spec = garchSpec(model = model, cond.dist = rand.gen)
  garch.sim = garchSim(spec, n = n)
  res = c(ES(garch.sim, p = p),
          VaR(garch.sim, p = p),
          sd(garch.sim),
          maxDrawdown(garch.sim))
  names(res) = c("ES", "VaR", "volatility", "maxDrawdown")
  return(res)
}

rep_sim_ts_garch <- function(rep = 500, p = 0.95, model, n, rand.gen, return_df = FALSE){
  print(unlist(model))
  df = replicate(rep, sim_ts_garch(p = 0.95, model = model, n = n, rand.gen = rand.gen))
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
# rep_sim_ts_garch(rep = 100, p = 0.95, model =list(ar = 0.5, ma = c(0.3, -0.3),alpha = 0.2, beta = 0.7) 
#                  , n = 100, rand.gen = "norm", return_df = FALSE)

##########################
### AR(1) ~ GARCH(1,1) ###
##########################
ts.param.garch_AR1 = expand.grid(c(-0.25,0.25), 
                           c(1e-08, 1e-07),seq(from = 0.05, to = 0.2,by = 0.03),seq(0.1,0.9,0.2))
colnames(ts.param.garch_AR1) = c("ar1","omega","alpha","beta")
ts.param.garch_AR1 = ts.param.garch_AR1[ts.param.garch_AR1["alpha"]+ts.param.garch_AR1["beta"]<1,]

# compute the acf from true model
sim.garch_AR1.acf = t(apply(ts.param.garch_AR1, 1,
                       function(x) ARMAacf(ar = x["ar1"], lag.max = 3)[2:4]))
colnames(sim.garch_AR1.acf) = c("ac1",'ac2',"ac3")

# simulation1
sim.risk.garch_AR1 = apply(ts.param.garch_AR1, 1, 
                      function(x){rep_sim_ts_garch(model=list(ar=x["ar1"],omega = x["omega"], alpha = x["alpha"],beta = x["beta"]),
                                             n=1000, rand.gen = "norm")})

# sim.risk.garch_AR1 = data.frame(cbind(t(sim.risk.garch_AR1), sim.garch_AR1.acf))



# simulation2
ar1_var_param = expand.grid(seq(0.1,0.8,0.1), 
                            c(1e-07),c(0.05,0.2),c(0.1,0.6))
colnames(ar1_var_param) = c("ar1","omega","alpha","beta")

sim.risk.garch_AR1.ar1 = apply(ar1_var_param, 1, 
                           function(x){rep_sim_ts_garch(rep = 100, model=list(ar=x["ar1"],omega = x["omega"], alpha = x["alpha"],beta = x["beta"]),
                                                        n=1000, rand.gen = "norm")})

sim.risk.garch_AR1.ar1 = as.data.frame(t(sim.risk.garch_AR1.ar1))
# sim.risk.garch_AR1 = data.frame(cbind(t(sim.risk.garch_AR1), sim.garch_AR1.acf))
# plot_df = data.frame(cbind(ts.param.garch_AR1, sim.risk.garch_AR1))
# plot_df[abs(plot_df$Var2 - 0.1) < 1e-3, ]



#################### The characteristics of the the sample empirical return distribution ###############
return_dist_garch_AR1 = lapply(seq(from = 0.05, to = 0.2,by = 0.03),function(s){
  spec = garchSpec(model = list(ar=0.2,omega = 1e-07, alpha = s,beta = 0.5), 
                   cond.dist = "norm")
  garch.sim = garchSim(spec, n = 1000)
}) 
return_dist_stat_garch_AR1 = lapply(return_dist_garch_AR1, function(x){c(mean(x), sd(x), skewness(x), kurtosis(x))})

length(return_dist_garch_AR1[[1]])

format(round(return_dist_stat[[1]], 3), nsmall = 3)
format(round(return_dist_stat[[2]], 3), nsmall = 3)
format(round(return_dist_stat[[3]], 3), nsmall = 3)
format(round(return_dist_stat[[4]], 3), nsmall = 3)
format(round(return_dist_stat[[5]], 3), nsmall = 3)
format(round(return_dist_stat[[6]], 3), nsmall = 3)

##################################
## Plots: 1) vs alpha or 2) ar1 ##
##################################

### alpha ###
# postive/negative ar, they have a similar effect.
# with alpha:
png("Analysis/figures/simulation_garch/garch_AR1_risk_measures_neg_alpha.png", width = 800, height = 600)
subset = (ts.param.garch_AR1["ar1"]  == -0.25 & ts.param.garch_AR1["beta"]< 0.9)
# & ts.param.garch_AR1["omega"]==1e-07)
plot_grid(ggplot(data.frame(cbind(ts.param.garch_AR1[subset, ], sim.risk.garch_AR1[subset, ])), 
                 aes(x = alpha, y = ES, group = interaction(beta,omega))) + 
            geom_line(aes(colour = beta)) +
            geom_point(aes(shape = factor(omega)))+
            theme_light() +
            xlab(expression(alpha)) +
            scale_colour_gradient2(high="steelblue", mid="darksalmon") ,
          
          ggplot(data.frame(cbind(ts.param.garch_AR1[subset, ], sim.risk.garch_AR1[subset, ])), 
                 aes(x = alpha, y = VaR, group = interaction(beta,omega))) + 
            geom_line(aes(colour = beta)) +
            geom_point(aes(shape = factor(omega)))+
            theme_light() +
            xlab(expression(alpha)) +
            scale_colour_gradient2(high="steelblue", mid="darksalmon") ,
          
          ggplot(data.frame(cbind(ts.param.garch_AR1[subset, ], sim.risk.garch_AR1[subset, ])), 
                 aes(x = alpha, y = sd, group = interaction(beta,omega))) + 
            geom_line(aes(colour = beta)) +
            geom_point(aes(shape = factor(omega)))+
            theme_light() +
            xlab(expression(alpha)) +
            scale_colour_gradient2(high="steelblue", mid="darksalmon") ,
          
          ggplot(data.frame(cbind(ts.param.garch_AR1[subset, ], sim.risk.garch_AR1[subset, ])), 
                 aes(x = alpha, y = CED, group = interaction(beta,omega))) + 
            geom_line(aes(colour = beta)) +
            geom_point(aes(shape = factor(omega)))+
            theme_light() +
            xlab(expression(alpha)) +
            scale_colour_gradient2(high="steelblue", mid="darksalmon") ,      
          ncol = 2, align = 'v')
dev.off()


################################# vs AR coefficients ##########################################
# sim.risk.garch_AR1.ar1 ar1_var_param

png("Analysis/figures/simulation_garch/garch_AR1_risk_measures_ar1.png", width = 800, height = 600)
# subset = (ts.param.garch_AR1["ar1"]  == -0.25 & ts.param.garch_AR1["beta"]< 0.9)
# & ts.param.garch_AR1["omega"]==1e-07)
ar1_var_param = transform(ar1_var_param, alpha = as.factor(alpha),beta = as.factor(beta))

plot_grid(ggplot(data.frame(cbind(ar1_var_param, sim.risk.garch_AR1.ar1)), 
                 aes(x = ar1, y = ES, group = interaction(beta,alpha))) + 
            geom_line(aes(colour = beta)) +
            geom_point(aes(shape = alpha))+
            theme_light() +
            xlab(expression(kappa)) ,
          
          ggplot(data.frame(cbind(ar1_var_param, sim.risk.garch_AR1.ar1)), 
                 aes(x = ar1, y = VaR, group = interaction(beta,alpha))) + 
            geom_line(aes(colour = beta)) +
            geom_point(aes(shape = alpha))+
            theme_light() +
            xlab(expression(kappa)) ,
          
          ggplot(data.frame(cbind(ar1_var_param, sim.risk.garch_AR1.ar1)), 
                 aes(x = ar1, y = sd, group = interaction(beta,alpha))) + 
            geom_line(aes(colour = beta)) +
            geom_point(aes(shape = alpha))+
            theme_light() +
            xlab(expression(kappa)) ,
          
          ggplot(data.frame(cbind(ar1_var_param, sim.risk.garch_AR1.ar1)), 
                 aes(x = ar1, y = CED, group = interaction(beta,alpha))) + 
            geom_line(aes(colour = beta)) +
            geom_point(aes(shape = alpha))+
            theme_light() +
            xlab(expression(kappa)) ,     
          ncol = 2, align = 'v')
dev.off()

############################### Max drawdown Distr with alpha and beta ###############################
# just see ar1 = 0.25
dd_dist_garch_AR1 <- expand.grid(c(-0.25,0.25), 
                                 c(1e-07),seq(from = 0.05, to = 0.2,by = 0.05),seq(0.1,0.7,0.3))
colnames(dd_dist_garch_AR1) = c("ar1","omega","alpha","beta")
dd_dist_garch_AR1 = split(dd_dist_garch_AR1, seq(nrow(dd_dist_garch_AR1)))
dd_dist_garch_AR1 = lapply(dd_dist_garch_AR1,unlist)
# subset = dd_dist_garch_AR1["ar1"]==0.25

sim.risk.garch_AR1.df = lapply(dd_dist_garch_AR1,
                               function(x){rep_sim_ts_garch(rep = 500, model=list(ar=x["ar1"],omega = x["omega"], alpha = x["alpha"],beta = x["beta"]),
                                                            n=1000, rand.gen = "norm", return_df = TRUE)})

png("Analysis/figures/simulation_garch/garch_AR1_alpha_beta.png", width = 800, height = 400)
sim.risk.garch_AR1.densityList <- list()
for (i in seq(2, 24, 2)){
  temp = dd_dist_garch_AR1[[toString(i)]]
  sim.risk.garch_AR1.densityList[[toString(i)]] = 
    ggplot(df <- data.frame(maxDrawdown = sim.risk.garch_AR1.df[[i]]['maxDrawdown',]), 
           aes(x = maxDrawdown)) + geom_density(fill = 'snow3', colour = 'snow3') + theme_bw() + 
    xlab(paste("alpha = ",temp["alpha"], ";beta = ",temp["beta"] ))+
    xlim(0, 0.15) + ylim(0, 70) 
}
do.call("plot_grid", c(sim.risk.garch_AR1.densityList,
                       ncol = 4, align = 'v'))
dev.off()


png("Analysis/figures/simulation_garch/garch_AR1_alpha_beta_neg.png", width = 800, height = 400)
sim.risk.garch_AR1.densityList <- list()
for (i in seq(1, 23, 2)){
  temp = dd_dist_garch_AR1[[toString(i)]]
  sim.risk.garch_AR1.densityList[[toString(i)]] = 
    ggplot(df <- data.frame(maxDrawdown = sim.risk.garch_AR1.df[[i]]['maxDrawdown',]), 
           aes(x = maxDrawdown)) + geom_density(fill = 'snow3', colour = 'snow3') + theme_bw() + 
    xlab(paste("alpha = ",temp["alpha"], ";beta = ",temp["beta"] ))+
    xlim(0, 0.08) + ylim(0, 120)
}
do.call("plot_grid", c(sim.risk.garch_AR1.densityList,
                       ncol = 4))
dev.off()
















# png("Analysis/figures/simulation/garch_AR1_risk_measures_pos_alpha.png", width = 800, height = 400)
# subset = (ts.param.garch_AR1["ar1"]  == 0.25 & ts.param.garch_AR1["beta"]< 0.7)
#          # & ts.param.garch_AR1["omega"]==1e-07)
# plot_grid(ggplot(data.frame(cbind(ts.param.garch_AR1[subset, ], sim.risk.garch_AR1[subset, ])), 
#                  aes(x = ac1, y = ES, group = factor(alpha))) + geom_point(aes(colour = alpha)) + theme_light() +
#             xlab(expression(rho[1])) +
#             scale_colour_gradient2(high="steelblue", mid="darksalmon") ,
#           
#           ggplot(data.frame(cbind(ts.param.garch_AR1[subset, ], sim.risk.garch_AR1[subset, ])), 
#                  aes(x = ac1, y = VaR, group = factor(alpha))) + geom_point(aes(colour = alpha)) + theme_light() +
#             xlab(expression(rho[1])) +
#             scale_colour_gradient2(high="steelblue", mid="darksalmon"),
#           
#           ggplot(data.frame(cbind(ts.param.garch_AR1[subset, ], sim.risk.garch_AR1[subset, ])), 
#                  aes(x = ac1, y = sd, group = factor(alpha))) + geom_point(aes(colour = alpha)) + theme_light() +
#             xlab(expression(rho[1])) + ylab("Volatility") + 
#             scale_colour_gradient2(high="steelblue", mid="darksalmon"),
#           
#           ggplot(data.frame(cbind(ts.param.garch_AR1[subset, ], sim.risk.garch_AR1[subset, ])), 
#                  aes(x = ac1, y = CED, group = factor(alpha))) + geom_line(aes(colour = alpha)) + theme_light() +
#             xlab(expression(rho[1])) +
#             scale_colour_gradient2(high="steelblue", mid="darksalmon"),
#           
#           ncol = 2, align = 'v')
# dev.off()

# negative ar
png("Analysis/figures/simulation/garch_AR1_risk_measures_neg_alpha.png", width = 800, height = 400)
subset = (ts.param.garch_AR1["ar1"]  == -0.25 & ts.param.garch_AR1["beta"]< 0.7)
# & ts.param.garch_AR1["omega"]==1e-07)
plot_grid(ggplot(data.frame(cbind(ts.param.garch_AR1[subset, ], sim.risk.garch_AR1[subset, ])), 
                 aes(x = ac1, y = ES, group = factor(alpha))) + geom_point(aes(colour = alpha)) + theme_light() +
            xlab(expression(rho[1])) +
            scale_colour_gradient2(high="steelblue", mid="darksalmon") ,
          
          ggplot(data.frame(cbind(ts.param.garch_AR1[subset, ], sim.risk.garch_AR1[subset, ])), 
                 aes(x = ac1, y = VaR, group = factor(alpha))) + geom_point(aes(colour = alpha)) + theme_light() +
            xlab(expression(rho[1])) +
            scale_colour_gradient2(high="steelblue", mid="darksalmon"),
          
          ggplot(data.frame(cbind(ts.param.garch_AR1[subset, ], sim.risk.garch_AR1[subset, ])), 
                 aes(x = ac1, y = sd, group = factor(alpha))) + geom_point(aes(colour = alpha)) + theme_light() +
            xlab(expression(rho[1])) + ylab("Volatility") + 
            scale_colour_gradient2(high="steelblue", mid="darksalmon"),
          
          ggplot(data.frame(cbind(ts.param.garch_AR1[subset, ], sim.risk.garch_AR1[subset, ])), 
                 aes(x = ac1, y = CED, group = factor(alpha))) + geom_line(aes(colour = alpha)) + theme_light() +
            xlab(expression(rho[1])) +
            scale_colour_gradient2(high="steelblue", mid="darksalmon"),
          
          ncol = 2, align = 'v')
dev.off()

# vs kappa
png("Analysis/figures/simulation/garch_AR1_risk_measures_pos_alpha_kappa.png", width = 800, height = 400)
subset = ( ts.param.garch_AR1["beta"]< 0.7)
# & ts.param.garch_AR1["omega"]==1e-07)
plot_grid(ggplot(data.frame(cbind(ts.param.garch_AR1[subset, ], sim.risk.garch_AR1[subset, ])), 
                 aes(x = alpha, y = ES, group = factor(ar1))) + geom_point(aes(colour = ar1,shape = factor(beta))) + theme_light() +
            xlab(expression(alpha)) +
            scale_colour_gradient2(high="steelblue", mid="darksalmon") ,
          
          ggplot(data.frame(cbind(ts.param.garch_AR1[subset, ], sim.risk.garch_AR1[subset, ])), 
                 aes(x = alpha, y = VaR, group = factor(ar1))) + geom_point(aes(colour = ar1,shape = factor(beta))) + theme_light() +
            xlab(expression(alpha)) +
            scale_colour_gradient2(high="steelblue", mid="darksalmon"),
          
          ggplot(data.frame(cbind(ts.param.garch_AR1[subset, ], sim.risk.garch_AR1[subset, ])), 
                 aes(x = alpha, y = sd, group = factor(ar1))) + geom_point(aes(colour = ar1,shape = factor(beta))) + theme_light() +
            xlab(expression(alpha)) + ylab("Volatility") + 
            scale_colour_gradient2(high="steelblue", mid="darksalmon"),
          
          ggplot(data.frame(cbind(ts.param.garch_AR1[subset, ], sim.risk.garch_AR1[subset, ])), 
                 aes(x = alpha, y = CED, group = factor(ar1))) + 
            geom_line(aes(colour = ar1)) + theme_light() +
            geom_point(aes(shape = factor(beta)))+
            xlab(expression(alpha)) +
            scale_colour_gradient2(high="steelblue", mid="darksalmon"),     
          ncol = 2, align = 'v')
dev.off()


#alpha: seq(from = 0.05, to = 0.2,by = 0.05)
# temp1 <- function(s){
#   spec = garchSpec(model = list(ar=0.2,omega = 1e-07, alpha = s,beta = 0.5), 
#                    cond.dist = "norm")
#   garch.sim = garchSim(spec, n = n)
# }










##########################
### MA(1) ~ GARCH(1,1) ###
##########################

ts.param.garch_MA1 = expand.grid(c(-0.25,0.25), 
                                 c(1e-08, 1e-07),seq(from = 0.05, to = 0.2,by = 0.03),seq(0.1,0.9,0.2))
colnames(ts.param.garch_MA1) = c("ma1","omega","alpha","beta")
ts.param.garch_MA1 = ts.param.garch_MA1[ts.param.garch_MA1["alpha"]+ts.param.garch_MA1["beta"]<1,]

# compute the acf from true model
sim.garch_MA1.acf = t(apply(ts.param.garch_MA1, 1,
                            function(x) ARMAacf(ma = x["ma1"], lag.max = 3)[2:4]))

colnames(sim.garch_MA1.acf) = c("ac1",'ac2',"ac3")

# simulation
sim.risk.garch_MA1 = apply(ts.param.garch_MA1, 1, 
                           function(x){rep_sim_ts_garch(model=list(ar=x["ma1"],omega = x["omega"], alpha = x["alpha"],beta = x["beta"]),
                                                        n=1000, rand.gen = "norm")})

#sim.risk.garch_MA1 = sim.risk.garch_MA1[,1:4]
sim.risk.garch_MA1 = data.frame(cbind(t(sim.risk.garch_MA1), sim.garch_MA1.acf))

# plot_df = data.frame(cbind(ts.param.garch_MA1, sim.risk.garch_MA1))
# plot_df[abs(plot_df$Var2 - 0.1) < 1e-3, ]


############################
### ARMA(1) ~ GARCH(1,1) ###
############################
ts.param.garch_ARMA11 = expand.grid(c(-0.25,0.25),c(-0.25,0.25), 
                                 c(1e-08, 1e-07),seq(from = 0.05, to = 0.2,by = 0.03),seq(0.1,0.9,0.2))
colnames(ts.param.garch_ARMA11) = c("ar1","ma1","omega","alpha","beta")
ts.param.garch_ARMA11 = ts.param.garch_ARMA11[ts.param.garch_ARMA11["alpha"]+ts.param.garch_ARMA11["beta"]<1,]

# compute the acf from true model
sim.garch_ARMA11.acf = t(apply(ts.param.garch_ARMA11, 1,
                            function(x) ARMAacf(ar = ???,ma = x, lag.max = 3)[2:4]))
colnames(sim.garch_MA1.acf) = c("ac1",'ac2',"ac3")

# simulation
sim.risk.garch_MA1 = apply(ts.param.garch_MA1, 1, 
                           function(x){rep_sim_ts_garch(model=list(ar=x["ma1"],omega = x["omega"], alpha = x["alpha"],beta = x["beta"]),
                                                        n=1000, rand.gen = "norm")})


sim.risk.garch_MA1 = data.frame(cbind(t(sim.risk.garch_MA1), sim.garch_MA1.acf))







#########################
## return distribution ##
#########################
# take look at alpha
return_dist_garch_AR1 = lapply(seq(from = 0.05, to = 0.2,by = 0.03),function(s){
  spec = garchSpec(model = list(ar=0.2,omega = 1e-07, alpha = s,beta = 0.5), 
                   cond.dist = "norm")
  garch.sim = garchSim(spec, n = 1000)
}) 
return_dist_stat_garch_AR1 = lapply(return_dist_garch_AR1, function(x){c(mean(x), sd(x), skewness(x), kurtosis(x))})
format(round(return_dist_stat[[1]], 3), nsmall = 3)
format(round(return_dist_stat[[2]], 3), nsmall = 3)
format(round(return_dist_stat[[3]], 3), nsmall = 3)
format(round(return_dist_stat[[4]], 3), nsmall = 3)
format(round(return_dist_stat[[5]], 3), nsmall = 3)
format(round(return_dist_stat[[6]], 3), nsmall = 3)


### alpha ###
# postive ar
png("Analysis/figures/simulation/garch_AR1_risk_measures_pos_alpha.png", width = 800, height = 400)
subset = (ts.param.garch_AR1["ar1"]  == 0.25 & ts.param.garch_AR1["beta"]< 0.7)
# & ts.param.garch_AR1["omega"]==1e-07)
plot_grid(ggplot(data.frame(cbind(ts.param.garch_AR1[subset, ], sim.risk.garch_AR1[subset, ])), 
                 aes(x = ac1, y = ES, group = factor(alpha))) + geom_point(aes(colour = alpha)) + theme_light() +
            xlab(expression(rho[1])) +
            scale_colour_gradient2(high="steelblue", mid="darksalmon") ,
          
          ggplot(data.frame(cbind(ts.param.garch_AR1[subset, ], sim.risk.garch_AR1[subset, ])), 
                 aes(x = ac1, y = VaR, group = factor(alpha))) + geom_point(aes(colour = alpha)) + theme_light() +
            xlab(expression(rho[1])) +
            scale_colour_gradient2(high="steelblue", mid="darksalmon"),
          
          ggplot(data.frame(cbind(ts.param.garch_AR1[subset, ], sim.risk.garch_AR1[subset, ])), 
                 aes(x = ac1, y = sd, group = factor(alpha))) + geom_point(aes(colour = alpha)) + theme_light() +
            xlab(expression(rho[1])) + ylab("Volatility") + 
            scale_colour_gradient2(high="steelblue", mid="darksalmon"),
          
          ggplot(data.frame(cbind(ts.param.garch_AR1[subset, ], sim.risk.garch_AR1[subset, ])), 
                 aes(x = ac1, y = CED, group = factor(alpha))) + geom_line(aes(colour = alpha)) + theme_light() +
            xlab(expression(rho[1])) +
            scale_colour_gradient2(high="steelblue", mid="darksalmon"),
          
          ncol = 2, align = 'v')
dev.off()

# negative ar
png("Analysis/figures/simulation/garch_AR1_risk_measures_neg_alpha.png", width = 800, height = 400)
subset = (ts.param.garch_AR1["ar1"]  == -0.25 & ts.param.garch_AR1["beta"]< 0.7)
# & ts.param.garch_AR1["omega"]==1e-07)
plot_grid(ggplot(data.frame(cbind(ts.param.garch_AR1[subset, ], sim.risk.garch_AR1[subset, ])), 
                 aes(x = ac1, y = ES, group = factor(alpha))) + geom_point(aes(colour = alpha)) + theme_light() +
            xlab(expression(rho[1])) +
            scale_colour_gradient2(high="steelblue", mid="darksalmon") ,
          
          ggplot(data.frame(cbind(ts.param.garch_AR1[subset, ], sim.risk.garch_AR1[subset, ])), 
                 aes(x = ac1, y = VaR, group = factor(alpha))) + geom_point(aes(colour = alpha)) + theme_light() +
            xlab(expression(rho[1])) +
            scale_colour_gradient2(high="steelblue", mid="darksalmon"),
          
          ggplot(data.frame(cbind(ts.param.garch_AR1[subset, ], sim.risk.garch_AR1[subset, ])), 
                 aes(x = ac1, y = sd, group = factor(alpha))) + geom_point(aes(colour = alpha)) + theme_light() +
            xlab(expression(rho[1])) + ylab("Volatility") + 
            scale_colour_gradient2(high="steelblue", mid="darksalmon"),
          
          ggplot(data.frame(cbind(ts.param.garch_AR1[subset, ], sim.risk.garch_AR1[subset, ])), 
                 aes(x = ac1, y = CED, group = factor(alpha))) + geom_line(aes(colour = alpha)) + theme_light() +
            xlab(expression(rho[1])) +
            scale_colour_gradient2(high="steelblue", mid="darksalmon"),
          
          ncol = 2, align = 'v')
dev.off()

# vs kappa
png("Analysis/figures/simulation/garch_AR1_risk_measures_pos_alpha_kappa.png", width = 800, height = 400)
subset = ( ts.param.garch_AR1["beta"]< 0.7)
# & ts.param.garch_AR1["omega"]==1e-07)
plot_grid(ggplot(data.frame(cbind(ts.param.garch_AR1[subset, ], sim.risk.garch_AR1[subset, ])), 
                 aes(x = alpha, y = ES, group = factor(ar1))) + geom_point(aes(colour = ar1,shape = factor(beta))) + theme_light() +
            xlab(expression(alpha)) +
            scale_colour_gradient2(high="steelblue", mid="darksalmon") ,
          
          ggplot(data.frame(cbind(ts.param.garch_AR1[subset, ], sim.risk.garch_AR1[subset, ])), 
                 aes(x = alpha, y = VaR, group = factor(ar1))) + geom_point(aes(colour = ar1,shape = factor(beta))) + theme_light() +
            xlab(expression(alpha)) +
            scale_colour_gradient2(high="steelblue", mid="darksalmon"),
          
          ggplot(data.frame(cbind(ts.param.garch_AR1[subset, ], sim.risk.garch_AR1[subset, ])), 
                 aes(x = alpha, y = sd, group = factor(ar1))) + geom_point(aes(colour = ar1,shape = factor(beta))) + theme_light() +
            xlab(expression(alpha)) + ylab("Volatility") + 
            scale_colour_gradient2(high="steelblue", mid="darksalmon"),
          
          ggplot(data.frame(cbind(ts.param.garch_AR1[subset, ], sim.risk.garch_AR1[subset, ])), 
                 aes(x = alpha, y = CED, group = factor(ar1))) + 
            geom_line(aes(colour = ar1)) + theme_light() +
            geom_point(aes(shape = factor(beta)))+
            xlab(expression(alpha)) +
            scale_colour_gradient2(high="steelblue", mid="darksalmon"),     
          ncol = 2, align = 'v')
dev.off()


#alpha: seq(from = 0.05, to = 0.2,by = 0.05)
# temp1 <- function(s){
#   spec = garchSpec(model = list(ar=0.2,omega = 1e-07, alpha = s,beta = 0.5), 
#                    cond.dist = "norm")
#   garch.sim = garchSim(spec, n = n)
# }


















# temp <- function(x){
#   return(rep_sim_ts_garch(model=list(ar=x["ar1"],omega = x["omega"], alpha = x["alpha"],beta = x["beta"]),
#                              n=1000, rand.gen = "norm", return_df = TRUE))
# }
# temp1 = temp(unlist(dd_dist_garch_AR1[1,]))





sim.risk.garch_AR1 = apply(ts.param.garch_AR1, 1, 
                           function(x){rep_sim_ts_garch(model=list(ar=x["ar1"],omega = x["omega"], alpha = x["alpha"],beta = x["beta"]),
                                                        n=1000, rand.gen = "norm")})




##
# omega is 10e-08 to 10e-07
# alpha is range from 0.05 to 0.2
# from our empirical study, the beta is ranging from 0.1-1

# ARMA(1,2)-GARCH(1,1) - use default garch values
spec = garchSpec(model = list(ar = 0.5, ma = c(0.3, -0.3),alpha = 0.2, beta = 0.7))

# GARCH(1,1) - specify omega/alpha/beta
spec = garchSpec(model = list(omega = 1e-6, alpha = 0.1, beta = 0.8))
garchSim(spec, n = 10)

# GARCH(1,1) - use default omega and specify alpha/beta
spec = garchSpec(model = list(alpha = 0.2, beta = 0.7))
garchSim(spec, n = 10)

spec = garchSpec(model = list(mu = 0.001, ar = c(0.5,0,0,0,0.1)))
garchSim(spec, n = 10)

spec = garchSpec(model = list())
