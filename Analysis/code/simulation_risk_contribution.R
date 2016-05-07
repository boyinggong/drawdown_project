library(gridExtra)
library(grid)
library(PerformanceAnalytics)
library(reshape)
library(PortRisk)
library(cowplot)
library(ggplot2)
library(xts)


#### functions

max_drawdown = function(r, denominator = "start"){
  log_r = log(r + 1)
  sum = 0
  this_sum = 0
  Ri = 1
  Rj = 1
  this_Ri = 1
  this_Rj = 1
  for (i in 1:length(r)){
    this_sum = this_sum + log_r[i]
    this_Rj = i
    if (this_sum<sum){
      sum = this_sum
      Ri = this_Ri
      Rj = this_Rj
    }else if (this_sum>0){
      this_sum = 0
      this_Ri = i + 1
    }
  }
  if (denominator == "start"){
    max_drawdown = exp(sum(log_r[1:(Ri-1)])) - exp(sum(log_r[1:Rj]))
    return(list(max_drawdown=max_drawdown, Ri=Ri, Rj=Rj))
  }else if (denominator == "peak"){
    max_drawdown = -(exp(sum)-1)
  }
}

drawdown_contribution = function(r, weight){
  #   browser()
  ## r: data frame or matrix, each column represents the return series of one asset class
  r = as.matrix(r)
  if (!all.equal(sum(weight), 1)) stop("weight must sum to one")
  price = apply(r+1, 2, cumprod)%*%weight
  r_all = (price - c(1, price[-length(price)]))/c(1, price[-length(price)])
  max_d = max_drawdown(r_all)
  if (max_d$Ri == 1 & max_d$Rj == 1){
    contribution = c(0, 0)
  }else if (max_d$Ri ==1 & max_d$Rj > 1 ){
    contribution = ((r[1, ]+1)-apply(r[1:max_d$Rj, ]+1, 2, prod)) * weight
  }else if (max_d$Ri ==2 ){
    contribution = ((r[1, ]+1)-apply(r[1:max_d$Rj, ]+1, 2, prod)) * weight
  }else {
    contribution = (apply(r[1:(max_d$Ri-1), ]+1, 2, prod)-apply(r[1:max_d$Rj, ]+1, 2, prod)) * weight
  }
  return(list(max_drawdown = max_d$max_drawdown, contribution = contribution))
}


# sim_CED: simulate portfolio
sim_CED_helper = function(p = 0.9, n = 100, model1, rand.gen1, 
                   model2, rand.gen2, rep = 1000, weights){# browser()
  maxdrawdown_df = t(sapply(1:rep, function(x){
    ts.sim1 = arima.sim(model = model1, n = n, rand.gen = rand.gen1)
    ts.sim2 = arima.sim(model = model2, n = n, rand.gen = rand.gen2)
    CED_contri = unlist(drawdown_contribution(cbind(ts.sim1, ts.sim2), weight = weights))
    return(CED_contri)
  }))
  tail_index = maxdrawdown_df[, "max_drawdown"] > quantile(maxdrawdown_df[, "max_drawdown"], probs = 0.9)
  res = colMeans(maxdrawdown_df[tail_index, ])
  names(res)[1] = "CED"
  return(res)
}


################# repeat CED to make it stable
sim_CED <- function(p = 0.9, n = 100, model1, rand.gen1, 
                    model2, rand.gen2, rep = 1000, weights, nrep){
  ret <- replicate(nrep, sim_CED_helper (p = 0.9, n = 100, model1, rand.gen1, 
                                         model2, rand.gen2, rep = 1000, weights = weight))
  return(rowMeans(ret))
}
# sim_CED(p = 0.9, n = 100, model1 = model, rand.gen1 =rand.gen, 
#          model2 = model, rand.gen2 = rand.gen, rep = 1000, weights = weight, 5)



sim_ES = function(p = 0.9, n = 100, model1, rand.gen1, 
                  model2, rand.gen2, rep = 100, weights){
  ES_df = t(sapply(1:rep, function(x){
    ts.sim1 = arima.sim(model = model1, n = n, rand.gen = rand.gen1)
    ts.sim2 = arima.sim(model = model2, n = n, rand.gen = rand.gen2)
    ts.df = as.data.frame(cbind(ts.sim1, ts.sim2))
    rownames(ts.df) = seq(as.Date("2000/1/1"), by = "day", length.out = n)
    res = ES(ts.df, weights = weights, p = p, portfolio_method = "component")
    return(c(ES = res$MES, mrc = res$contribution))
  }))
  res = colMeans(ES_df)
  return(res)
}

# working
sim_VaR = function(p = 0.9, n = 100, model1, rand.gen1, 
                  model2, rand.gen2, rep = 100, weights){
  VaR_df = t(sapply(1:rep, function(x){
    ts.sim1 = arima.sim(model = model1, n = n, rand.gen = rand.gen1)
    ts.sim2 = arima.sim(model = model2, n = n, rand.gen = rand.gen2)
    ts.df = as.data.frame(cbind(ts.sim1, ts.sim2))
    rownames(ts.df) = seq(as.Date("2000/1/1"), by = "day", length.out = n)
    res = VaR(ts.df, weights = weights, p = p, portfolio_method = "component")
    return(c(VaR = res$MVaR, mrc = res$contribution))
  }))
  res = colMeans(VaR_df)
  return(res)
}

# working
sim_volatility = function(p = 0.9, n = 100, model1, rand.gen1, 
                  model2, rand.gen2, rep = 1000, weights){
  volatility_df = t(sapply(1:rep, function(x){
    ts.sim1 = arima.sim(model = model1, n = n, rand.gen = rand.gen1)
    ts.sim2 = arima.sim(model = model2, n = n, rand.gen = rand.gen2)
    ts.df = as.data.frame(cbind(ts.sim1, ts.sim2))
    rownames(ts.df) = seq(as.Date("2000/1/1"), by = "day", length.out = n)
    volatility = portvol(colnames(ts.df), weights = weights,
                  start = rownames(ts.df)[1], end = rownames(ts.df)[nrow(ts.df)], data = ts.df)
    mrc = mctr(colnames(ts.df), weights = weights,
               start = rownames(ts.df)[1], end = rownames(ts.df)[nrow(ts.df)], data = ts.df)*weights
    return(c(Volatility = volatility, mrc = mrc))
  }))
  res = colMeans(volatility_df)
  return(res)
}

model1 = model
model2 = model
rand.gen1 = rand.gen
rand.gen2 = rand.gen
n = 100

##### tests

model = list(ar=0.1)
rand.gen = function(n) rnorm(n, sd = 0.01)
weights = c(0.5, 0.5)

sim_CED(p = 0.9, n = 100, model1 = model, rand.gen1 = rand.gen, 
        model2 = model, rand.gen2 = rand.gen, rep = 1000, weights = weight)

sim_ES(p = 0.9, n = 100, model1 = model, rand.gen1 = rand.gen, 
        model2 = model, rand.gen2 = rand.gen, rep = 1000, weights = weight)

sim_VaR(p = 0.9, n = 100, model1 = model, rand.gen1 = rand.gen, 
       model2 = model, rand.gen2 = rand.gen, rep = 1000, weights = weight)

sim_volatility(p = 0.9, n = 100, model1 = model, rand.gen1 = rand.gen, 
        model2 = model, rand.gen2 = rand.gen, rep = 1000, weights = weight)

ts.sim = arima.sim(model = list(ar=0.1), n = 100, rand.gen = function(n) rnorm(n, sd = 0.01))


########################### simulation for difference param #################################
ts.ar1 <- expand.grid(seq(from = -0.7, to = 0.7, by = 0.1), seq(from = -0.7, to = 0.7, by = 0.1))
ts.param.ar1 = as.data.frame(t(ts.ar1))
rownames(ts.param.ar1) = NULL
colnames(ts.param.ar1) = 1:ncol(ts.param.ar1)

risk_measure = "CED"
# FUN = get(paste("sim_",risk_measure, sep = ""))
# 
# rand.gen = function(n) rnorm(n, sd = 0.01)
# weight = c(0.6,0.4)
# helper<- function(vec){
#   # vec can be a row of a dataframe with 2 value, also can be a list of parameters
#   ret <- FUN(p = 0.9, n = 100, model1 = list(vec[1]),rand.gen1 = rand.gen, 
#       model2 = list(vec[2]), rand.gen2 = rand.gen, rep = 100, weights = weight)
#   return(ret)
# }
# # helper(ts.param.ar1[1,])
# sapply(ts.param.ar1[,1:2],helper)

group_sim <- function(param, risk_measure, rand.gen, weight){
#   browser()
  FUN = get(paste("sim_",risk_measure, sep = ""))
  ret1 <- sapply(param, function(vec){
    return(FUN(p = 0.9, n = 1000, model1 = list(ar=vec[1]),rand.gen1 = rand.gen, 
             model2 = list(ar=vec[2]), rand.gen2 = rand.gen, rep = 100, weights = weight))})
  ret <- t(rbind(param,ret1))
  colnames(ret)[1:2] = c("model1","model2")
  return(ret)
}

weightss <- list()
weightss[["w1"]]  = c(0.5, 0.5)
weightss[["w2"]]  = c(0.4, 0.6)
weightss[["w3"]]  = c(0.3, 0.7)

# run CED for different weight
risk_measure = "CED"
for (i in names(weightss)){
  assign(paste(risk_measure,"_ar1_",i, sep = ""),group_sim(ts.param.ar1, risk_measure = risk_measure, 
                                                           rand.gen = function(n) rnorm(n, sd = 0.01),weight = weightss[[i]]))
}

# run VaR for different weight
risk_measure = "VaR"
# run ES for different weight
risk_measure = "ES"

## reform the dataframe
# risk_measure = "CED"
# df_names <- paste(risk_measure, "_ar1_",names(weightss), sep = "")

helper_get_ratio <- function(risk_measure,wi){
#   browser()
  df <- get(paste(risk_measure, "_ar1_",wi, sep = ""))
  ret <- df[,!(colnames(df) %in% c("model1","model2",risk_measure))]/df[,risk_measure]
  colnames(ret) <- paste(wi, c(".sim1",".sim2"), sep = "")
  return(ret)
}

# helper_get_ratio(risk_measure, "w1")

get_ratio <- function(risk_measure){
  df <- ts.ar1
  for (wi in names(weightss)){
    df = cbind(df,helper_get_ratio(risk_measure,wi))
  }
  colnames(df)[1:2] = c("model1","model2")
  return (df)
}
# get_ratio("CED")
#################################### plot it ######################################
# CED

# df_CED <- df[(df[,model2] %in% c(-0.7,-0.1,.7,.1)),]
# df_CED <- df_CED[!names(df_CED) %in% paste(weightss,".sim2", sep = "")]
# ret <- melt(df_CED, id = list("model1","model2"))
# plot df formation function:

helper_plot_df <- function(risk_measure, fixat = -0.7){
#   browser()
  df <- get_ratio(risk_measure)
  df_CED = df[abs(df[,"model2"] - fixat)<1e-13,]
  df_CED <- df_CED[!names(df_CED) %in% paste(names(weightss),".sim2", sep = "")]
  ret = melt(df_CED, id = c("model1","model2"))
  return (ret)
}

# helper_plot_df("CED", fixat = -0.1)
plot_rc <- function(risk_measure, fixats = c(-0.7,-0.1,.7,.1), model = "ar1"){
  png(paste("Analysis/figures/risk_contribution/", risk_measure,"_", model, 
            ".png", sep = ''), width = 1600, height = 1600)
  plots = list()
  # fixats <-  c(-0.7,-0.1,.7,.1)
  for (i in fixats){
    print(i)
    plots[[toString(i)]] <- ggplot(data = helper_plot_df(risk_measure,fixat = i), 
                                   aes(x = model1, y = value, group = as.factor(variable), color = variable))+
      geom_line()+
      ylab(paste(risk_measure, "Proportion"))+
      xlab(expression(rho))+
      theme_bw()+
      ggtitle(paste("AR coef of model 2:",toString(i)))
  }
  do.call("grid.arrange", c(plots, ncol=2))
  dev.off()
}
plot_rc("CED")
plot_rc("VaR")
plot_rc("ES")






# risk_measure = "CED"
# plots = list()
# # fixats <-  c(-0.7,-0.1,.7,.1)
# for (i in fixats){
#   print(i)
#   plots[[toString(i)]] <- ggplot(data = helper_plot_df(risk_measure,fixat = i), 
#                       aes(x = model1, y = value, group = as.factor(variable), color = variable))+
#     geom_line()+
#     ylab(paste(risk_measure, "Proportion"))+
#     xlab(expression(rho))+
#     theme_bw()+
#     ggtitle(paste("AR coef of model 2:",toString(i)))
# }
# do.call("grid.arrange", c(plots, ncol=2))
# dev.off()






# p_rc_CED <- ggplot (data = df_CED, aes(x = model1, y = value, group = variable))+
#   geom_line(aes(color = variable))+
#   ylab(paste(risk_measure, "Proportion"))+
#   xlab(expression(rho))+
#   theme_bw()
# 
# plotRollingStat <- function(windw, FUN, ymin, ymax, scale = 1, freq_option = ""){
#   png(paste("../figures/rolling_stats/", FUN, windw, freq_option, 
#             "_scaled.png", sep = ''), width = 1600, height = 1600)
#   plots = list()
#   for (asset in names(assetData)){
#     plots[[asset]] <- ggplot(get(paste(FUN, windw, freq_option, sep = ''))[[asset]], 
#                              aes(x=Date, y=values*scale)) +
#       geom_line() +
#       ggtitle(asset) +
#       labs(y = paste(FUN)) +
#       ylim(ymin, ymax) + 
#       labs(x = "Date") +
#       theme_minimal()
#   }
#   do.call("grid.arrange", c(plots, ncol=3))
#   dev.off()
# }











