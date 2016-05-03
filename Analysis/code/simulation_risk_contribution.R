
library(PerformanceAnalytics)
library(reshape)
library(PortRisk)
library(cowplot)
library(ggplot2)
library(cowplot)
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

sim_CED = function(p = 0.9, n = 100, model1, rand.gen1, 
                   model2, rand.gen2, rep = 1000, weights){
  maxdrawdown_df = t(sapply(1:rep, function(x){
    ts.sim1 = arima.sim(model = model1, n = n, rand.gen = rand.gen1)
    ts.sim2 = arima.sim(model = model2, n = n, rand.gen = rand.gen2)
    CED_contri = unlist(drawdown_contribution(cbind(ts.sim1, ts.sim2), weight = weight))
    return(CED_contri)
  }))
  tail_index = maxdrawdown_df[, "max_drawdown"] > quantile(maxdrawdown_df[, "max_drawdown"], probs = 0.9)
  res = colMeans(maxdrawdown_df[tail_index, ])
  names(res)[1] = "CED"
  return(res)
}

sim_ES = function(p = 0.9, n = 100, model1, rand.gen1, 
                  model2, rand.gen2, rep = 1000, weights){
  ES_df = t(sapply(1:rep, function(x){
    ts.sim1 = arima.sim(model = model1, n = n, rand.gen = rand.gen1)
    ts.sim2 = arima.sim(model = model2, n = n, rand.gen = rand.gen2)
    ts.df = as.data.frame(cbind(ts.sim1, ts.sim2))
    rownames(ts.df) = seq(as.Date("2000/1/1"), by = "day", length.out = n)
    res = ES(ts.df, weights = weight, p = p, portfolio_method = "component")
    return(c(ES = res$MES, mrc = res$contribution))
  }))
  res = colMeans(ES_df)
  return(res)
}

# working
sim_VaR = function(p = 0.9, n = 100, model1, rand.gen1, 
                  model2, rand.gen2, rep = 1000, weights){
  VaR_df = t(sapply(1:rep, function(x){
    ts.sim1 = arima.sim(model = model1, n = n, rand.gen = rand.gen1)
    ts.sim2 = arima.sim(model = model2, n = n, rand.gen = rand.gen2)
    ts.df = as.data.frame(cbind(ts.sim1, ts.sim2))
    rownames(ts.df) = seq(as.Date("2000/1/1"), by = "day", length.out = n)
    res = VaR(ts.df, weights = weight, p = p, portfolio_method = "component")
    return(c(VaR = res$MVaR, mrc = res$contribution))
  }))
  res = colMeans(VaR_df)
  return(res)
}

# # working
# sim_volatility = function(p = 0.9, n = 100, model1, rand.gen1, 
#                   model2, rand.gen2, rep = 1000, weights){
#   volatility_df = t(sapply(1:rep, function(x){
#     ts.sim1 = arima.sim(model = model1, n = n, rand.gen = rand.gen1)
#     ts.sim2 = arima.sim(model = model2, n = n, rand.gen = rand.gen2)
#     ts.df = as.data.frame(cbind(ts.sim1, ts.sim2))
#     rownames(ts.df) = seq(as.Date("2000/1/1"), by = "day", length.out = n)
#     res = portvol(ts.df, weights = weight,
#                   start = rownames(ts.df)[1], end = rownames(ts.df)[nrow(ts.df)], data = ts.df)
#     return(c(Volatility = res$MES, mrc = res$contribution))
#   }))
#   res = colMeans(volatility_df)
#   return(res)
# }


##### tests

model = list(ar=0.1)
rand.gen = function(n) rnorm(n, sd = 0.01)
weight = c(0.5, 0.5)

sim_CED(p = 0.9, n = 100, model1 = model, rand.gen1 = rand.gen, 
        model2 = model, rand.gen2 = rand.gen, rep = 1000, weights = weight)

sim_ES(p = 0.9, n = 100, model1 = model, rand.gen1 = rand.gen, 
        model2 = model, rand.gen2 = rand.gen, rep = 1000, weights = weight)

sim_VaR(p = 0.9, n = 100, model1 = model, rand.gen1 = rand.gen, 
       model2 = model, rand.gen2 = rand.gen, rep = 1000, weights = weight)



ts.sim = arima.sim(model = list(ar=0.1), n = 100, rand.gen = function(n) rnorm(n, sd = 0.01))





