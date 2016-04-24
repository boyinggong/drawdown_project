source("read_data.R")
source("risk_diagostics_function.R")

library(PerformanceAnalytics)

max_drawdown = function(r){
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
  max_drawdown = -(exp(sum)-1)
  return(list(max_drawdown=max_drawdown, Ri=Ri, Rj=Rj))
}

## r: data frame or matrix, each column represents the return series of one asset class
## weight: length equal to the number of columns in r, 
## represents the weight of each assets in the portfolio
## return: maximum drawdown, a vector of drawdown contributions(length = number of asset class)

### CHECK CORRECTNESS!!!!!!!

drawdown_contribution = function(r, weight){
  if (!all.equal(sum(weight), 1)) stop("weight must sum to one")
  r_all = r%*%weight
  max_d = max_drawdown(r_all)
  contribution = (1-apply(r[maxD$Ri:max_d$Rj, ]+1, 2, prod)) * weight
  contribution = max_d_asset/sum(max_d_asset)
  return(list(max_drawdown = max_d$max_drawdown, contribution = contribution))
}

drawdown_contribution(r, c(0.4, 0.6))

## calculate risk contribution (rolling and )

risk_contribution = function(){
  
}



### risk contributions of VaR, ES and volatility
### Reference: 
### ES http://braverock.com/brian/R/PerformanceAnalytics/html/ES.html
### VaR http://braverock.com/brian/R/PerformanceAnalytics/html/VaR.html
### volatility page3 https://cran.r-project.org/web/packages/PortRisk/PortRisk.pdf



















## TESTS

# r = matrix(returns, nc =2)
# weight = c(0.4, 0.6)
# r%*%weight
# 
# a1 = 1
# a2 = 100
# 
# dt <- as.data.frame(assetData$AGG$retrn_dl[a1:a2])
# rownames(dt) <- assetData$AGG$Date[a1:a2] 
# maxDrawdown(dt)
# 
# returns = assetData$AGG[a1:a2, ]$retrn_dl
# myfunc = max_drawdown(returns)
# myfunc$max_drawdown
# 
# return_series = assetData$AGG$retrn_dl[(myfunc$Ri+a1-1):(myfunc$Rj+a1-1)]
# -(exp(sum(log(return_series+1)))-1)
# 
# 
# library(rbenchmark)
# benchmark(maxDrawdown(dt), max_drawdown(returns))