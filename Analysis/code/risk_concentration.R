source("read_data.R")
source("risk_diagostics_function.R")

library(PerformanceAnalytics)

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
  ## r: data frame or matrix, each column represents the return series of one asset class
  r = as.matrix(r)
  if (!all.equal(sum(weight), 1)) stop("weight must sum to one")
  price = apply(r+1, 2, cumprod)%*%weight
  r_all = (price - c(1, price[-length(price)]))/c(1, price[-length(price)])
  max_d = max_drawdown(r_all)
  contribution = (apply(r[1:(max_d$Ri-1), ]+1, 2, prod)-apply(r[1:max_d$Rj, ]+1, 2, prod)) * weight
  return(list(max_drawdown = max_d$max_drawdown, contribution = contribution))
}

###################
# Test data frame #
###################

SPX = assetData$SPX$retrn_dl[which(assetData$SPX$Date == "2007-01-03"):
                               (which(assetData$SPX$Date == "2008-01-03")-1)]
RMZ = assetData$RMZ$retrn_dl[which(assetData$RMZ$Date == "2007-01-03"):
                               (which(assetData$RMZ$Date == "2008-01-03")-1)]
test_r = data.frame(SPX=SPX, RMZ=RMZ)

max_drawdown(test_r$SPX)
test_contr = drawdown_contribution(r=test_r, weight=c(0.4, 0.6))


## calculate risk contribution (rolling and )

max_drawdown(SPX)

risk_contribution = function(){
  
}



### risk contributions of VaR, ES and volatility
### Reference: 
### ES http://braverock.com/brian/R/PerformanceAnalytics/html/ES.html
### VaR http://braverock.com/brian/R/PerformanceAnalytics/html/VaR.html
### volatility page3 https://cran.r-project.org/web/packages/PortRisk/PortRisk.pdf




