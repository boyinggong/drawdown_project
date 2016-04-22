source("read_data.R")
source("risk_diagostics_function.R")

library(PerformanceAnalytics)

max_drawdown = function(returns){
  log_returns = log(returns + 1)
  sum = 0
  this_sum = 0
  Ri = 1
  Rj = 1
  this_Ri = 1
  this_Rj = 1
  for (i in 1:length(returns)){
    this_sum = this_sum + log_returns[i]
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


a1 = 23
a2 = 100

dt <- as.data.frame(assetData$AGG$retrn_dl[a1:a2])
rownames(dt) <- assetData$AGG$Date[a1:a2] 
maxDrawdown(dt)

returns = assetData$AGG[a1:a2, ]$retrn_dl
myfunc = max_drawdown(returns)
myfunc$max_drawdown

return_series = assetData$AGG$retrn_dl[(myfunc$Ri+a1-1):(myfunc$Rj+a1-1)]
-(exp(sum(log(return_series+1)))-1)


library(rbenchmark)
benchmark(maxDrawdown(dt), max_drawdown(returns))