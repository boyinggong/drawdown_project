ts_sim <- arima.sim(list(order = c(1,0,0), ar = 0.7), n = 3000)
acf(ts_sim)
pacf(ts_sim)

ts_df = as.data.frame(cbind(1:3000, ts_sim))
colnames(ts_df) <-c("Date", "retrn_dl")
calcRolling <- function(val, prd, FUN, ...){
  res <- sapply(1:(nrow(val)-prd+1), function(x){
    dt <- as.data.frame(val$retrn_dl[x:(x+prd-1)])
    rownames(dt) <- val$Date[x:(x+prd-1)] 
    do.call(FUN, list(dt[, 1], ...))
  })
}
# VaR 2yr

# ES 2yr

# 
