
source("read_data.R")

#################
### functions ###
#################

VaR <- function(R, p = 0.95, sign = -1){
  sign * quantile(R, probs = 1-p)
}

ES <- function(R, p = 0.95, sign = -1){
  sign * mean(R[R < quantile(R, probs = 1-p)])
}

volatility <- function(R){
  sqrt(252)*sqrt(var(R))
}

###################################
### calculate the rolling stats ###
###################################

# 3 month: 63 days
# 6 month: 126 days
# 1 year: 252 days
# 2 years: 504 days
# 5 years: 1260 days
# calculation are based on daily returns

lvs <- c(.9, .95, .99)
prds <- c(63, 126, 252, 504, 1260, 63+1260)
names(prds) <- c("3mon", "6mon", "1yr", "2yr", "5yr", "3mon5yr")

calcRolling <- function(val, prd, FUN, ...){
  res <- sapply(1:(nrow(val)-prd+1), function(x){
    dt <- as.data.frame(val$retrn_dl[x:(x+prd-1)])
    rownames(dt) <- val$Date[x:(x+prd-1)] 
    do.call(FUN, list(dt[, 1], ...))
  })
}

getRollingDataFrame <- function(windw, FUN, assetData, ...){
  lapply(assetData, 
         function(asset)data.frame(values = calcRolling(asset, prd = prds[windw], FUN, ...),
                                   Date = asset$Date[prds[windw]:(nrow(asset))]))
}

assign(paste(FUN = "ES", windw = "1yr", sep = ''), 
       getRollingDataFrame(windw = "1yr", FUN = "ES", assetData = assetData , p = 0.95))
assign(paste(FUN = "ES", windw = "6mon", sep = ''), 
       getRollingDataFrame(windw = "6mon", FUN = "ES", assetData = assetData, p = 0.95))
assign(paste(FUN = "VaR", windw = "1yr", sep = ''), 
       getRollingDataFrame(windw = "1yr", FUN = "VaR", assetData = assetData, p = 0.95))
assign(paste(FUN = "VaR", windw = "6mon", sep = ''), 
       getRollingDataFrame(windw = "6mon", FUN = "VaR", assetData = assetData, p = 0.95))
assign(paste(FUN = "volatility", windw = "1yr", sep = ''), 
       getRollingDataFrame(windw = "1yr", FUN = "volatility", assetData = assetData))
assign(paste(FUN = "volatility", windw = "6mon", sep = ''), 
       getRollingDataFrame(windw = "6mon", FUN = "volatility", assetData = assetData))

##############################
### plot the rolling stats ###
##############################

plotRollingStat <- function(windw, FUN, ymin, ymax, scale = 1, freq_option = ""){
  png(paste("../figures/rolling_stats/", FUN, windw, freq_option, 
            "_scaled.png", sep = ''), width = 1600, height = 1600)
  plots = list()
  for (asset in names(assetData)){
    plots[[asset]] <- ggplot(get(paste(FUN, windw, freq_option, sep = ''))[[asset]], 
                             aes(x=Date, y=values*scale)) +
      geom_line() +
      ggtitle(asset) +
      labs(y = paste(FUN)) +
      ylim(ymin, ymax) + 
      labs(x = "Date") +
      theme_minimal()
  }
  do.call("grid.arrange", c(plots, ncol=3))
  dev.off()
}

plotRollingStat(FUN = "ES", windw = "1yr", ymin = 0, ymax = 12, scale = 100)
plotRollingStat(FUN = "ES", windw = "6mon", ymin = 0, ymax = 15, scale = 100)
plotRollingStat(FUN = "VaR", windw = "1yr", ymin = 0, ymax = 12, scale = 100)
plotRollingStat(FUN = "VaR", windw = "6mon", ymin = 0, ymax = 12, scale = 100)
plotRollingStat(FUN = "volatility", windw = "1yr", ymin = 0, ymax = 1)
plotRollingStat(FUN = "volatility", windw = "6mon", ymin = 0, ymax = 1.25)
