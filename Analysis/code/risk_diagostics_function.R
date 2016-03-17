
library("PerformanceAnalytics")

#################################
# tranditional risk diagnostics # 
#################################

VaR <- function(R, p = 0.95, sign = -1){
  sign * quantile(R, probs = 1-p)
}

ES <- function(R, p = 0.95, sign = -1){
  sign * mean(R[R < quantile(R, probs = 1-p)])
}

volatility <- function(R, freq_option = "Daily"){
  if (freq_option == "Weekly"){
    return(sqrt(52)*sqrt(var(R)))
  }else if(freq_option == "Monthly"){
    return(sqrt(12)*sqrt(var(R)))    
  }
  sqrt(252)*sqrt(var(R))
}

serialCorrelation <- function(R, order = 1){
  R1 = R[1:(length(R)-order)]
  R2 = R[(1+order):length(R)]
  if (sd(R1) == 0 |sd(R2) == 0) return(0) else return(cor(R1, R2))
}

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

##########################
# maximum drawdown & CED #
##########################

calcMaxDd <- function(val, prd){
  res <- sapply(1:(nrow(val)-prd+1), function(x){
    dt <- as.data.frame(val$retrn_dl[x:(x+prd-1)])
    rownames(dt) <- val$Date[x:(x+prd-1)] 
    maxDrawdown(dt)
  })
}

getMaxDdDataFrame <- function(windw, Data = assetData){
  lapply(assetData, 
         function(asset)data.frame(values = calcMaxDd(asset, prd = prds[windw]),
                                   Date = asset$Date[prds[windw]:(nrow(asset))]))
}

plotMaxDrawdown <- function(windw, xmin, xmax, ymin, ymax, freq_option = ''){
  png(paste("../figures/maxDrawdown_CED/maxDrawdown", windw, freq_option, 
            ".png", sep = ''), width = 1600, height = 1600)
  plots = list()
  for (asset in names(assetData)){
    plots[[asset]] <- ggplot(get(paste("maxDrawdown", windw, freq_option, sep = ''))[[asset]], 
                             aes(x=values)) +
      geom_density() +
      ggtitle(asset) +
      xlim(xmin, xmax) + 
      ylim(ymin, ymax) + 
      labs(x = "maxDrawdown") +
      theme_minimal()
  }
  do.call("grid.arrange", c(plots, ncol=3))
  dev.off()
}

calcCED <- function(val, prd, p = 0.9){
  res <- sapply(1:(length(val)-prd+1), function(x){
    dt <- val[x:(x+prd-1)]
    mean(dt[dt > quantile(dt, probs = p)])
  })
}

getCEDDataFrame <- function(windw, Data, p = 0.9){
  lapply(Data, 
         function(asset)data.frame(values = calcCED(asset$values, prd = prds[windw], p),
                                   Date = asset$Date[prds[windw]:(nrow(asset))]))
}

####################################
# correlation between two measures #
####################################

calc_corr <- function(measure1, measure2){
  len_asset_list <- length(measure1)
  corr_vec <- rep(0, len_asset_list)
  names(corr_vec) = names(assetData)
  for (asset in names(assetData)){
    corr_vec[asset] = cor(measure1[[asset]]$values, measure2[[asset]]$values)
  }
  return(corr_vec)
}
