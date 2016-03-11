
library(PerformanceAnalytics)
library(ggplot2)
library(grid)
library(gridExtra)

source("read_data.R")

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

assign(paste(FUN = "maxDrawdown", windw = "3mon", sep = ''), 
       getMaxDdDataFrame(windw = "3mon", Data = assetData))

########################################################
### plot the empirical maximum drawdown distribution ###
########################################################

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

plotMaxDrawdown(windw = "3mon", xmin = 0, xmax = 0.4, ymin = 0, ymax = 80)

#########################
### calculate the CED ###
#########################

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

assign("CED3mon5yr", 
       getCEDDataFrame(windw = "5yr", Data = maxDrawdown3mon, p = 0.9))
plotRollingStat(windw = "3mon5yr", FUN = "CED", ymin = 0, ymax = 0.5, scale = 1)

assign("CED3mon2yr", 
       getCEDDataFrame(windw = "2yr", Data = maxDrawdown3mon, p = 0.9))
plotRollingStat(windw = "3mon2yr", FUN = "CED", ymin = 0, ymax = 0.7, scale = 1)

