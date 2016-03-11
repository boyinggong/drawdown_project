library(ggplot2)
library(grid)
library(gridExtra)

source("read_data.R")

#########################
### basic information ###
#########################

for (asset in names(assetData)){
  print(asset)
  print(paste(range(assetData[[asset]]$Date), collapse = " to "))
}

########################
### plot the returns ###
########################

png("../figures/summary_daily/returns.png", width = 1200, height = 1200)
plots = list()
for (asset in names(assetData)){
  plots[[asset]] <- ggplot(plt <- data.frame(x_axis = assetData[[asset]]$Date,
                                             y_axis = assetData[[asset]]$retrn_dl), 
                           aes(x=x_axis, y=y_axis)) +
    geom_line() +
    ggtitle(asset) +
    labs(y = "Returns") + 
    ylim(-0.15, 0.15) +
    labs(x = "Date") +
    theme_minimal()
}
do.call("grid.arrange", c(plots, ncol=3))
dev.off()

##############################################
### plot the empirical return distribution ###
##############################################

png("../figures/summary_daily/returns_dist.png", width = 1600, height = 1600)
plots = list()
for (asset in names(assetData)){
  if (asset != "G0O1"){
    plots[[asset]] <- ggplot(plt <- data.frame(x_axis = assetData[[asset]]$retrn_dl), 
                             aes(x=x_axis)) +
      geom_density() + 
      ggtitle(asset) +
      labs(x = "Returns") + xlim(-0.04, 0.04) + 
      ylim(0, 175) +
      theme_minimal()
  } else {
    plots[[asset]] <- ggplot(plt <- data.frame(x_axis = assetData[[asset]]$retrn_dl), 
                             aes(x=x_axis)) +
      geom_density() + 
      ggtitle(asset) +
      labs(x = "Returns") + 
      xlim(-0.04, 0.04) +
      theme_minimal()
  }
}
do.call("grid.arrange", c(plots, ncol=3))
dev.off()

##############################################
######## calculate the Sharpe Ratio, #########
### standard deviation, skewness, kurtosis ###
##############################################

statSmmr <- matrix(rep(0, 4*length(assetData)), nrow=length(assetData))
rownames(statSmmr) <- names(assetData)
colnames(statSmmr) <- c("SR","sd","skewness","kurtosis")

for (asset in names(assetData)){
  statSmmr[asset, 2] <- sd(assetData[[asset]]$retrn_dl) * sqrt(252)
  statSmmr[asset, 3] <- skewness(assetData[[asset]]$retrn_dl) # method?
  statSmmr[asset, 4] <- kurtosis(assetData[[asset]]$retrn_dl)
  Rf = 0
  statSmmr[asset, 1] <- (mean(assetData[[asset]]$retrn_dl) - Rf)/ statSmmr[asset, 2] * sqrt(252)
} 

write.csv(statSmmr, file = "../tables/summary_daily/statSmmr.csv")

#################################
#### delete the unuseful data ###
#################################

variables = ls()
variables = variables[-which(variables %in% c("assetData"))]
rm(list = variables)
rm(variables)
