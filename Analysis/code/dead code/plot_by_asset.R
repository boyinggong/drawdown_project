
######################### plot the empirical maximum drawdown distribution by asset #########################

windw = "yr5"

nasset = 9

count = 1
png("../results/maxdd_RMZ.png", width = 800, height = 200)
plots = list()
for (windw in c("mon3", "mon6", "yr1", "yr2", "yr5")){
  dt <- data.frame(Dd = get(paste("maxDrawdown", windw, sep=''))[[nasset]], 
                   Date = get(paste("maxDrawdown", windw, "_date", sep=''))[[nasset]])
  plots[[count]] <- ggplot(dt, aes(x=Dd)) +
    geom_density() + 
    ggtitle(windw) +
    # labs(x = "Maximum Drawdown") + 
    xlim(0, 1) + ylim(0, 10)
  count = count+1
}
multiplot(plotlist = plots, cols = 5)
dev.off()


######################### plot the risk measures by asset #########################

png("../results/risk_measure_RMZ.png", width = 800, height = 200)
nasset = 9
plots = list()
count = 1
for (FUN in c("VaR", "var", "ES", "maxDrawdown")){
  windw = "6mon"
  if (FUN == "maxDrawdown"){
    windw = "mon6"
  }
  plt <- data.frame(y_axis = get(paste(FUN, windw, sep = ''))[[nasset]]*100,
                    x_axis = get(paste(FUN, windw, "_date",sep = ''))[[nasset]])
  plots[[count]] <- ggplot(plt, aes(x=x_axis, y=y_axis)) +
    geom_line() +
    ggtitle(FUN) +
    labs(y = FUN) +
    # ylim(0, 15) + 
    labs(x = "Date")
  count = count+1
}
multiplot(plotlist = plots, cols = 4)
dev.off()

################## plot CED verses ES ##################

count = 1
png(paste("../results/CED_ES_", windw, ".png", sep = ''), width = 1600, height = 1600)
plots = list()
for (i in assetsList){
  plt <- data.frame(y_axis = CED_3mon_5yr[[count]],
                    x_axis = ES3mon5yr[[count]])
  plots[[i]] <- ggplot(plt, aes(x=x_axis, y=y_axis)) +
    geom_point() +
    ggtitle(i) + 
    labs(y = "CED", x = "ES")
#    ylim(0, 1) +
  count = count+1
}
multiplot(plotlist = plots, cols = 3)
dev.off()

################## plot CED verses first-order serial correlation ##################

count = 1
png(paste("../results/CED_cor_", windw, ".png", sep = ''), width = 1600, height = 1600)
plots = list()
for (i in assetsList){
  plt <- data.frame(y_axis = CED_3mon_5yr[[count]],
                    x_axis = calc_auto_corr3mon5yr[[count]])
  plots[[i]] <- ggplot(plt, aes(x=x_axis, y=y_axis)) +
    geom_point() +
    ggtitle(i) + 
    labs(y = "CED", x = "Autocorrelation")
  #    ylim(0, 1) +
  count = count+1
}
multiplot(plotlist = plots, cols = 3)
dev.off()





