###########################
### Plot for each class ###
###########################
# p all I have is .9
# ES and VaR
ES_all <- as.vector(read.csv("Analysis/results/resES.csv", header = TRUE)["X0.9"][-c(5,7),])
VAR_all <- as.vector(read.csv("Analysis/results/resVaR.csv", header = TRUE)["X0.9"][-c(5,7),])
names(ES_all) = names(maxDrawdown3mon)
names(VAR_all) = names(maxDrawdown3mon)

prds_rolling = prds 
prds = unlist(lapply(assetData, nrow))

# CED
CED_all <- rep(0, length(maxDrawdown3mon))
names(CED_all) = names(maxDrawdown3mon)
for (asset in names(maxDrawdown3mon)){
  CED_all[asset] = calcCED(maxDrawdown3mon[[asset]]$values, 
                           prd = length(maxDrawdown3mon[[asset]]$values))
}

# volatility
VTT_all <- numeric(length(assetData))
names(VTT_all) <- names(assetData)
for (asset in names(assetData)){
  VTT_all[asset] = volatility(assetData[[asset]]$retrn_dl,freq_option = "ALL")
}

# Kappa for each model
# AR(1)
Kappa_ar1_all <- numeric(length(assetData))
names(Kappa_ar1_all) <- names(assetData)
for (asset in names(assetData)){
  Kappa_ar1_all[asset] <-arima(assetData[[asset]]$retrn_dl, order = c(1,0,0))$coef["ar1"]
}

# MA(1)
Kappa_ma1_all <- numeric(length(assetData))
names(Kappa_ma1_all) <- names(assetData)
for (asset in names(assetData)){
  Kappa_ma1_all[asset] <-arima(assetData[[asset]]$retrn_dl, order = c(0,0,1))$coef["ma1"]
}

# ARMA(1,1)
Kappa_arma11_all <- numeric(length(assetData))
names(Kappa_arma11_all) <- names(assetData)
for (asset in names(assetData)){
  Kappa_arma11_all[asset] <-arima(assetData[[asset]]$retrn_dl, order = c(1,0,1))$coef["ar1"]
}
#rm(Kappa_arma1_all)
# BEST MODEL
BestOrder_all <- rep(list(numeric(3)),11)
names(BestOrder_all) <- names(assetData)

for (asset in names(assetData)){
  temp = auto.arima(assetData[[asset]]$retrn_dl)$arma
  BestOrder_all[asset] = list(c(temp[1],temp[6],temp[2]))
}

Kappa_best_all <- numeric(length(assetData))
names(Kappa_best_all) <- names(assetData)
for (asset in names(assetData)){
  Kappa_best_all[asset] <- arima(assetData[[asset]]$retrn_dl, order = BestOrder_all[[asset]])$coef["ar1"]
}

############################# First Order Serial Correlation ##############################
# AR(1)
rho_ar1_all <- numeric(length(assetData))
names(rho_ar1_all) <- names(assetData)
for (asset in names(assetData)){
  rho_ar1_all[asset] <- SerCol_single_period(assetData[[asset]]$retrn_dl,c(1,0,0),ser_order = 1)
}


# MA(1)
rho_ma1_all <- numeric(length(assetData))
names(rho_ma1_all) <- names(assetData)
for (asset in names(assetData)){
  rho_ma1_all[asset] <- SerCol_single_period(assetData[[asset]]$retrn_dl,c(0,0,1),ser_order = 1)
}

# ARMA(1,1)
rho_arma11_all <- numeric(length(assetData))
names(rho_arma11_all) <- names(assetData)
for (asset in names(assetData)){
  rho_arma11_all[asset] <- SerCol_single_period(assetData[[asset]]$retrn_dl,c(1,0,1),ser_order = 1)
}

# Bestorder
rho_best_all <- numeric(length(assetData))
names(rho_best_all) <- names(assetData)
for (asset in names(assetData)){
  print(asset)
  rho_best_all[asset] <- SerCol_single_period(assetData[[asset]]$retrn_dl,BestOrder_all[[asset]],ser_order = 1)
}


############################ create dataframe and order ################################
formDataframe <- function(riskMeasure, serCol, opt = "Raw"){
  #browser()
  ret <- as.data.frame(cbind(get(riskMeasure), get(serCol)))
  if (opt == "Cleaned"){
    ret = ret[-c(4,5,11),]
  }
  colnames(ret) <- c("x_axis", "y_axis")
  ret <- ret[order(ret["x_axis"]),]
  return(ret)
}
# formDataframe("ES_all","Kappa_ar1_all")

####################################### script ##########################################
# riskmeasurements and Kappa(1)
RM = "VTT"
SC = "Kappa"
opt = "Cleaned"
models = c("ar1","ma1","arma11", "best")

png(paste("Analysis/figures/asset_sc_all/asset-",RM, SC, opt , ".png", sep = ''),
    width = 800, height = 800)
plots = list()
for (i in models){
  #i = "ar1"
  riskMeasure <- paste(RM,"_all", sep = "")
  serCol <- paste(SC ,i,"all", sep = "_")
  plt <- formDataframe(riskMeasure, serCol,opt=opt)
  plots[[i]] <- ggplot(plt, aes(x=x_axis, y=y_axis)) +
    geom_line(col = "red")+
    geom_text(aes(label = rownames(plt))) +
    ggtitle(paste("Model:", i, sep=" ")) +
    xlab(RM)+
    ylab(paste(SC," order =1", sep = " "))
}
multiplot(plotlist = plots, cols = 3)
dev.off()



# riskmeasurements and rho(1)
RM = "CED"
SC = "rho"
opt = "Cleaned"
models = c("ar1","ma1","arma11", "best")

png(paste("Analysis/figures/asset_sc_all/asset-",RM, SC,opt, ".png", sep = ''),
    width = 800, height = 800)
plots = list()
for (i in models){
  #i = "ar1"
  riskMeasure <- paste(RM,"_all", sep = "")
  serCol <- paste(SC ,i,"all", sep = "_")
  plt <- formDataframe(riskMeasure, serCol,opt = opt)
  plots[[i]] <- ggplot(plt, aes(x=x_axis, y=y_axis)) +
    geom_line(col = "red")+
    geom_text(aes(label = rownames(plt))) +
    ggtitle(paste("Model:", i, sep=" ")) +
    xlab(RM)+
    ylab(paste(SC," order =1", sep = " "))
}
multiplot(plotlist = plots, cols = 3)
dev.off()

save(list = ls(all = TRUE), file= "plot_SC_RM.RData")
