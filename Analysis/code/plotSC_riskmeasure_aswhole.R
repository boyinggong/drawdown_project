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
orderList <- list()
orderList[["ar1"]] <- c(1,0,0)
orderList[["ma1"]] <- c(0,0,1)
orderList[["arma11"]] <- c(1,0,1)

findkappa1 <- function(order, assetList){
  ret <- numeric(length(assetList))
  names(ret) <- names(assetList)
  for (asset in names(assetList)){
    if (order == "ma1"){
      ret[asset] <-arima(assetList[[asset]]$retrn_dl, order = orderList[[order]])$coef["ma1"]
    }else {
      ret[asset] <-arima(assetList[[asset]]$retrn_dl, order = orderList[[order]])$coef["ar1"]}
  }
  return(ret)
}
# temp3 = findkappa1("ar1", assetData)
# temp4 = findkappa1("ma1",assetData)
# temp5 = findkappa1("arma11",assetData)

Kappa_ar1_all <- findkappa1("ar1", assetData)
# MA(1)
Kappa_ma1_all <- findkappa1("ma1",assetData)
# ARMA(1,1)
Kappa_arma11_all <- findkappa1("arma11",assetData)


# BEST MODEL
findBestModel <- function(assetList){
 # browser()
  BestOrder_all <- list()
  for (asset in names(assetList)){
    # print(asset)
    temp = auto.arima(assetList[[asset]]$retrn_dl)$arma
    BestOrder_all[[asset]] = c(temp[1],temp[6],temp[2])
  }
  return (BestOrder_all)
}
# temp6 <- findBestModel(assetData)
BestOrder_all <- findBestModel(assetData)

findkappa2 <- function(BestOrder_all,assetList){
  Kappa_best_all <- numeric(length(assetList))
  names(Kappa_best_all) <- names(assetList)
  for (asset in names(assetList)){
    tmp <- arima(assetList[[asset]]$retrn_dl, order = BestOrder_all[[asset]])$coef["ar1"]
    if (is.na(tmp)){
      Kappa_best_all[asset] = 0
    }else{
    Kappa_best_all[asset] <- arima(assetList[[asset]]$retrn_dl, order = BestOrder_all[[asset]])$coef["ar1"]}
  }
  return (Kappa_best_all)
}
#temp7 <- findkappa2(BestOrder_all, assetData)

Kappa_best_all <- findkappa2(BestOrder_all, assetData)

############################# First Order Serial Correlation ##############################
# AR(1)
findrho1 <- function(order, assetList){
  ret <- numeric(length(assetList))
  names(ret) <- names(assetList)
  for (asset in names(assetList)){
    ret[asset] <- SerCol_single_period(assetList[[asset]]$retrn_dl,orderList[[order]],ser_order = 1)
  }
  return(ret)
}
#temp8 <- findrho1("ar1",assetData)
# temp9 <- findrho1("ma1",assetData)
# temp9 <- findrho1("arma11",assetData)


rho_ar1_all <- findrho1("ar1",assetData)

# MA(1)
rho_ma1_all <- findrho1("ma1",assetData)

# ARMA(1,1)
rho_arma11_all <- findrho1("arma11",assetData)

# Bestorder
findrho2 <- function(BestOrder_all,assetList){
  rho_best_all <- numeric(length(assetList))
  names(rho_best_all) <- names(assetList)
  for (asset in names(assetList)){
    tmp2 <- SerCol_single_period(assetList[[asset]]$retrn_dl,BestOrder_all[[asset]],ser_order = 1)
    if (is.na(tmp2)){
      rho_best_all[asset] = 0
    }else{
    rho_best_all[asset] <- tmp2}
  }
  return(rho_best_all)
}
# temp1 <- findrho2(BestOrder_all,assetData)
# all.equal(temp1, rho_best_all)

rho_best_all <- findrho2(BestOrder_all,assetData)


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
