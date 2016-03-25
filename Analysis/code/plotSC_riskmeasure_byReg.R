regimes_boundary["AGG",]

formDataframe2 <- function(riskMeasure, serCol, opt = "Raw"){
  #browser()
  ret <- as.data.frame(cbind(riskMeasure, serCol))
  if (opt == "Cleaned"){
    ret = ret[-c(4,5,11),]
  }
  colnames(ret) <- c("x_axis", "y_axis")
  ret <- ret[order(ret["x_axis"]),]
  return(ret)
}
###################### Find the corresponding subset in returns ############################
assetData_regime <- list()
for (asset in names(assetData)){
  bound <-regimes_boundary[asset,]
  assetData_regime[[paste(asset, "_reg1",sep = "")]] = assetData[[asset]][bound[1]:bound[2],]
  assetData_regime[[paste(asset, "_reg2",sep = "")]] = assetData[[asset]][bound[3]:bound[4],]
}

#################### Find the Kappa for each model for each regime #######################
Kappa_ar1_reg <- findkappa1("ar1", assetData_regime)
Kappa_ma1_reg <- findkappa1("ma1", assetData_regime)
Kappa_arma11_reg <- findkappa1("arma11", assetData_regime)
BestOrder_reg <- findBestModel(assetData_regime)
Kappa_best_reg <- findkappa2(BestOrder_reg, assetData_regime)

#################### Find the Rho for each model for each regime #######################
rho_ar1_reg <- findrho1("ar1", assetData_regime)
rho_ma1_reg <- findrho1("ma1", assetData_regime)
rho_arma11_reg <- findrho1("arma11", assetData_regime)
rho_best_reg <- findrho2(BestOrder_reg,assetData_regime)

## split the vector to order ##
splitMe <- function(vector){
  is.odd <- rep(c(TRUE, FALSE), length = length(vector))
  list(reg1 = vector[is.odd], reg2 = vector[!is.odd])
}
# splitMe(Kappa_best_reg)

################### Plot Pair ######################
# RM is the risk measurement
# SC is the Kappa/rho
RMs = c("VaR","ES", "Volatility", "CED")
SCs = c("Kappa", "rho")
MODELs = c("ar1","ma1","arma11", "best")


SC = "rho"
REG = "reg1"

plotplotplot <- function(SC, REG){
  png(paste("Analysis/figures/asset_sc_reg/asset-", SC,REG , ".png", sep = ''),
      width = 1600, height = 1600)
  plots = list()
  for (model in models){
    for (RM in RMs){
      #      print(model)
      #      print(RM)
      #     model = "ar1"
      #     RM = "ES"
      if (REG == "reg1"){
        riskMeasure <- risk_regime1[,RM]
      }else if (REG == "reg2"){
        riskMeasure <- risk_regime2[,RM]
      }else {
        print("something wrong here")
      }
      temp1 <- get(paste(SC,model,"reg",sep = "_"))
      serCol <-splitMe(temp1)[[REG]]
      plt <- formDataframe2(riskMeasure, serCol)
      plots[[paste(model,RM, sep = "_")]] <- ggplot (plt, aes(x=x_axis, y=y_axis))+
        geom_line(col = "red")+
        geom_text(aes(label = rownames(plt))) +
        ggtitle(paste("Model:", model, sep=" ")) +
        xlab(RM)+
        ylab(paste(SC," order =1", sep = " "))
    }
  }
  multiplot(plotlist = plots, cols = 4)
  dev.off()
}
plotplotplot ("Kappa", "reg2")
plotplotplot ("rho", "reg2")
plotplotplot ("Kappa", "reg1")
plotplotplot ("rho", "reg1")

# rm(model, models)
# rm(temp,temp1,temp2,temp3,temp4,temp5,temp6,temp7,temp8,temp9)
save(list = ls(all = TRUE), file= "plot_SC_RM_byReg.RData")
