
#############################
#### calculate the risks ####
## for regime1 and regime2 ##
#############################

regimes_boundary = matrix(0, nr = length(assetData), nc = 4)
rownames(regimes_boundary) = names(assetData)
risk_regime1 = regimes_boundary
names(risk_regime1) = c("VaR", "ES", "Volatility", "CED")
risk_regime2 = regimes_boundary
names(risk_regime2) = c("VaR", "ES", "Volatility", "CED")

assign(paste(FUN = "maxDrawdown", windw = "1mon", sep = ''), 
       getMaxDdDataFrame(windw = "1mon", Data = assetData))

for (asset in names(assetData)){
  a = which(assetData[[asset]]$Date == assetData[["RMZ"]]$Date[596]) + 31
  b = a + 500
  c = which(assetData[[asset]]$Date == assetData[["RMZ"]]$Date[2015]) + 31
  d = c + 500
  print(asset)
  regimes_boundary[asset, ] = c(a, b, c, d)
  risk_regime1[asset, ] = c(VaR(assetData[[asset]]$retrn_dl[a:b], p = 0.95),
                            ES(assetData[[asset]]$retrn_dl[a:b], p = 0.95),
                            volatility(assetData[[asset]]$retrn_dl[a:b]),
                            calcCED(maxDrawdown1mon[[asset]]$values[(a+21):b], prd = 480))
  risk_regime2[asset, ] = c(VaR(assetData[[asset]]$retrn_dl[c:d], p = 0.95),
                            ES(assetData[[asset]]$retrn_dl[c:d], p = 0.95),
                            volatility(assetData[[asset]]$retrn_dl[c:d]),
                            calcCED(maxDrawdown1mon[[asset]]$values[(c+21):d], prd = 480))
  if (asset %in% c("MXEF", "G0O1", "RMZ")){
    print(sum(models[[asset]]["Fit"]["smoProb"][, 1][a:b] < 0.5))
    print(sum(models[[asset]]["Fit"]["smoProb"][, 1][c:d] > 0.5))
  }else{
    print(sum(models[[asset]]["Fit"]["smoProb"][, 1][a:b] > 0.5))
    print(sum(models[[asset]]["Fit"]["smoProb"][, 1][c:d] < 0.5))
  }
}








