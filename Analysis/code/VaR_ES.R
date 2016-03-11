
source("read_data.R")

#################
### functions ###
#################

VaR <- function(R, p = 0.95){
  return(-quantile(R, probs = 1-p))
}

ES <- function(R, p = 0.95){
  -mean(R[R < quantile(R, probs = 1-p)])
}

###################
### calculation ###
###################

lvs <- c(.9, .95, .99)
resVaR <- matrix(rep(0, length(lvs)*length(assetData)), ncol=length(lvs))
colnames(resVaR) <- lvs
rownames(resVaR) <- names(assetData)

resES <- matrix(rep(0, length(lvs)*length(assetData)), ncol=length(lvs))
colnames(resES) <- lvs
rownames(resES) <- names(assetData)

for (asset in names(assetData)){
  resVaR[asset, ] <- sapply(lvs, function(lv)VaR(assetData[[asset]]$retrn_dl, p=lv))
  resES[asset, ] <- sapply(lvs, function(lv)ES(assetData[[asset]]$retrn_dl, p=lv))
}

write.csv(resVaR, file = "../tables/summary_daily/resVaR.csv")
write.csv(resES, file = "../tables/summary_daily/resES.csv")

#################################
#### delete the unuseful data ###
#################################

variables = ls()
variables = variables[-which(variables %in% c("assetData"))]
rm(list = variables)
rm(variables)
