##################################################
#### calculate the correlation between assets ####
##################################################

calc_corr <- function(measure1, measure2){
  len_asset_list <- length(measure1)
  corr_vec <- rep(0, len_asset_list)
  names(corr_vec) = names(assetData)
  for (asset in names(assetData)){
    corr_vec[asset] = cor(measure1[[asset]]$values, measure2[[asset]]$values)
  }
  return(corr_vec)
}

corr_df = cbind(
  calc_corr(VaR6mon, ES6mon),
  calc_corr(VaR6mon, volatility6mon),
  calc_corr(VaR6mon, maxDrawdown6mon),
  calc_corr(ES6mon, volatility6mon),
  calc_corr(ES6mon, maxDrawdown6mon),
  calc_corr(volatility6mon, maxDrawdown6mon))

corr_CED_df = cbind(
  calc_corr(CED3mon2yr, volatility3mon2yr),
  calc_corr(CED3mon2yr, VaR3mon2yr),
  calc_corr(CED3mon2yr, ES3mon2yr))

