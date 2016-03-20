#######################
#### calculate the correlation between assets ####
#######################

calc_corr <- function(measure1, measure2, period1, period2){
  len_asset_list <- length(get(paste(measure1, period1, sep = '')))
  corr_vec <- rep(0, len_asset_list)
  for (i in 1:len_asset_list){
    corr_vec[i] = cor(get(paste(measure1, period1, sep = ''))[[i]], 
                      get(paste(measure2, period2, sep = ''))[[i]])
  }
  return(corr_vec)
}

# calc_corr("VaR", "ES", "1yr", "1yr")[c(1:4, 6:11)]
calc_corr("VaR", "ES", "6mon", "6mon")[c(1:4, 6:11)]
# calc_corr("var", "ES", "1yr", "1yr")[c(1:4, 6:11)]
calc_corr("var", "ES", "6mon", "6mon")[c(1:4, 6:11)]
# calc_corr("VaR", "var", "1yr", "1yr")[c(1:4, 6:11)]
calc_corr("VaR", "var", "6mon", "6mon")[c(1:4, 6:11)]

# mean(calc_corr("VaR", "ES", "1yr", "1yr")[c(1:4, 6:11)])
mean(calc_corr("VaR", "ES", "6mon", "6mon")[c(1:4, 6:11)])
# mean(calc_corr("var", "ES", "1yr", "1yr")[c(1:4, 6:11)])
mean(calc_corr("var", "ES", "6mon", "6mon")[c(1:4, 6:11)])
# mean(calc_corr("VaR", "var", "1yr", "1yr")[c(1:4, 6:11)])
mean(calc_corr("VaR", "var", "6mon", "6mon")[c(1:4, 6:11)])

# calc_corr("VaR", "maxDrawdown", "1yr", "yr1")[c(1:4, 6:11)]
calc_corr("VaR", "maxDrawdown", "6mon", "mon6")[c(1:4, 6:11)]
# calc_corr("var", "maxDrawdown", "1yr", "yr1")[c(1:4, 6:11)]
calc_corr("var", "maxDrawdown", "6mon", "mon6")[c(1:4, 6:11)]
# calc_corr("ES", "maxDrawdown", "1yr", "yr1")[c(1:4, 6:11)]
calc_corr("ES", "maxDrawdown", "6mon", "mon6")[c(1:4, 6:11)]

# mean(calc_corr("VaR", "maxDrawdown", "1yr", "yr1")[c(1:4, 6:11)])
mean(calc_corr("VaR", "maxDrawdown", "6mon", "mon6")[c(1:4, 6:11)])
# mean(calc_corr("var", "maxDrawdown", "1yr", "yr1")[c(1:4, 6:11)])
mean(calc_corr("var", "maxDrawdown", "6mon", "mon6")[c(1:4, 6:11)])
# mean(calc_corr("ES", "maxDrawdown", "1yr", "yr1")[c(1:4, 6:11)])
mean(calc_corr("ES", "maxDrawdown", "6mon", "mon6")[c(1:4, 6:11)])


mean(calc_corr("VaR", "calc_auto_corr", "6mon", "6mon")[c(1:4, 6:11)])
mean(calc_corr("var", "calc_auto_corr", "6mon", "6mon")[c(1:4, 6:11)])
mean(calc_corr("ES", "calc_auto_corr", "6mon", "6mon")[c(1:4, 6:11)])
mean(calc_corr("maxDrawdown", "calc_auto_corr","mon6", "6mon")[c(1:4, 6:11)])

mean(calc_corr("VaR", "CED", "3mon5yr", "_3mon_5yr")[c(1:4, 6:11)])
mean(calc_corr("ES", "CED", "3mon5yr", "_3mon_5yr")[c(1:4, 6:11)])
mean(calc_corr("var", "CED", "3mon5yr", "_3mon_5yr")[c(1:4, 6:11)])
mean(calc_corr("calc_auto_corr", "CED", "3mon5yr", "_3mon_5yr")[c(1:4, 6:11)])


########## rolling auto-correlation ###########

series_cor = rep(0, 11)
for (i in 1:11){
  series_cor[i] = acf(get(assetsList[i])$retrn_dl,
                   type = "correlation",
                   plot = TRUE)$acf[2]
}

series_cor

windw = "6mon"
FUN = "calc_auto_corr"

calc_auto_corr <- function(x){
  acf(x, type = "correlation", plot = FALSE)$acf[2]
}

calcRolling <- function(val, prd, FUN, ...){
  res <- sapply(2:(nrow(val)-prd+1), function(x){
    dt <- as.data.frame(val$retrn_dl[x:(x+prd-1)])
    rownames(dt) <- val$Date[x:(x+prd-1)] 
    do.call(FUN, list(dt[, 1]))
  })
}

assign(paste(FUN, windw, sep = ''), 
       lapply(assetsList, function(i)calcRolling(get(i), prd = prds[windw], FUN)))
assign(paste(FUN, windw, "_date",sep = ''),
       lapply(assetsList, function(i)get(i)$Date[(1+prds[windw]):(nrow(get(i)))]))


#######################
#### ks.test of asset returns ####
#######################

ks.test_res = rep(0, length(assetsList))
for (i in 1:length(assetsList)){
  x = get(assetsList[i])$retrn_dl
  ks.test_res[i] = ks.boot(x, rnorm(10000, mean(x), sd(x)))$ks.boot.pvalue
}

library(Matching)

ks.test(rnorm(1000), rnorm(1000))


i = 2
x = get(assetsList[i])$retrn_dl
wahttttt = ks.boot(x, rnorm(10000, mean(x), sd(x)))
wahttttt$ks.boot.pvalue

