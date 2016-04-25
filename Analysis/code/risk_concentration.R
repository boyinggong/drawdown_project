source("Analysis/code/read_data.R")
source("Analysis/code/risk_diagostics_function.R")

library(PerformanceAnalytics)

max_drawdown = function(r, denominator = "start"){
  log_r = log(r + 1)
  sum = 0
  this_sum = 0
  Ri = 1
  Rj = 1
  this_Ri = 1
  this_Rj = 1
  for (i in 1:length(r)){
    this_sum = this_sum + log_r[i]
    this_Rj = i
    if (this_sum<sum){
      sum = this_sum
      Ri = this_Ri
      Rj = this_Rj
    }else if (this_sum>0){
      this_sum = 0
      this_Ri = i + 1
    }
  }
  if (denominator == "start"){
    max_drawdown = exp(sum(log_r[1:(Ri-1)])) - exp(sum(log_r[1:Rj]))
    return(list(max_drawdown=max_drawdown, Ri=Ri, Rj=Rj))
  }else if (denominator == "peak"){
    max_drawdown = -(exp(sum)-1)
  }
}

drawdown_contribution = function(r, weight){
#   browser()
  ## r: data frame or matrix, each column represents the return series of one asset class
  r = as.matrix(r)
  if (!all.equal(sum(weight), 1)) stop("weight must sum to one")
  price = apply(r+1, 2, cumprod)%*%weight
  r_all = (price - c(1, price[-length(price)]))/c(1, price[-length(price)])
  max_d = max_drawdown(r_all)
  if (max_d$Ri == 1 & max_d$Rj == 1){
    contribution = c(SPX = 0, RMZ = 0)
  } else if (max_d$Ri ==1 & max_d$Rj > 1 ){
    contribution = (1-apply(r[1:max_d$Rj, ]+1, 2, prod)) * weight
  } else if (max_d$Ri ==2 ){
    contribution = ((r[1:(max_d$Ri-1), ]+1)-apply(r[1:max_d$Rj, ]+1, 2, prod)) * weight
  }else {
    contribution = (apply(r[1:(max_d$Ri-1), ]+1, 2, prod)-apply(r[1:max_d$Rj, ]+1, 2, prod)) * weight
  }
  return(list(max_drawdown = max_d$max_drawdown, contribution = contribution))
}

###################
# Test data frame #
###################

SPX = assetData$SPX$retrn_dl[which(assetData$SPX$Date == "2007-01-03"):
                               (which(assetData$SPX$Date == "2008-01-03")-1)]
RMZ = assetData$RMZ$retrn_dl[which(assetData$RMZ$Date == "2007-01-03"):
                               (which(assetData$RMZ$Date == "2008-01-03")-1)]
test_r = data.frame(SPX=SPX, RMZ=RMZ)

max_drawdown(test_r$SPX)
test_contr = drawdown_contribution(r=test_r, weight=c(0.4, 0.6))
c(test_contr$max_drawdown,test_contr$contribution)
## meeee!!!!!!!!!!!!!!!!!!!!!!
####################### calc all drawdowns in a given window ###################################

drawdown_contribution_reform <- function(r, weight){
  dc <- drawdown_contribution(r, weight)
  ret <- c(max_drawdown = dc$max_drawdown, dc$contribution)
  return(ret)
}

# test_2 = data.frame(Date =SPX$Date ,SPX=SPX$retrn_dl, RMZ=RMZ$retrn_dl)

calcRolling_rc <- function(combo_df, prd, FUN, ...){
  res <- sapply(1:(nrow(combo_df)-prd+1) , function(x){
    dt <- as.data.frame(combo_df[(x:(x+prd-1)), (2:3)])
    rownames(dt) <- combo_df$Date[x:(x+prd-1)] 
    do.call(FUN, list(dt, ...))
  })
  ret = as.data.frame(cbind(Date = combo_df$Date[prd:nrow(combo_df)],t(res)))
  ret[,"Date"] <- as.Date(ret[,"Date"])
  return(ret)
}


calcCED_rc <- function(dd_df, prd, p = 0.9){
  res <- lapply(1:(nrow(dd_df)-prd+1), function(x){
    sub_df <- dd_df[x:(x+prd-1),]
    ret <- sub_df[sub_df$max_drawdown > quantile(sub_df$max_drawdown, probs = p),]
    return(ret)
  })
  E_ret <- lapply(res,function(i){
    nn = nrow(i)
    temp <- cbind(Date = i[nn,1], t(apply(i[,2:4],2,mean)))
    return(temp)
  } )

  final <- do.call(rbind, E_ret)
  assets <- colnames(final[,-c(1,2)])
  summs <- apply(final[,-c(1,2)],1, sum)
  for (asset in assets){
    final <- cbind(final, final[,asset]/summs) 
  }
  final <- as.data.frame(final)
  final[,"Date"] = as.Date(final[,"Date"])
  colnames(final) = c("Date", "CED", assets, paste(assets,".ratio",sep = ""))
  return(final)
}

# xin3 <- calcCED_rc(xin2,prds["3mon2yr"] )



### risk contributions of VaR, ES and volatility
### Reference: 
### ES http://braverock.com/brian/R/PerformanceAnalytics/html/ES.html
### VaR http://braverock.com/brian/R/PerformanceAnalytics/html/VaR.html
### volatility page3 https://cran.r-project.org/web/packages/PortRisk/PortRisk.pdf




