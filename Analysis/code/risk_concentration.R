source("read_data.R")
# source("risk_diagostics_function.R")
# install.packages("reshape")
# install.packages("PortRisk")
# install.packages("cowplot")
install.packages("~/R_package/graph_1.30.0.tar.gz", repos = NULL, type="source")S
remove.packages("PortRisk")

library(PerformanceAnalytics)
library(ggplot2)
library(reshape)
library(PortRisk)
library(cowplot)


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
    contribution = c(0, 0)
  }else if (max_d$Ri ==1 & max_d$Rj > 1 ){
    contribution = (1-apply(r[1:max_d$Rj, ]+1, 2, prod)) * weight
  }else if (max_d$Ri ==2 ){
    contribution = ((r[1:(max_d$Ri-1), ]+1)-apply(r[1:max_d$Rj, ]+1, 2, prod)) * weight
  }else {
    contribution = (apply(r[1:(max_d$Ri-1), ]+1, 2, prod)-apply(r[1:max_d$Rj, ]+1, 2, prod)) * weight
  }
  return(list(max_drawdown = max_d$max_drawdown, contribution = contribution))
}

###################
# Test data frame #
###################

Date = assetData$SPX$Date[which(assetData$SPX$Date == "2006-01-03"):
                                (which(assetData$SPX$Date == "2015-12-31"))]
SPX = assetData$SPX$retrn_dl[which(assetData$SPX$Date == "2006-01-03"):
                               (which(assetData$SPX$Date == "2015-12-31"))]
RMZ = assetData$RMZ$retrn_dl[which(assetData$RMZ$Date == "2006-01-03"):
                               (which(assetData$RMZ$Date == "2015-12-31"))]
test_r = data.frame(Date=Date, SPX=SPX, RMZ=RMZ)
rownames(test_r) = Date

max_drawdown(test_r$SPX)
test_contr = drawdown_contribution(r=test_r[, 2:3], weight=c(0.4, 0.6))
c(test_contr$max_drawdown,test_contr$contribution)

####################### calc all drawdowns in a given window ###################################

drawdown_contribution_reform <- function(r, weight){
#   browser()
  dc <- drawdown_contribution(r, weight)
  ret <- c(max_drawdown = dc$max_drawdown, dc$contribution)
  return(ret)
}

# drawdown_contribution_reform(r=test_r[,2:3], weight=c(0.4, 0.6))
# test_2 = data.frame(Date =SPX$Date ,SPX=SPX$retrn_dl, RMZ=RMZ$retrn_dl)

calcRolling_rc <- function(combo_df, prd, FUN, ...){
#   browser()
  res <- sapply(1:(nrow(combo_df)-prd+1) , function(x){
    dt <- as.data.frame(combo_df[(x:(x+prd-1)), (2:ncol(combo_df))])
    rownames(dt) <- combo_df$Date[x:(x+prd-1)] 
    unlist(do.call(FUN, list(dt, ...)))
  })
  ret = as.data.frame(cbind(Date = combo_df$Date[prd:nrow(combo_df)],t(res)))
  ret[,"Date"] <- as.Date(ret[,"Date"], origin = "1970-01-01")
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
  final[,"Date"] = as.Date(final[,"Date"], origin = "1970-01-01")
  colnames(final) = c("Date", "CED", assets, paste(assets,".contribution",sep = ""))
  return(final)
}

########################################################
### calculate risk contribution of four risk measures ##
########################################################

w = c(0.5, 0.5)
w = c(0.5, 0.5)
w = c(0.5, 0.5)

## CED
dd_df = calcRolling_rc(combo_df = test_r, prd = 63, 
                       FUN = drawdown_contribution_reform, weight = w)

CED_rc <- calcCED_rc(dd_df, prd = 252)
CED_total = calcCED_rc(dd_df, prd = 2454)
# 63 2454
# 126 2391
CED_plot = ggplot(melt(CED_rc[, c(1, 2, 5, 6)], id = c("Date")), 
                  aes(x= Date, y = value, group = variable))+
  geom_line()+
  facet_grid(variable~., scales = "free")+
  theme_light()

## ES
ES_rc = function(data, weights, p){
  res = ES(data, weights = weights, p = p, portfolio_method = "component")
  return(c(ES = res$MES, mrc = res$contribution, contribution = res$pct_contrib_MES))
}
ES_df = calcRolling_rc(combo_df = test_r, prd = 63+252-1, FUN = ES_rc, 
                       p = 0.9, weights = w)
ES_total = calcRolling_rc(combo_df = test_r, prd = 2517, FUN = ES_rc, 
                       p = 0.9, weights = w)
ES_plot = ggplot(melt(ES_df[, c(1, 2, 5, 6)], id = c("Date")), 
                 aes(x= Date, y = value, group = variable))+
  geom_line()+
  facet_grid(variable~., scales = "free")+
  theme_light()

## VaR
VaR_rc = function(data, weights, p){
  res = VaR(data, weights = weights, p = p, portfolio_method = "component")
  return(c(VaR = res$MVaR, mrc = res$contribution, contribution = res$pct_contrib_MVaR))
}
VaR_df = calcRolling_rc(combo_df = test_r, prd = 63+252-1, FUN = VaR_rc, 
                       p = 0.9, weights = w)
VaR_total = calcRolling_rc(combo_df = test_r, prd = 2517, FUN = VaR_rc, 
                          p = 0.9, weights = w)
VaR_plot = ggplot(melt(VaR_df[, c(1, 2, 5, 6)], id = c("Date")), 
                 aes(x= Date, y = value, group = variable))+
  geom_line()+
  facet_grid(variable~., scales = "free")+
  theme_light()

## volatility
volatility_rc = function(data, weights){
  volatility = portvol(colnames(data), weights = weights,
                       start = rownames(data)[1], end = rownames(data)[nrow(data)], data = data)
  mrc = mctr(colnames(data), weights = weights,
             start = rownames(data)[1], end = rownames(data)[nrow(data)], data = data)*weights
  contribution = mrc/sum(mrc)
  c(volatility = volatility, mrc = mrc, contribution = contribution)
}
# volatility_rc(test_r, weights = c(0.4, 0.6))

vol_df = calcRolling_rc(combo_df = test_r, prd = 63+252-1, FUN = volatility_rc, 
                        weights = w)
vol_total = calcRolling_rc(combo_df = test_r, prd = 2517, FUN = volatility_rc, 
                          weights = w)
volatility_plot = ggplot(melt(vol_df[, c(1, 2, 5, 6)], id = c("Date")), 
                  aes(x= Date, y = value, group = variable))+
  geom_line()+
  facet_grid(variable~., scales = "free")+
  theme_light()

png("../figures/risk_contribution/SPX_RMZ_55.png", width = 800, height = 800)
plot_grid(CED_plot, ES_plot, VaR_plot, volatility_plot, ncol = 2, align = "v")
dev.off()


### risk contributions of VaR, ES and volatility
### Reference: 
### ES http://braverock.com/brian/R/PerformanceAnalytics/html/ES.html
### VaR http://braverock.com/brian/R/PerformanceAnalytics/html/VaR.html
### volatility page3 https://cran.r-project.org/web/packages/PortRisk/PortRisk.pdf

w = c(0.5, 0.5)
w = c(0.6, 0.4)
w = c(0.7, 0.3)

dd_df = calcRolling_rc(combo_df = test_r, prd = 63, 
                       FUN = drawdown_contribution_reform, weight = w)
CED_total = calcCED_rc(dd_df, prd = 2454)
ES_total = calcRolling_rc(combo_df = test_r, prd = 2517, FUN = ES_rc, 
                          p = 0.9, weights = w)
VaR_total = calcRolling_rc(combo_df = test_r, prd = 2517, FUN = VaR_rc, 
                           p = 0.9, weights = w)
vol_total = calcRolling_rc(combo_df = test_r, prd = 2517, FUN = volatility_rc, 
                           weights = w)

overall_rc = as.data.frame(matrix(0, nc = 3, nr = 12))
colnames(overall_rc) = c("weights", "Measures", "Values")
overall_rc[, 1] = c(rep("50/50", 4), rep("60/40", 4), rep("70/30", 4))
overall_rc[, 2] = rep(c("CED", "ES", "VaR", "Volatility"), 3)
overall_rc[1:4, 3] = c(CED_total[1, "SPX.contribution"], ES_total$contribution.SPX,
                       VaR_total$contribution.SPX, vol_total$contribution.SPX)
overall_rc[5:8, 3] = c(CED_total[1, "SPX.contribution"], ES_total$contribution.SPX,
                       VaR_total$contribution.SPX, vol_total$contribution.SPX)
overall_rc[9:12, 3] = c(CED_total[1, "SPX.contribution"], ES_total$contribution.SPX,
                       VaR_total$contribution.SPX, vol_total$contribution.SPX)

png("../figures/risk_contribution/overall_rc.png", width = 600, height = 400)
ggplot(overall_rc, aes(x = Measures, y = Values, group = weights, fill = Measures)) + 
  geom_bar(stat="identity") + ylim(0, 1) + 
  theme_light() + facet_grid(.~weights) + scale_fill_hue(c=45, l=80) +
  xlab("Risk measures") + ylab("Risk contribution")
dev.off()


###################
### risk parity ###
###################

dd_df = calcRolling_rc(combo_df = test_r, prd = 63, 
                       FUN = drawdown_contribution_reform, weight = c(0.6128, 0.3872))
CED_total = calcCED_rc(dd_df, prd = 2454)
CED_total
# 63 2454
# 126 2391

## CED 3 month parity 0.6128, 0.3872
CED_parity = c(0.6128, 0.3872)

ES_total = calcRolling_rc(combo_df = test_r, prd = 2517, FUN = ES_rc, 
                          p = 0.9, weights = c(0.66671, 0.33329))
ES_total
ES_parity = c(0.66671, 0.33329)
## ES parity 0.66671, 0.33329

VaR_total = calcRolling_rc(combo_df = test_r, prd = 2517, FUN = VaR_rc, 
                           p = 0.9, weights = c(0.4577, 0.5423))
VaR_total
VaR_parity = c(0.4577, 0.5423)
## VaR parity 

vol_total = calcRolling_rc(combo_df = test_r, prd = 2517, FUN = volatility_rc, 
                           weights = c(0.64286, 0.35714))
vol_total
volatility_parity = c(0.64286, 0.35714)
## volatility parity 0.64291, 0.35717

risk_parity = matrix(0, nc = 4, nr = 4)
colnames(risk_parity) = c("CED_parity", "ES_parity", "VaR_parity", "volatility_parity")
rownames(risk_parity) = c("CED", "ES", "VaR", "volatility")

####################################### CED parity

dd_df = calcRolling_rc(combo_df = test_r, prd = 63, 
                       FUN = drawdown_contribution_reform, weight = CED_parity)
CED_total = calcCED_rc(dd_df, prd = 2454)
CED_total

ES_total = calcRolling_rc(combo_df = test_r, prd = 2517, FUN = ES_rc, 
                          p = 0.9, weights = CED_parity)

VaR_total = calcRolling_rc(combo_df = test_r, prd = 2517, FUN = VaR_rc, 
                          p = 0.9, weights = CED_parity)

vol_total = calcRolling_rc(combo_df = test_r, prd = 2517, FUN = volatility_rc, 
                           weights = CED_parity)

risk_parity[, "CED_parity"] = c(CED_total[1, "SPX.contribution"], ES_total$contribution.SPX,
                                VaR_total$contribution.SPX, vol_total$contribution.SPX)

####################################### ES parity

dd_df = calcRolling_rc(combo_df = test_r, prd = 63, 
                       FUN = drawdown_contribution_reform, weight = ES_parity)
CED_total = calcCED_rc(dd_df, prd = 2454)
CED_total

ES_total = calcRolling_rc(combo_df = test_r, prd = 2517, FUN = ES_rc, 
                          p = 0.9, weights = ES_parity)
ES_total

VaR_total = calcRolling_rc(combo_df = test_r, prd = 2517, FUN = VaR_rc, 
                           p = 0.9, weights = ES_parity)

vol_total = calcRolling_rc(combo_df = test_r, prd = 2517, FUN = volatility_rc, 
                           weights = ES_parity)
vol_total

risk_parity[, "ES_parity"] = c(CED_total[1, "SPX.contribution"], ES_total$contribution.SPX,
                               VaR_total$contribution.SPX, vol_total$contribution.SPX)

####################################### VaR parity

dd_df = calcRolling_rc(combo_df = test_r, prd = 63, 
                       FUN = drawdown_contribution_reform, weight = VaR_parity)
CED_total = calcCED_rc(dd_df, prd = 2454)
CED_total

ES_total = calcRolling_rc(combo_df = test_r, prd = 2517, FUN = ES_rc, 
                          p = 0.9, weights = VaR_parity)
ES_total

VaR_total = calcRolling_rc(combo_df = test_r, prd = 2517, FUN = VaR_rc, 
                           p = 0.9, weights = VaR_parity)

vol_total = calcRolling_rc(combo_df = test_r, prd = 2517, FUN = volatility_rc, 
                           weights = VaR_parity)
vol_total

risk_parity[, "VaR_parity"] = c(CED_total[1, "SPX.contribution"], ES_total$contribution.SPX,
                                VaR_total$contribution.SPX, vol_total$contribution.SPX)

####################################### Volatility parity

dd_df = calcRolling_rc(combo_df = test_r, prd = 63, 
                       FUN = drawdown_contribution_reform, weight = volatility_parity)
CED_total = calcCED_rc(dd_df, prd = 2454)
CED_total

ES_total = calcRolling_rc(combo_df = test_r, prd = 2517, FUN = ES_rc, 
                          p = 0.9, weights = volatility_parity)
ES_total

VaR_total = calcRolling_rc(combo_df = test_r, prd = 2517, FUN = VaR_rc, 
                           p = 0.9, weights = volatility_parity)

vol_total = calcRolling_rc(combo_df = test_r, prd = 2517, FUN = volatility_rc, 
                           weights = volatility_parity)
vol_total

risk_parity[, "volatility_parity"] = c(CED_total[1, "SPX.contribution"], ES_total$contribution.SPX,
                                       VaR_total$contribution.SPX, vol_total$contribution.SPX)

###########
##  PLOT ##
###########

melt_parity_df = melt(risk_parity[c(1, 2, 4), c(1, 2, 4)], 
                      measure.vars = c("CED_parity", 
                                       "VaR_parity", "volatility_parity"))
colnames(melt_parity_df)[1] = "Measures"

png("../figures/risk_contribution/risk_parity.png", width = 600, height = 400)
ggplot(melt_parity_df, aes(x = Measures, y = value, group = X2, fill = Measures)) + 
  geom_bar(stat="identity") + ylim(0, 1) + 
  geom_hline(yintercept=0.5, color="grey1", linetype="dashed") +
  theme_light() + facet_grid(.~X2) + scale_fill_hue(c=45, l=80) +
  xlab("Risk measures") + ylab("Risk contribution")
dev.off()







