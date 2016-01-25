setwd("/Users/jorothygong/Desktop/drawdown/Analysis/csv_data/")
library("PerformanceAnalytics")
library(ggplot2)
library(grid)
library(gridExtra)

######################### read in the data file #########################

assetsList <- c("AGG", "HYG", "TIP",
                "BCOM", "BUHY", "G0O1", "LTP5TRUU", "MXEA", "MXEF", "RAY", "RMZ", "SPX", "USGG10YR")

setAs("character","myDate", function(from) as.Date(from, format="%m/%d/%y") )
for (i in 1:3){
  assign(assetsList[i], 
         read.csv(paste(assetsList[i], "_US_Equity.csv", sep = ''), 
                  colClasses=c("myDate", "numeric")))
} # US equity * 3
for (i in 4:13){
  assign(assetsList[i], 
         read.csv(paste(assetsList[i], "_Index.csv", sep = ''), 
                  colClasses=c("myDate", "numeric")))
} # index * 10

# revise year from 20XX to 19XX
library(lubridate)
index = which(year(SPX$Date) > 2020)
year(SPX$Date[index]) = year(SPX$Date[index]) - 100
# plot(SPX)
index = which(year(USGG10YR$Date) > 2020)
year(USGG10YR$Date[index]) = year(USGG10YR$Date[index]) - 100
#plot(USGG10YR)

G0O1 <- G0O1[which(G0O1$Date == as.Date("1992-04-01")):nrow(G0O1), ]
rownames(G0O1) <- NULL
LTP5TRUU <- LTP5TRUU[which(LTP5TRUU$Date == as.Date("2010-06-03")):nrow(LTP5TRUU), ]
rownames(LTP5TRUU) <- NULL

######################### basic information #########################

for (i in assetsList){
  val <- get(i)
  print(i)
  print(paste(range(val$Date), collapse = " to "))
}

######################### calculate the returns #########################

# annual returns are based on 252 bussiness days

for (i in assetsList){
  val <- get(i)
  len <- nrow(val)
  # daily returns
  retrn_dl <- c(0,(val$PX_LAST[2:len]-val$PX_LAST[1:(len-1)])/val$PX_LAST[1:(len-1)])
  # annalized returnss
  retrn_an <- (1+retrn_dl)^252-1
  assign(i, cbind(val, retrn_dl, retrn_an)) 
}

for (i in assetsList){
  val <- get(i)
  print(i)
  print(which(val$PX_LAST == 0))    # check if there were missing values
}

######################### plot the returns #########################

par(mfrow=c(3,5))

for (i in assetsList){
  val <- get(i)
  plot(val$Date[2:length(val$Date)], val$retrn_dl[2:length(val$Date)],
       main = i,
       xlab = "Date",
       ylab = "Return")
}

dev.off()

######################### calculate the Sharpe Ratio, #########################
################### standard deviation, skewness, kurtosis ####################

statSmmr <- matrix(rep(0, 4*length(assetsList)), nrow=length(assetsList))
rownames(statSmmr) <- assetsList
colnames(statSmmr) <- c("SR","sd","skewness","kurtosis")

for (i in assetsList){
  val <- get(i)
  dt <- as.data.frame(val$retrn_dl)
  rownames(dt) <- val$Date
  # statSmmr[i, 1] <- SharpeRatio(dt, Rf = 0)
  statSmmr[i, 2] <- sd(val$retrn_dl)
  statSmmr[i, 3] <- skewness(val$retrn_dl) # method?
  statSmmr[i, 4] <- kurtosis(val$retrn_dl)
  Rf = 0
  statSmmr[i, 1] <- (mean(val$retrn_dl) - Rf)/ statSmmr[i, 2]
} 

write.csv(statSmmr, file = "../results/statSmmr.csv")

######################### calculate the VaR & ES #########################

VaR <- function(R, p = 0.95){
  quantile(R, probs = 1-p)
}

ES <- function(R, p = 0.95){
  mean(R[R < quantile(R, probs = 1-p)])
}

microbenchmark(VaR(AGG$retrn_dl, p = 0.95), 
               PerformanceAnalytics::VaR(AGG$retrn_dl, p = 0.95))

lvs <- c(.9, .95, .99)
resVaR <- matrix(rep(0, length(lvs)*length(assetsList)), ncol=length(lvs))
colnames(resVaR) <- lvs
rownames(resVaR) <- assetsList

resES <- matrix(rep(0, length(lvs)*length(assetsList)), ncol=length(lvs))
colnames(resES) <- lvs
rownames(resES) <- assetsList

count <- 1

for (i in assetsList){
  val <- get(i)
  resVaR[count, ] <- sapply(lvs, function(lv)VaR(val$retrn_dl, p=lv))
  resES[count, ] <- sapply(lvs, function(lv)ES(val$retrn_dl, p=lv))
  count = count + 1
}

write.csv(resVaR, file = "../results/resVaR.csv")
write.csv(resES, file = "../results/resES.csv")

######################### calculate the CED #########################

# 3 month: 63 days
# 6 month: 126 days
# 1 year: 252 days
# 2 years: 504 days
# 5 years: 1260 days
# calculation are based on annualized returns

lvs <- c(.9, .95, .99)
prds <- c(63, 126, 252, 504, 1260)
names(prds) <- c("mon3", "mon6", "yr1", "yr2", "yr5")

calcCED <- function(val, prd){
  sapply(2:(nrow(val)-prd+1), function(x){
    dt <- as.data.frame(val$retrn_dl[x:(x+prd-1)])
    rownames(dt) <- val$Date[x:(x+prd-1)] 
    maxDrawdown(dt)
  })
}

calcCED(MXEF, 1260)

for (prd in prds){
  resCEDs <- matrix(rep(0, length(assetsList)*length(lvs)), ncol = length(lvs))
  resCEDs <- as.data.frame(resCEDs)
  rownames(resCEDs) <- assetsList
  colnames(resCEDs) <- lvs
  
  for (i in assetsList){
    val <- get(i)
    mxd <- calcCED(val, prd)
    resCEDs[i, ] <- sapply(lvs, function(lv)ES(-mxd, p=lv))
    print(i)
  }
  write.csv(resCEDs, file = paste("../results/", prd, "_CED.csv", sep = ''))
}

#### plot CED

CED3mon <- read.csv("../results/63_CED.csv")
CED6mon <- read.csv("../results/126_CED.csv")
CED1yr <- read.csv("../results/252_CED.csv")
CED2yr <- read.csv("../results/504_CED.csv")
CED5yr <- read.csv("../results/1260_CED.csv")


count = 1
png(paste("../results/CED.png", sep = ''), width = 1600, height = 1600)
plots = list()
for (i in assetsList){
  plt <- data.frame(CED = unlist(c(CED3mon[count, 2:4], CED6mon[count, 2:4], CED1yr[count, 2:4], 
                            CED2yr[count, 2:4], CED5yr[count, 2:4])),
                    prd = as.factor(rep(names(prds), each = 3)),
                    confLevel = rep(c("p = 0.9", "p = 0.95", "p = 0.99"), 5))
  plots[[i]] <- ggplot(plt, aes(x = prd, y = -CED, group = confLevel, color = confLevel)) +
    geom_line() + geom_point() +
    ggtitle(i) + ylim(0, 0.75) +
    labs(y = paste(FUN, "(%)"))+
    labs(x = "Date")
  count = count+1
}
multiplot(plotlist = plots, cols = 4)
dev.off()



######################### calculate the rolling stats #########################

# 3 month: 63 days
# 6 month: 126 days
# 1 year: 252 days
# 2 years: 504 days
# 5 years: 1260 days
# calculation are based on annualized returns

lvs <- c(.9, .95, .99)
prds <- c(63, 126, 252, 504, 1260)
names(prds) <- c("3mon", "6mon", "1yr", "2yr", "5yr")

calcRolling <- function(val, lv, prd, FUN, ...){
  res <- sapply(2:(nrow(val)-prd+1), function(x){
    dt <- as.data.frame(val$retrn_dl[x:(x+prd-1)])
    rownames(dt) <- val$Date[x:(x+prd-1)] 
    do.call(FUN, list(dt[, 1], lv))
  })
}

################   plot   ################


windw = "1yr"
FUN = "ES"

windw = "6mon"
FUN = "ES"

windw = "6mon"
FUN = "VaR"

windw = "1yr"
FUN = "VaR"

assign(paste(FUN, windw, sep = ''), 
       lapply(assetsList, function(i)calcRolling(get(i), lv = 0.95, prd = prds[windw], FUN)))
assign(paste(FUN, windw, "_date",sep = ''),
       lapply(assetsList, function(i)get(i)$Date[(1+prds[windw]):(nrow(get(i)))]))
testplot <- data.frame(ES = get(paste(FUN, windw, sep = ''))[[1]], Date = get(paste(FUN, windw, "_date",sep = ''))[[1]])

count = 1
png(paste("../results/", FUN, windw, ".png", sep = ''), width = 1600, height = 1600)
plots = list()
for (i in assetsList){
  plt <- data.frame(y_axis = get(paste(FUN, windw, sep = ''))[[count]]*100,
                    x_axis = get(paste(FUN, windw, "_date",sep = ''))[[count]])
  plots[[i]] <- ggplot(plt, aes(x=x_axis, y=y_axis)) +
    geom_line() +
    ggtitle(i) +
    labs(y = paste(FUN, "(%)"))+
    labs(x = "Date")
  count = count+1
}
multiplot(plotlist = plots, cols = 4)
dev.off()

###################  rolling variance   ###################

windw = "6mon"
FUN = "var"

windw = "1yr"
FUN = "var"

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
testplot <- data.frame(ES = get(paste(FUN, windw, sep = ''))[[1]], Date = get(paste(FUN, windw, "_date",sep = ''))[[1]])

count = 1
png(paste("../results/variance", windw, ".png", sep = ''), width = 1600, height = 1600)
plots = list()
for (i in assetsList){
  plt <- data.frame(y_axis = get(paste(FUN, windw, sep = ''))[[count]],
                    x_axis = get(paste(FUN, windw, "_date",sep = ''))[[count]])
  plots[[i]] <- ggplot(plt, aes(x=x_axis, y=y_axis)) +
    geom_line() +
    ggtitle(i) +
    labs(y = paste("variance"))
    labs(x = "Date")
  count = count+1
}
multiplot(plotlist = plots, cols = 4)
dev.off()









# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  require(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}


