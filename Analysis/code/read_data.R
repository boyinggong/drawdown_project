
###################################
###### read in the data file ######
###################################

# assetsList <- c("AGG", "HYG", "TIP",
#                 "BCOM", "BUHY", "G0O1", "LTP5TRUU", "MXEA", "MXEF", "RAY", "RMZ", "SPX", "USGG10YR")

assetsList <- c("AGG", "HYG", "TIP",
                "BCOM", "G0O1", "MXEA", "MXEF", "RAY", "RMZ", "SPX", "USGG10YR")

setAs("character","myDate", function(from) as.Date(from, format="%m/%d/%y"))
assetData = list()
for (asset in c("AGG", "HYG", "TIP")){
  assetData[[asset]] = read.csv(paste("Analysis/csv_data/", asset, "_US_Equity.csv", sep = ''), 
                            colClasses=c("myDate", "numeric"))
} # US equity
for (asset in c("BCOM", "G0O1", "MXEA", "MXEF", "RAY", "RMZ", "SPX", "USGG10YR")){
  assetData[[asset]] = read.csv(paste("Analysis/csv_data/", asset, "_Index.csv", sep = ''), 
                            colClasses=c("myDate", "numeric"))
} # index

# revise year from 20XX to 19XX
library(lubridate)
index = which(year(assetData$SPX$Date) > 2020)
year(assetData$SPX$Date[index]) = year(assetData$SPX$Date[index]) - 100
index = which(year(assetData$USGG10YR$Date) > 2020)
year(assetData$USGG10YR$Date[index]) = year(assetData$USGG10YR$Date[index]) - 100

# delete some data based on monthly or weekly calculation
assetData$G0O1 <- assetData$G0O1[which(assetData$G0O1$Date == as.Date("1992-04-01")):nrow(assetData$G0O1), ]
rownames(assetData$G0O1) <- NULL

assetData$MXEA <- assetData$MXEA[which(assetData$MXEA$Date == as.Date("1972-01-03")):nrow(assetData$MXEA), ]
rownames(assetData$MXEA) <- NULL

# assetData$LTP5TRUU <- assetData$LTP5TRUU[which(assetData$LTP5TRUU$Date == 
#                                                  as.Date("2010-06-03")):nrow(assetData$LTP5TRUU), ]
# rownames(assetData$LTP5TRUU) <- NULL

# check if there were missing values
for (i in assetsList){
  val <- assetData[[i]]
  print(i)
  print(which(val$PX_LAST == 0))    
}

###################################
###### calculate the returns ######
###################################

# annual returns are based on 252 bussiness days

for (i in assetsList){
  val <- assetData[[i]]
  len <- nrow(val)
  # daily returns
  retrn_dl <- c(0,(val$PX_LAST[2:len]-val$PX_LAST[1:(len-1)])/val$PX_LAST[1:(len-1)])
  # annalized returnss
  retrn_an <- (1+retrn_dl)^252-1
  # logarithmic return
  retrn_log <- c(0, log(val$PX_LAST[2:len]/val$PX_LAST[1:(len-1)]))
  assetData[[i]] = cbind(val, retrn_dl, retrn_an, retrn_log)
}

# delete the first row with no former day information
# thus no return data
for (asset in names(assetData)){
  assetData[[asset]] = assetData[[asset]][c(-1), ]
}

# for (asset in assetData){
#   print(head(asset))
# }

#################################
#### delete the unuseful data ###
#################################

variables = ls()
variables = variables[-which(variables %in% c("assetData"))]
rm(list = variables)
rm(variables)

