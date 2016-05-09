SPX = CED3mon2yr[["SPX"]]

SPX_price = assetData[["SPX"]]

SPX$Date[10001: 10564] = NA
SPX$values[10001: 10564] = NA


png('test1.png', width = 600, height = 500)
plot(SPX_price$Date[10565:16607], SPX_price$PX_LAST[10565:16607], 
     type='l', 
     ylab = "SPX Price", xlab = "Date", ylim = c(0, 2100),
     yaxt='n', lwd=0.75, font = 1)
axis(2, pretty(c(0, 2100)))
par(new=T)
plot(SPX$Date[10000:16042], 100*SPX$values[10000:16042], 
     ylim=c(0, 100),col='red3', 
     type='l', lwd=0.75,
     axes=F, ylab='', xlab = '')
axis(4, pretty(c(0, 100)), col='red3')
mtext("CED(%)", side=4, line=3)
dev.off()

################################

RMZ = CED3mon2yr[["RMZ"]]

RMZ_price = assetData[["RMZ"]]

RMZ$Date[2: 566] = NA
RMZ$values[2: 566] = NA

CED_RMZ = rep(0, length(RMZ_price$Date))
CED_RMZ[2: 566] = NA
CED_RMZ[567:length(RMZ_price$Date)] = RMZ$values

png('test2.png', width = 600, height = 500)
plot(RMZ_price$Date, RMZ_price$PX_LAST, 
     type='l', 
     ylab = "SPX Price", xlab = "Date", ylim = c(0, 1300),
     yaxt='n', lwd=0.75, font = 1)
axis(2, pretty(c(0, 1300)))
par(new=T)
plot(RMZ_price$Date, 100*CED_RMZ, 
     ylim=c(0, 100),col='red3', 
     type='l', lwd=0.75,
     axes=F, ylab='', xlab = '')
axis(4, pretty(c(0, 100)), col='red3')
mtext("CED(%)", side=4, line=3)
dev.off()

length(RMZ_price$Date)
length(CED_RMZ)
