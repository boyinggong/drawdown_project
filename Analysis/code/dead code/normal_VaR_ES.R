assetsList


meanAsset = rep(0, 11)
for (i in 1:11){
  meanAsset[i] = mean(get(assetsList[i])$retrn_dl)
}

sdAsset = rep(0, 11)
for (i in 1:11){
  sdAsset[i] = sd(get(assetsList[i])$retrn_dl)
}

VaRnorm = rep(0, 11)
for (i in 1:11){
  VaRnorm[i] = meanAsset[i] + qnorm(0.01) * sdAsset[i]
}
VaRnorm * 100


normdt = rnorm(100000000)
mean(normdt[normdt > qnorm(0.99)])
# 0.9  1.754958
# 0.95 2.062626
# 0.99 2.665093
rm(normdt)

ESnorm = rep(0, 11)
for (i in 1:11){
  ESnorm[i] = meanAsset[i] + 2.665093 * sdAsset[i]
}
ESnorm * 100


qnorm(0.05, mean = mean(AGG$retrn_dl), sd = sd(AGG$retrn_dl))

sd(AGG$retrn_dl)

length(AGG$retrn_dl)

sum(AGG$retrn_dl < -0.004)

156 / 3087


sqrt(var(AGG$retrn_dl))



quantile(AGG$retrn_dl, p = 0.05)

?qnorm
