

format(round(resES[, "0.9"]*100, 4), nsmall = 4)
a <- paste(format(round(resES[, "0.9"]*100, 4), nsmall = 4), 
           format(round(resES[, "0.95"]*100, 4), nsmall = 4), sep = ' $ ')
b <- paste(a, collapse = "\\\\ \\hline \n")
cat(b)

################ Statistical summary ################

indexSub = 1:11

a0 <- paste(assetsList[indexSub],
           format(round(statSmmr[indexSub, 1], 3), nsmall = 3),  
           format(round(statSmmr[indexSub, 2], 3), nsmall = 3), 
           format(round(statSmmr[indexSub, 3], 2), nsmall = 2), 
           format(round(statSmmr[indexSub, 4], 2), nsmall = 2), sep = ' & ')
b0 <- paste(a0, collapse = "\\\\ \n")
cat(b0)

################ VaR & ES ################

a <- paste(assetsList[indexSub],
           format(round(resVaR[indexSub, "0.9"]*100, 2), nsmall = 2),  
           format(round(resVaR[indexSub, "0.95"]*100, 2), nsmall = 2), 
           format(round(resVaR[indexSub, "0.99"]*100, 2), nsmall = 2), 
           format(round(resES[indexSub, "0.9"]*100, 2), nsmall = 2), 
           format(round(resES[indexSub, "0.95"]*100, 2), nsmall = 2),
           format(round(resES[indexSub, "0.99"]*100, 2), nsmall = 2), sep = ' & ')
b <- paste(a, collapse = "\\\\ \n")
cat(b)

################ CED ################

CED3mon <- read.csv("../results/63_CED.csv")
CED6mon <- read.csv("../results/126_CED.csv")
CED1yr <- read.csv("../results/252_CED.csv")
CED2yr <- read.csv("../results/504_CED.csv")


aa <- paste(assetsList,
           format(round(-CED3mon[, "X0.9"]*100, 2), nsmall = 2),  
           format(round(-CED3mon[, "X0.95"]*100, 2), nsmall = 2), 
           format(round(-CED3mon[, "X0.99"]*100, 2), nsmall = 2),
           format(round(-CED6mon[, "X0.9"]*100, 2), nsmall = 2),  
           format(round(-CED6mon[, "X0.95"]*100, 2), nsmall = 2), 
           format(round(-CED6mon[, "X0.99"]*100, 2), nsmall = 2), sep = ' & ')
bb <- paste(aa, collapse = "\\\\ \n")
cat(bb)

aa <- paste(assetsList,
            format(round(-CED1yr[, "X0.9"]*100, 2), nsmall = 2),  
            format(round(-CED1yr[, "X0.95"]*100, 2), nsmall = 2), 
            format(round(-CED1yr[, "X0.99"]*100, 2), nsmall = 2),
            format(round(-CED2yr[, "X0.9"]*100, 2), nsmall = 2),  
            format(round(-CED2yr[, "X0.95"]*100, 2), nsmall = 2), 
            format(round(-CED2yr[, "X0.99"]*100, 2), nsmall = 2), sep = ' & ')
bb <- paste(aa, collapse = "\\\\ \n")
cat(bb)

################ Statistical summary ################
########### of regime dependent mdoels ##############

a0 <- paste(assetsList,
            # format(round(statSmmr_regime1[, 1], 3), nsmall = 3),  
            format(round(statSmmr_regime1[, 2], 3), nsmall = 3),  
            format(round(statSmmr_regime2[, 2], 3), nsmall = 3), 
            format(round(statSmmr_regime1[, 3], 2), nsmall = 2), 
            format(round(statSmmr_regime2[, 3], 2), nsmall = 2), 
            format(round(statSmmr_regime1[, 4], 2), nsmall = 2), 
            format(round(statSmmr_regime2[, 4], 2), nsmall = 2),
            # format(round(statSmmr_regime2[, 1], 3), nsmall = 3), 
            sep = ' & ')
b0 <- paste(a0, collapse = "\\\\ \n")
cat(b0)



###############################

a0 <- paste(names(assetData),
            format(round(corr_df[, 6], 2), nsmall = 2),
            format(round(corr_df[, 3], 2), nsmall = 2),
            format(round(corr_df[, 5], 2), nsmall = 2),
            format(round(corr_df[, 2], 2), nsmall = 2),
            format(round(corr_df[, 4], 2), nsmall = 2),
            format(round(corr_df[, 1], 2), nsmall = 2),
            sep = ' & ')
b0 <- paste(a0, collapse = "\\\\ \n")
cat(b0)


a0 <- paste(names(assetData),
            format(round(corr_CED_df[, 1], 2), nsmall = 2),
            format(round(corr_CED_df[, 2], 2), nsmall = 2),
            format(round(corr_CED_df[, 3], 2), nsmall = 2),
            sep = ' & ')
b0 <- paste(a0, collapse = "\\\\ \n")
cat(b0)

a0 <- paste(names(assetData),
            format(round(serialCor_df[, 1], 2), nsmall = 2),
            format(round(serialCor_df[, 2], 2), nsmall = 2),
            format(round(serialCor_df[, 3], 2), nsmall = 2),
            format(round(serialCor_df[, 4], 2), nsmall = 2),
            sep = ' & ')
b0 <- paste(a0, collapse = "\\\\ \n")
cat(b0)

