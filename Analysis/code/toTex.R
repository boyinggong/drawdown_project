

format(round(resES[, "0.9"]*100, 4), nsmall = 4)
a <- paste(format(round(resES[, "0.9"]*100, 4), nsmall = 4), 
           format(round(resES[, "0.95"]*100, 4), nsmall = 4), sep = ' $ ')
b <- paste(a, collapse = "\\\\ \\hline \n")
cat(b)

################ Statistical summary ################

a0 <- paste(assetsList,
           format(round(statSmmr[, 1], 4), nsmall = 4),  
           format(round(statSmmr[, 2], 4), nsmall = 4), 
           format(round(statSmmr[, 3], 4), nsmall = 4), 
           format(round(statSmmr[, 4], 4), nsmall = 4), sep = ' & ')
b0 <- paste(a0, collapse = "\\\\ \n")
cat(b0)

################ VaR & ES ################

a <- paste(assetsList,
           format(round(resVaR[, "0.9"]*100, 4), nsmall = 4),  
           format(round(resVaR[, "0.95"]*100, 4), nsmall = 4), 
           format(round(resVaR[, "0.99"]*100, 4), nsmall = 4), 
           format(round(resES[, "0.9"]*100, 4), nsmall = 4), 
           format(round(resES[, "0.95"]*100, 4), nsmall = 4),
           format(round(resES[, "0.99"]*100, 4), nsmall = 4), sep = ' & ')
b <- paste(a, collapse = "\\\\ \n")
cat(b)

################ CED ################

aa <- paste(assetsList,
           format(round(resCEDs[, "0.9"]*100, 4), nsmall = 4),  
           format(round(resCEDs[, "0.95"]*100, 4), nsmall = 4), 
           format(round(resCEDs[, "0.99"]*100, 4), nsmall = 4), sep = ' & ')
bb <- paste(aa, collapse = "\\\\ \n")
cat(bb)


