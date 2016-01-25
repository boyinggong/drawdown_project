

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
b <- paste(a, collapse = "\\\\ \n"
           )
cat(b)

################ CED ################

CED3mon <- read.csv("../results/63_CED.csv")
CED6mon <- read.csv("../results/126_CED.csv")
CED1yr <- read.csv("../results/252_CED.csv")
CED2yr <- read.csv("../results/504_CED.csv")


aa <- paste(assetsList,
           format(round(-CED3mon[, "X0.9"]*100, 4), nsmall = 4),  
           format(round(-CED3mon[, "X0.95"]*100, 4), nsmall = 4), 
           format(round(-CED3mon[, "X0.99"]*100, 4), nsmall = 4),
           format(round(-CED6mon[, "X0.9"]*100, 4), nsmall = 4),  
           format(round(-CED6mon[, "X0.95"]*100, 4), nsmall = 4), 
           format(round(-CED6mon[, "X0.99"]*100, 4), nsmall = 4), sep = ' & ')
bb <- paste(aa, collapse = "\\\\ \n")
cat(bb)

aa <- paste(assetsList,
            format(round(-CED1yr[, "X0.9"]*100, 4), nsmall = 4),  
            format(round(-CED1yr[, "X0.95"]*100, 4), nsmall = 4), 
            format(round(-CED1yr[, "X0.99"]*100, 4), nsmall = 4),
            format(round(-CED2yr[, "X0.9"]*100, 4), nsmall = 4),  
            format(round(-CED2yr[, "X0.95"]*100, 4), nsmall = 4), 
            format(round(-CED2yr[, "X0.99"]*100, 4), nsmall = 4), sep = ' & ')
bb <- paste(aa, collapse = "\\\\ \n")
cat(bb)


