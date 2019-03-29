


########################################################
####                  Check results                #####
######################################################## 

# Check results for 4 important species: BUOED, MECAL, FATIN, PAMAJ

setwd("S:/PhD/Second chapter/Data/Results/TRIM/3autoreg_simple")
load("HDS_BUOED.RData")
summary <- as.data.frame(out$summary)
summary[which(summary$Rhat > 1.1), ]

# EFFECT CO-VARIATES IN SIGMA
# Observer effect
plot(-21, xlim = c(1,15), ylim = c(3,6), ylab = "sigma", xlab = "Observer")
points(summary[grep("sig.obs", rownames(summary)), 1], pch = 16)
x <- c(1:15)
lci <- summary[grep("sig.obs", rownames(summary)), 3]
uci <- summary[grep("sig.obs", rownames(summary)), 7]
arrows(x,lci,x,uci, code=3, angle=90, length=0.04)

# Year effect
plot(-21, xlim = c(1,9), ylim = c(-1,1), ylab = "sigma", xlab = "Year")
points(summary[grep("log.sigma.year", rownames(summary)), 1], pch = 16)
x <- c(1:9)
lci <- summary[grep("log.sigma.year", rownames(summary)), 3]
uci <- summary[grep("log.sigma.year", rownames(summary)), 7]
arrows(x,lci,x,uci, code=3, angle=90, length=0.04)       

# EFFECT CO-VARIATES IN LAMBDA: Maybe not necessary to put it if I have the hds plot of the trend?
plot(-21, xlim = c(1,9), ylim = c(-1,1), ylab = "lambda", xlab = "Year")
points(summary[grep("log.lambda.year", rownames(summary)), 1], pch = 16)
x <- c(1:9)
lci <- summary[grep("log.lambda.year", rownames(summary)), 3]
uci <- summary[grep("log.lambda.year", rownames(summary)), 7]
arrows(x,lci,x,uci, code=3, angle=90, length=0.04)
