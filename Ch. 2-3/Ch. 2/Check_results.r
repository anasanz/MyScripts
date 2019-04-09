


########################################################
####                  Check results                #####
######################################################## 

# Check results for important species

# ---- BUOED ----

setwd("S:/PhD/Second chapter/Data/Results/TRIM/3autoreg_simple")
load("HDS_BUOED.RData")
summary <- as.data.frame(out$summary)
summary[which(summary$Rhat > 1.1), ]

# EFFECT CO-VARIATES IN SIGMA
par(mfrow = c(1,2))
title("STONE CURLEW", line = -3, cex = 2, outer = TRUE)
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

# EFFECT CO-VARIATES IN LAMBDA: Maybe not necessary to put it if I have the hds plot of the trend? YES
par(mfrow = c(1,1))
plot(-21, xlim = c(1,9), ylim = c(-1,1), ylab = "lambda", xlab = "Year")
points(summary[grep("log.lambda.year", rownames(summary)), 1], pch = 16)
x <- c(1:9)
lci <- summary[grep("log.lambda.year", rownames(summary)), 3]
uci <- summary[grep("log.lambda.year", rownames(summary)), 7]
arrows(x,lci,x,uci, code=3, angle=90, length=0.04)

# ---- CACHL ----

setwd("S:/PhD/Second chapter/Data/Results/TRIM/3autoreg_simple")
load("HDS_CACHL.RData")
summary <- as.data.frame(out$summary)
summary[which(summary$Rhat > 1.1), ]

# EFFECT CO-VARIATES IN SIGMA
par(mfrow = c(1,2))
# Observer effect
plot(-21, xlim = c(1,15), ylim = c(1,6), ylab = "sigma", xlab = "Observer")
points(summary[grep("sig.obs", rownames(summary)), 1], pch = 16)
x <- c(1:15)
lci <- summary[grep("sig.obs", rownames(summary)), 3]
uci <- summary[grep("sig.obs", rownames(summary)), 7]
arrows(x,lci,x,uci, code=3, angle=90, length=0.04)

# Year effect
plot(-21, xlim = c(1,9), ylim = c(-2,2), ylab = "sigma", xlab = "Year")
points(summary[grep("log.sigma.year", rownames(summary)), 1], pch = 16)
x <- c(1:9)
lci <- summary[grep("log.sigma.year", rownames(summary)), 3]
uci <- summary[grep("log.sigma.year", rownames(summary)), 7]
arrows(x,lci,x,uci, code=3, angle=90, length=0.04)

title("Carduelis chloris ", line = -3, cex = 2, outer = TRUE)


# ---- CAINA ----

setwd("S:/PhD/Second chapter/Data/Results/TRIM/3autoreg_simple")
load("HDS_CAINA.RData")
summary <- as.data.frame(out$summary)
summary[which(summary$Rhat > 1.1), ]

# EFFECT CO-VARIATES IN SIGMA
par(mfrow = c(1,2))
# Observer effect
plot(-21, xlim = c(1,15), ylim = c(1,6), ylab = "sigma", xlab = "Observer")
points(summary[grep("sig.obs", rownames(summary)), 1], pch = 16)
x <- c(1:15)
lci <- summary[grep("sig.obs", rownames(summary)), 3]
uci <- summary[grep("sig.obs", rownames(summary)), 7]
arrows(x,lci,x,uci, code=3, angle=90, length=0.04)

# Year effect
plot(-21, xlim = c(1,9), ylim = c(-2,2), ylab = "sigma", xlab = "Year")
points(summary[grep("log.sigma.year", rownames(summary)), 1], pch = 16)
x <- c(1:9)
lci <- summary[grep("log.sigma.year", rownames(summary)), 3]
uci <- summary[grep("log.sigma.year", rownames(summary)), 7]
arrows(x,lci,x,uci, code=3, angle=90, length=0.04)

title("Carduelis cannabina", line = -3, cex = 2, outer = TRUE)

# ---- COOEN ----

setwd("S:/PhD/Second chapter/Data/Results/TRIM/3autoreg_simple")
load("HDS_COOEN.RData")
summary <- as.data.frame(out$summary)
summary[which(summary$Rhat > 1.1), ]

# EFFECT CO-VARIATES IN SIGMA
par(mfrow = c(1,2))
# Observer effect
plot(-21, xlim = c(1,15), ylim = c(2,13), ylab = "sigma", xlab = "Observer")
points(summary[grep("sig.obs", rownames(summary)), 1], pch = 16)
x <- c(1:15)
lci <- summary[grep("sig.obs", rownames(summary)), 3]
uci <- summary[grep("sig.obs", rownames(summary)), 7]
arrows(x,lci,x,uci, code=3, angle=90, length=0.04)

# Year effect
plot(-21, xlim = c(1,9), ylim = c(-6,6), ylab = "sigma", xlab = "Year")
points(summary[grep("log.sigma.year", rownames(summary)), 1], pch = 16)
x <- c(1:9)
lci <- summary[grep("log.sigma.year", rownames(summary)), 3]
uci <- summary[grep("log.sigma.year", rownames(summary)), 7]
arrows(x,lci,x,uci, code=3, angle=90, length=0.04)

title("Columba oenas", line = -3, cex = 2, outer = TRUE)

# ---- FATIN ----

setwd("S:/PhD/Second chapter/Data/Results/TRIM/3autoreg_simple")
load("HDS_FATIN.RData")
summary <- as.data.frame(out$summary)
summary[which(summary$Rhat > 1.1), ]

# EFFECT CO-VARIATES IN SIGMA
par(mfrow = c(1,2))
# Observer effect
plot(-21, xlim = c(1,15), ylim = c(min(summary$`2.5%`[4:18]),max(summary$`97.5%`[4:18])), ylab = "sigma", xlab = "Observer")
points(summary[grep("sig.obs", rownames(summary)), 1], pch = 16)
x <- c(1:15)
lci <- summary[grep("sig.obs", rownames(summary)), 3]
uci <- summary[grep("sig.obs", rownames(summary)), 7]
arrows(x,lci,x,uci, code=3, angle=90, length=0.04)

# Year effect
plot(-21, xlim = c(1,9), ylim = c(min(summary$`2.5%`[19:27]),max(summary$`97.5%`[19:27])), ylab = "sigma", xlab = "Year")
points(summary[grep("log.sigma.year", rownames(summary)), 1], pch = 16)
x <- c(1:9)
lci <- summary[grep("log.sigma.year", rownames(summary)), 3]
uci <- summary[grep("log.sigma.year", rownames(summary)), 7]
arrows(x,lci,x,uci, code=3, angle=90, length=0.04)

title("Falco tinnunculus", line = -3, cex = 2, outer = TRUE)

# ---- MECAL ----

setwd("S:/PhD/Second chapter/Data/Results/TRIM/3autoreg_simple")
load("HDS_MECAL.RData")
summary <- as.data.frame(out$summary)
summary[which(summary$Rhat > 1.1), ]

# EFFECT CO-VARIATES IN SIGMA
par(mfrow = c(1,2))
# Observer effect
plot(-21, xlim = c(1,15), ylim = c(min(summary$`2.5%`[4:18]),max(summary$`97.5%`[4:18])), ylab = "sigma", xlab = "Observer")
points(summary[grep("sig.obs", rownames(summary)), 1], pch = 16)
x <- c(1:15)
lci <- summary[grep("sig.obs", rownames(summary)), 3]
uci <- summary[grep("sig.obs", rownames(summary)), 7]
arrows(x,lci,x,uci, code=3, angle=90, length=0.04)

# Year effect
plot(-21, xlim = c(1,9), ylim = c(min(summary$`2.5%`[19:27]),max(summary$`97.5%`[19:27])), ylab = "sigma", xlab = "Year")
points(summary[grep("log.sigma.year", rownames(summary)), 1], pch = 16)
x <- c(1:9)
lci <- summary[grep("log.sigma.year", rownames(summary)), 3]
uci <- summary[grep("log.sigma.year", rownames(summary)), 7]
arrows(x,lci,x,uci, code=3, angle=90, length=0.04)

title("Melanocorypha calandra", line = -3, cex = 2, outer = TRUE)

# ---- MICAL ----

setwd("S:/PhD/Second chapter/Data/Results/TRIM/3autoreg_simple")
load("HDS_MICAL.RData")
summary <- as.data.frame(out$summary)
summary[which(summary$Rhat > 1.1), ]

# EFFECT CO-VARIATES IN SIGMA
par(mfrow = c(1,2))
# Observer effect
plot(-21, xlim = c(1,15), ylim = c(min(summary$`2.5%`[4:18]),max(summary$`97.5%`[4:18])), ylab = "sigma", xlab = "Observer")
points(summary[grep("sig.obs", rownames(summary)), 1], pch = 16)
x <- c(1:15)
lci <- summary[grep("sig.obs", rownames(summary)), 3]
uci <- summary[grep("sig.obs", rownames(summary)), 7]
arrows(x,lci,x,uci, code=3, angle=90, length=0.04)

# Year effect
plot(-21, xlim = c(1,9), ylim = c(min(summary$`2.5%`[19:27]),max(summary$`97.5%`[19:27])), ylab = "sigma", xlab = "Year")
points(summary[grep("log.sigma.year", rownames(summary)), 1], pch = 16)
x <- c(1:9)
lci <- summary[grep("log.sigma.year", rownames(summary)), 3]
uci <- summary[grep("log.sigma.year", rownames(summary)), 7]
arrows(x,lci,x,uci, code=3, angle=90, length=0.04)

title("Emberiza calandra MICAL", line = -3, cex = 2, outer = TRUE)

# ---- MIMIG ----

setwd("S:/PhD/Second chapter/Data/Results/TRIM/3autoreg_simple")
load("HDS_MIMIG.RData")
summary <- as.data.frame(out$summary)
summary[which(summary$Rhat > 1.1), ]

# EFFECT CO-VARIATES IN SIGMA
par(mfrow = c(1,2))
# Observer effect
plot(-21, xlim = c(1,15), ylim = c(min(summary$`2.5%`[4:18]),max(summary$`97.5%`[4:18])), ylab = "sigma", xlab = "Observer")
points(summary[grep("sig.obs", rownames(summary)), 1], pch = 16)
x <- c(1:15)
lci <- summary[grep("sig.obs", rownames(summary)), 3]
uci <- summary[grep("sig.obs", rownames(summary)), 7]
arrows(x,lci,x,uci, code=3, angle=90, length=0.04)

# Year effect
plot(-21, xlim = c(1,9), ylim = c(min(summary$`2.5%`[19:27]),max(summary$`97.5%`[19:27])), ylab = "sigma", xlab = "Year")
points(summary[grep("log.sigma.year", rownames(summary)), 1], pch = 16)
x <- c(1:9)
lci <- summary[grep("log.sigma.year", rownames(summary)), 3]
uci <- summary[grep("log.sigma.year", rownames(summary)), 7]
arrows(x,lci,x,uci, code=3, angle=90, length=0.04)

title("Milvus migrans", line = -3, cex = 2, outer = TRUE)

# ---- PADOM ----

setwd("S:/PhD/Second chapter/Data/Results/TRIM/3autoreg_simple")
load("HDS_PADOM.RData")
summary <- as.data.frame(out$summary)
summary[which(summary$Rhat > 1.1), ]

# EFFECT CO-VARIATES IN SIGMA
par(mfrow = c(1,2))
# Observer effect
plot(-21, xlim = c(1,15), ylim = c(min(summary$`2.5%`[4:18]),max(summary$`97.5%`[4:18])), ylab = "sigma", xlab = "Observer")
points(summary[grep("sig.obs", rownames(summary)), 1], pch = 16)
x <- c(1:15)
lci <- summary[grep("sig.obs", rownames(summary)), 3]
uci <- summary[grep("sig.obs", rownames(summary)), 7]
arrows(x,lci,x,uci, code=3, angle=90, length=0.04)

# Year effect
plot(-21, xlim = c(1,9), ylim = c(min(summary$`2.5%`[19:27]),max(summary$`97.5%`[19:27])), ylab = "sigma", xlab = "Year")
points(summary[grep("log.sigma.year", rownames(summary)), 1], pch = 16)
x <- c(1:9)
lci <- summary[grep("log.sigma.year", rownames(summary)), 3]
uci <- summary[grep("log.sigma.year", rownames(summary)), 7]
arrows(x,lci,x,uci, code=3, angle=90, length=0.04)

title("Passer domesticus", line = -3, cex = 2, outer = TRUE)

# ---- PAMAJ ----

setwd("S:/PhD/Second chapter/Data/Results/TRIM/3autoreg_simple")
load("HDS_PAMAJ.RData")
summary <- as.data.frame(out$summary)
summary[which(summary$Rhat > 1.1), ]

# EFFECT CO-VARIATES IN SIGMA
par(mfrow = c(1,2))
# Observer effect
plot(-21, xlim = c(1,15), ylim = c(min(summary$`2.5%`[4:18]),max(summary$`97.5%`[4:18])), ylab = "sigma", xlab = "Observer")
points(summary[grep("sig.obs", rownames(summary)), 1], pch = 16)
x <- c(1:15)
lci <- summary[grep("sig.obs", rownames(summary)), 3]
uci <- summary[grep("sig.obs", rownames(summary)), 7]
arrows(x,lci,x,uci, code=3, angle=90, length=0.04)

# Year effect
plot(-21, xlim = c(1,9), ylim = c(min(summary$`2.5%`[19:27]),max(summary$`97.5%`[19:27])), ylab = "sigma", xlab = "Year")
points(summary[grep("log.sigma.year", rownames(summary)), 1], pch = 16)
x <- c(1:9)
lci <- summary[grep("log.sigma.year", rownames(summary)), 3]
uci <- summary[grep("log.sigma.year", rownames(summary)), 7]
arrows(x,lci,x,uci, code=3, angle=90, length=0.04)

title("Parus major", line = -3, cex = 2, outer = TRUE)


# ---- PAMON ----

setwd("S:/PhD/Second chapter/Data/Results/TRIM/3autoreg_simple")
load("HDS_PAMON.RData")
summary <- as.data.frame(out$summary)
summary[which(summary$Rhat > 1.1), ]

# EFFECT CO-VARIATES IN SIGMA
par(mfrow = c(1,2))
# Observer effect
plot(-21, xlim = c(1,15), ylim = c(min(summary$`2.5%`[4:18]),max(summary$`97.5%`[4:18])), ylab = "sigma", xlab = "Observer")
points(summary[grep("sig.obs", rownames(summary)), 1], pch = 16)
x <- c(1:15)
lci <- summary[grep("sig.obs", rownames(summary)), 3]
uci <- summary[grep("sig.obs", rownames(summary)), 7]
arrows(x,lci,x,uci, code=3, angle=90, length=0.04)

# Year effect
plot(-21, xlim = c(1,9), ylim = c(min(summary$`2.5%`[19:27]),max(summary$`97.5%`[19:27])), ylab = "sigma", xlab = "Year")
points(summary[grep("log.sigma.year", rownames(summary)), 1], pch = 16)
x <- c(1:9)
lci <- summary[grep("log.sigma.year", rownames(summary)), 3]
uci <- summary[grep("log.sigma.year", rownames(summary)), 7]
arrows(x,lci,x,uci, code=3, angle=90, length=0.04)

title("Passer montanus", line = -3, cex = 2, outer = TRUE)

# ---- TERAX ----

setwd("S:/PhD/Second chapter/Data/Results/TRIM/3autoreg_simple")
load("HDS_TERAX.RData")
summary <- as.data.frame(out$summary)
summary[which(summary$Rhat > 1.1), ]

# EFFECT CO-VARIATES IN SIGMA
par(mfrow = c(1,2))
# Observer effect
plot(-21, xlim = c(1,15), ylim = c(min(summary$`2.5%`[4:18]),max(summary$`97.5%`[4:18])), ylab = "sigma", xlab = "Observer")
points(summary[grep("sig.obs", rownames(summary)), 1], pch = 16)
x <- c(1:15)
lci <- summary[grep("sig.obs", rownames(summary)), 3]
uci <- summary[grep("sig.obs", rownames(summary)), 7]
arrows(x,lci,x,uci, code=3, angle=90, length=0.04)

# Year effect
plot(-21, xlim = c(1,9), ylim = c(min(summary$`2.5%`[19:27]),max(summary$`97.5%`[19:27])), ylab = "sigma", xlab = "Year")
points(summary[grep("log.sigma.year", rownames(summary)), 1], pch = 16)
x <- c(1:9)
lci <- summary[grep("log.sigma.year", rownames(summary)), 3]
uci <- summary[grep("log.sigma.year", rownames(summary)), 7]
arrows(x,lci,x,uci, code=3, angle=90, length=0.04)

title("Tetrax tetrax", line = -3, cex = 2, outer = TRUE)


# ---- GACRI ----

setwd("S:/PhD/Second chapter/Data/Results/TRIM/3autoreg_simple")
load("HDS_GACRI.RData")
summary <- as.data.frame(out$summary)
summary[which(summary$Rhat > 1.1), ]

# EFFECT CO-VARIATES IN SIGMA
par(mfrow = c(1,2))
# Observer effect
plot(-21, xlim = c(1,15), ylim = c(min(summary$`2.5%`[4:18]),max(summary$`97.5%`[4:18])), ylab = "sigma", xlab = "Observer")
points(summary[grep("sig.obs", rownames(summary)), 1], pch = 16)
x <- c(1:15)
lci <- summary[grep("sig.obs", rownames(summary)), 3]
uci <- summary[grep("sig.obs", rownames(summary)), 7]
arrows(x,lci,x,uci, code=3, angle=90, length=0.04)

# Year effect
plot(-21, xlim = c(1,9), ylim = c(min(summary$`2.5%`[19:27]),max(summary$`97.5%`[19:27])), ylab = "sigma", xlab = "Year")
points(summary[grep("log.sigma.year", rownames(summary)), 1], pch = 16)
x <- c(1:9)
lci <- summary[grep("log.sigma.year", rownames(summary)), 3]
uci <- summary[grep("log.sigma.year", rownames(summary)), 7]
arrows(x,lci,x,uci, code=3, angle=90, length=0.04)

title("Tetrax tetrax", line = -3, cex = 2, outer = TRUE)

