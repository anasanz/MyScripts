
rm(list=ls())


library(rjags)
library(jagsUI)
library(dplyr)
library(data.table)


source("D:/PhD/MyScripts/Ch. 2-3/Ch. 3/Results/Functions/ProcessCodaOutput.R")
source("D:/PhD/MyScripts/Ch. 2-3/Ch. 3/Results/Functions/plot.violins3.r")
source("D:/PhD/MyScripts/Ch. 2-3/Ch. 3/Results/Functions/DoScale.r")

# Load species analyzed in the model

setwd("D:/PhD/Third chapter/Data")
d <- read.csv("DataDS_ch3_15_19_READY_FIXED_LAST_GASSP.csv")
bad_bp <- c("GACRI", "GATHE", "PADOM", "STSSP") # to remove all species with bad bp except MICAL and MECAL
d <- d[-which(d$Species %in% bad_bp), ]

sp <- as.character(unique(d$Species))
sp <- sort(sp)
nSpecies <- length(sp)

# Load the three chains
load("D:/PhD/Third chapter/Data/model/14.2.10_f/JagsOutFOR14.2.10_fa.RData")
outa <- out
load("D:/PhD/Third chapter/Data/model/14.2.10_f/JagsOutFOR14.2.10_fb.RData")
outb <- out
load("D:/PhD/Third chapter/Data/model/14.2.10_f/JagsOutFOR14.2.10_fc.RData")
outc <- out
class(outc)


out.list<- list()
out.list[[1]] <- as.mcmc(outa$samples[[1]])
out.list[[2]] <- as.mcmc(outb$samples[[1]])
out.list[[3]] <- as.mcmc(outc$samples[[1]])

out.list <- as.mcmc.list(out.list)

source("D:/PhD/MyScripts/Ch. 2-3/Ch. 3/Results/Functions/ProcessCodaOutput.R")

out <- ProcessCodaOutput(out.list)

# For legend

setwd("D:/PhD/Third chapter/Data")
leg <- read.csv("leg_species.csv", sep = ";")
leg <- leg[which(leg$codiEspecie %in% sp), c(1,2,6)]
leg <- arrange(leg, by = leg$codiEspecie)
colnames(leg)[1] <- "sp"



# Create data frame with species - coefficients together
sp.df <- data.frame(sp = sp, bCropdiv = out$colnames.sims[grep("bCropdiv", out$colnames.sims)],
                    bFieldsize = out$colnames.sims[grep("bFieldsize", out$colnames.sims)])


coeff <- c("bCropdiv", "bFieldsize")
names <- c("bCropdiv", "bFieldsize")



# Plot Fig3
setwd("D:/PhD/Third chapter/Data/Results_species/14.2/14.2.10_f")
pdf("14.2.10_f_ViolinWide_FS_CD.pdf", 6, 7)

par(mfrow = c(1,2),
    mar = c(1,7,1.5,0),
    oma = c(3,1,2,1),
    xpd = NA) 


# CD

# Order it by effect size of bCropdiv

v1 <- data.frame(out$mean[names(out$mean) %in% coeff[1]])
v2 <- data.frame(sp.df[ ,colnames(sp.df) %in% coeff[1]])
values <- cbind(v1,v2)
colnames(values)[1] <- "mean"
colnames(values)[2] <- coeff[1]
values$index <- rownames(values)
values2 <- left_join(sp.df,values)
values_sorted <- arrange(values2, mean)
values_sorted_legend <- left_join(values_sorted, leg, by = "sp")
sp_sorted <-  values_sorted_legend$English
coef_sorted <- values_sorted[,-c(1,4)]

# Plot

plot(10, ylim = c(1, nSpecies), 
     xlim = c(-2,1.5), 
     type ="n", yaxt="n", 
     #xaxt="n", 
     xlab = " ", ylab = "", main = "Crop diversity",
     cex.axis = 0.8)

target1 <- cbind(c(-4.3,-2.6,-2.6,-4.3), c(33.5,33.5,34.5,34.5)) # 1st column X : bottomleft, bottom right, topright, topleft.# 2nd column Y
polygon(target1[,1], target1[,2], col =  adjustcolor("grey52",alpha.f = 0.4), border = "white")

target2 <- cbind(c(-4.6,-2.6,-2.6,-4.6), c(32.5,32.5,33.5,33.5)) # 1st column X : bottomleft, bottom right, topright, topleft.# 2nd column Y
polygon(target2[,1], target2[,2], col =  adjustcolor("grey52",alpha.f = 0.4), border = "white")

target3 <- cbind(c(-4.3,-2.6,-2.6,-4.3), c(22.5,22.5,23.5,23.5)) # 1st column X : bottomleft, bottom right, topright, topleft.# 2nd column Y
polygon(target3[,1], target3[,2], col =  adjustcolor("grey52",alpha.f = 0.4), border = "white")

target4 <- cbind(c(-4.3,-2.6,-2.6,-4.3), c(21.5,21.5,22.5,22.5)) # 1st column X : bottomleft, bottom right, topright, topleft.# 2nd column Y
polygon(target4[,1], target4[,2], col =  adjustcolor("grey52",alpha.f = 0.4), border = "white")

target5 <- cbind(c(-5.7,-2.6,-2.6,-5.7), c(9.5,9.5,10.5,10.5)) # 1st column X : bottomleft, bottom right, topright, topleft.# 2nd column Y
polygon(target5[,1], target5[,2], col =  adjustcolor("grey52",alpha.f = 0.4), border = "white")

target6 <- cbind(c(-5.4,-2.6,-2.6,-5.4), c(8.5,8.5,9.5,9.5)) # 1st column X : bottomleft, bottom right, topright, topleft.# 2nd column Y
polygon(target6[,1], target6[,2], col =  adjustcolor("grey52",alpha.f = 0.4), border = "white")

target7 <- cbind(c(-5.3,-2.6,-2.6,-5.3), c(2.5,2.5,3.5,3.5)) # 1st column X : bottomleft, bottom right, topright, topleft.# 2nd column Y
polygon(target7[,1], target7[,2], col =  adjustcolor("grey52",alpha.f = 0.4), border = "white")

axis(2, c(1:nSpecies), labels = sp_sorted, las = 2, cex.axis = 0.6)
#axis(1, seq(-2, 1.5, by = 0.5), labels = seq(-2, 1.5, by = 0.5), cex.axis = 0.6, line = 0.5, pos = -0.45)

for(i in 1:nSpecies){
  plot.violins3(list(out$sims.list$bCropdiv[ ,as.numeric(coef_sorted$index[i])]),
                x = i,
                at = i,
                violin.width = 0.3,
                plot.ci = 0.95,
                col = "grey52",
                add = T,
                alpha = 0.3,
                scale.width = FALSE,
                border.col = "grey52",
                horizontal = TRUE) }

segments(0, -1, 0, 38.4, col = "red")
#mtext("Crop diversity", line = 0, side = 3, cex = 0.6) 

# For indicating significant effects
over0<- as.numeric(data.table::between(0, out$q2.5$bCropdiv, out$q97.5$bCropdiv))
sig_TFM <- ifelse(over0 == 1, " ", "*")
sig_TFM <- as.data.frame(cbind(sp, sig_TFM))
sig_TFM_sp <- left_join(values_sorted_legend, sig_TFM, by = "sp")

text(-2, sort(seq(1:37)), labels = sig_TFM_sp$sig_TFM, cex = 0.8)


# FS

# Order it by effect size of bFieldsize

v1 <- data.frame(out$mean[names(out$mean) %in% coeff[2]])
v2 <- data.frame(sp.df[ ,colnames(sp.df) %in% coeff[2]])
values <- cbind(v1,v2)
colnames(values)[1] <- "mean"
colnames(values)[2] <- coeff[2]
values$index <- rownames(values)
values2 <- left_join(sp.df,values)
values_sorted <- arrange(values2, mean)
values_sorted_legend <- left_join(values_sorted, leg, by = "sp")
sp_sorted <-  values_sorted_legend$English
coef_sorted <- values_sorted[,-c(1,4)]

# Plot

plot(10, ylim = c(1, nSpecies), 
     xlim = c(-3, 1.5), 
     type ="n", yaxt="n", 
     #xaxt="n", 
     xlab = " ", ylab = "", main = "Field size",
     cex.axis = 0.8)

target1 <- cbind(c(-7.8,-3.8,-3.8,-7.8), c(36.5,36.5,37.5,37.5)) # 1st column X : bottomleft, bottom right, topright, topleft.# 2nd column Y
polygon(target1[,1], target1[,2], col =  adjustcolor("grey52",alpha.f = 0.4), border = "white")

target2 <- cbind(c(-7.4,-3.8,-3.8,-7.4), c(32.5,32.5,33.5,33.5)) # 1st column X : bottomleft, bottom right, topright, topleft.# 2nd column Y
polygon(target2[,1], target2[,2], col =  adjustcolor("grey52",alpha.f = 0.4), border = "white")

target3 <- cbind(c(-6,-3.8,-3.8,-6), c(31.5,31.5,32.5,32.5)) # 1st column X : bottomleft, bottom right, topright, topleft.# 2nd column Y
polygon(target3[,1], target3[,2], col =  adjustcolor("grey52",alpha.f = 0.4), border = "white")

target4 <- cbind(c(-6.3,-3.8,-3.8,-6.3), c(29.5,29.5,30.5,30.5)) # 1st column X : bottomleft, bottom right, topright, topleft.# 2nd column Y
polygon(target4[,1], target4[,2], col =  adjustcolor("grey52",alpha.f = 0.4), border = "white")

target5 <- cbind(c(-7.6,-3.8,-3.8,-7.6), c(26.5,26.5,27.5,27.5)) # 1st column X : bottomleft, bottom right, topright, topleft.# 2nd column Y
polygon(target5[,1], target5[,2], col =  adjustcolor("grey52",alpha.f = 0.4), border = "white")

target6 <- cbind(c(-6,-3.8,-3.8,-6), c(22.5,22.5,23.5,23.5)) # 1st column X : bottomleft, bottom right, topright, topleft.# 2nd column Y
polygon(target6[,1], target6[,2], col =  adjustcolor("grey52",alpha.f = 0.4), border = "white")

target7 <- cbind(c(-6,-3.8,-3.8,-6), c(7.5,7.5,8.5,8.5)) # 1st column X : bottomleft, bottom right, topright, topleft.# 2nd column Y
polygon(target7[,1], target7[,2], col =  adjustcolor("grey52",alpha.f = 0.4), border = "white")


axis(2, c(1:nSpecies), labels = sp_sorted, las = 2, cex.axis = 0.6)
#axis(1, seq(-2, 1.5, by = 0.5), labels = seq(-2, 1.5, by = 0.5), cex.axis = 0.6, pos = -0.45, line = 1)


for(i in 1:nSpecies){
  plot.violins3(list(out$sims.list$bFieldsize[ ,as.numeric(coef_sorted$index[i])]),
                x = i,
                at = i,
                violin.width = 0.3,
                plot.ci = 0.95,
                col = "grey52",
                add = T,
                alpha = 0.3,
                scale.width = FALSE,
                border.col = "grey52",
                horizontal = TRUE) }
segments(0, -1, 0, 38.4, col = "red")
#mtext("Field size", line = 0.5, side = 3, cex = 0.8) 

over0<- as.numeric(data.table::between(0, out$q2.5$bFieldsize, out$q97.5$bFieldsize))
sig_TFM <- ifelse(over0 == 1, " ", "*")
sig_TFM <- as.data.frame(cbind(sp, sig_TFM))
sig_TFM_sp <- left_join(values_sorted_legend, sig_TFM, by = "sp")

text(-3, sort(seq(1:37)), labels = sig_TFM_sp$sig_TFM, cex = 0.8)

mtext("Beta", line = 1.5, side = 1, cex = 1, outer = TRUE) 



dev.off()
