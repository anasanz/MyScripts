
rm(list=ls())


library(rjags)
library(jagsUI)
library(dplyr)

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
outall <- out$sims.list # 3 chains together 
df.outall <- as.data.frame(outall)
names(df.outall)
# Create data frame with species - coefficients together
sp.df <- data.frame(sp = sp, mu_a1.sp = out$colnames.sims[grep("mu_a1.sp", out$colnames.sims)],
                    b.a2 = out$colnames.sims[grep("b.a2", out$colnames.sims)],
                    b.a3 = out$colnames.sims[grep("b.a3", out$colnames.sims)])

# Process samples

coeff <- c("mu_a1.sp", "b.a2", "b.a3")
names <- c("b.SG", "b.AES", "b.GREEN")

# For legend

setwd("D:/PhD/Third chapter/Data")
leg <- read.csv("leg_species.csv", sep = ";")
leg <- leg[which(leg$codiEspecie %in% sp), c(1:3)]
leg <- arrange(leg, by = leg$codiEspecie)
colnames(leg)[1] <- "sp"

# ---- Wide ----

# Order it by effect size of SG

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
coef_sorted <- values_sorted[,-c(1,5)]

out$q97.5
# Plot
setwd("D:/PhD/Third chapter/Data/Results_species/14.2/14.2.10_f")
pdf(paste("14.2.10_f.ViolinWide.pdf"), 6,7)

par(mfrow = c(1,3),
    mar = c(1,1.5,1.5,0),
    oma = c(3,8.5,2,1),
    xpd = NA) 

# SG
plot(10, ylim = c(1, nSpecies), 
     xlim = c(-1,1.55), 
     type ="n", yaxt="n", xlab = " ", ylab = "", main = "TFM")

target1 <- cbind(c(-3.4,-1.3,-1.3,-3.4), c(36.5,36.5,37.5,37.5)) # 1st column X : bottomleft, bottom right, topright, topleft.# 2nd column Y
polygon(target1[,1], target1[,2], col =  adjustcolor("grey52",alpha.f = 0.4), border = "white")

target2 <- cbind(c(-2.7,-1.3,-1.3,-2.7), c(34.5,34.5,35.5,35.5)) # 1st column X : bottomleft, bottom right, topright, topleft.# 2nd column Y
polygon(target2[,1], target2[,2], col =  adjustcolor("grey52",alpha.f = 0.4), border = "white")

target3 <- cbind(c(-2.9,-1.3,-1.3,-2.9), c(33.5,33.5,34.5,34.5)) # 1st column X : bottomleft, bottom right, topright, topleft.# 2nd column Y
polygon(target3[,1], target3[,2], col =  adjustcolor("grey52",alpha.f = 0.4), border = "white")

target4 <- cbind(c(-2.7,-1.3,-1.3,-2.7), c(32.5,32.5,33.5,33.5)) # 1st column X : bottomleft, bottom right, topright, topleft.# 2nd column Y
polygon(target4[,1], target4[,2], col =  adjustcolor("grey52",alpha.f = 0.4), border = "white")

target5 <- cbind(c(-3.4,-1.3,-1.3,-3.4), c(22.5,22.5,23.5,23.5)) # 1st column X : bottomleft, bottom right, topright, topleft.# 2nd column Y
polygon(target5[,1], target5[,2], col =  adjustcolor("grey52",alpha.f = 0.4), border = "white")

target6 <- cbind(c(-3.6,-1.3,-1.3,-3.6), c(18.5,18.5,19.5,19.5)) # 1st column X : bottomleft, bottom right, topright, topleft.# 2nd column Y
polygon(target6[,1], target6[,2], col =  adjustcolor("grey52",alpha.f = 0.4), border = "white")

target7 <- cbind(c(-2.6,-1.3,-1.3,-2.6), c(8.5,8.5,9.5,9.5)) # 1st column X : bottomleft, bottom right, topright, topleft.# 2nd column Y
polygon(target7[,1], target7[,2], col =  adjustcolor("grey52",alpha.f = 0.4), border = "white")

axis(2, c(1:nSpecies), labels = sp_sorted, las = 2, cex.axis = 0.9)

for(i in 1:nSpecies){
  plot.violins3(list(out$sims.list$mu_a1.sp[ ,as.numeric(coef_sorted$index[i])]),
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



# AES
plot(10, ylim = c(1, nSpecies), 
     xlim = c(-1,1.55), 
     type ="n", yaxt="n", xlab = " ", ylab = "", main = "AES")

for(i in 1:nSpecies){
  plot.violins3(list(out$sims.list$b.a2[ ,as.numeric(coef_sorted$index[i])]),
                x = i,
                at = i,
                violin.width = 0.3,
                plot.ci = 0.95,
                col = "grey52",
                add = T,
                alpha = 0.3,
                scale.width = FALSE,
                border.col = "grey52",
                horizontal = TRUE)}

segments(0, -1, 0, 38.4, col = "red")

#GREEN
plot(10, ylim = c(1, nSpecies), 
     xlim = c(-1,1.55), 
     type ="n", yaxt="n", xlab = " ", ylab = "", main = "GREEN")

for(i in 1:nSpecies){
  plot.violins3(list(out$sims.list$b.a3[ ,as.numeric(coef_sorted$index[i])]),
                x = i,
                at = i,
                violin.width = 0.3,
                plot.ci = 0.95,
                col = "grey52",
                add = T,
                alpha = 0.3,
                scale.width = FALSE,
                border.col = "grey52",
                horizontal = TRUE)}
segments(0, -1, 0, 38.4, col = "red")

mtext("Beta", line = 1.5, side = 1, cex = 0.7, outer = TRUE) 

dev.off()

