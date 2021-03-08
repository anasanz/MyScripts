rm(list=ls())


################################################################
# I runned a new model to test if a mistake in the greening and AES 
# variable changes, but it doesnt. So I keep the original graphs 
# I did this script to check if results changed with the new model
################################################################

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
load("D:/PhD/Third chapter/Data/model/14.8.2_f_FIXED_RESUB/14.2.8_f_results_fixed_1toolong.RData")

# It did not thin, thin it:
subs <- seq(0,450000, by = 10)

# Choose only the fallow variables
b.a1 <- colnames(out$samples[[1]])[grep("b.a1", colnames(out$samples[[1]]))]
b.a2 <- colnames(out$samples[[1]])[grep("b.a2", colnames(out$samples[[1]]))]
b.a3 <- colnames(out$samples[[1]])[grep("b.a3", colnames(out$samples[[1]]))]

b <- c(b.a1, b.a2, b.a3)

out.list<- list()
out.list[[1]] <- as.mcmc(out$samples[[1]][subs,b])
out.list[[2]] <- as.mcmc(out$samples[[2]][subs,b])
out.list[[3]] <- as.mcmc(out$samples[[3]][subs,b])

out.list <- as.mcmc.list(out.list)

source("D:/PhD/MyScripts/Ch. 2-3/Ch. 3/Results/Functions/ProcessCodaOutput.R")

out <- ProcessCodaOutput(out.list)
outall <- out$sims.list # 3 chains together 
df.outall <- as.data.frame(outall)
names(df.outall)

# Create data frame with species - coefficients together
sp.df <- data.frame(sp = sp, b.a1, b.a2, b.a3)

# Process samples

coeff <- c("b.a1", "b.a2", "b.a3")
names <- c("b.SG", "b.AES", "b.GREEN")

# For legend

setwd("D:/PhD/Third chapter/Data")
leg <- read.csv("leg_species.csv", sep = ";")
leg <- leg[which(leg$codiEspecie %in% sp), c(1,2,6)]
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

# Plot
setwd("D:/PhD/Third chapter/Data/Results_species/14.2/14.2.8_f_fixed")
pdf(paste("14.2.8_f.ViolinWide_grey.pdf"), 6,7)

par(mfrow = c(1,3),
    mar = c(1,1.5,1.5,0),
    oma = c(3,8,2,1),
    xpd = NA) 

# SG
plot(10, ylim = c(1, nSpecies), 
     xlim = c(-1,1.55), 
     type ="n", yaxt="n", xlab = " ", ylab = "", main = "TFM")

target1 <- cbind(c(-3.4,-1.3,-1.3,-3.4), c(36.5,36.5,37.5,37.5)) # 1st column X : bottomleft, bottom right, topright, topleft.# 2nd column Y
polygon(target1[,1], target1[,2], col =  adjustcolor("grey52",alpha.f = 0.4), border = "white")

target2 <- cbind(c(-2.5,-1.3,-1.3,-2.5), c(34.5,34.5,35.5,35.5)) # 1st column X : bottomleft, bottom right, topright, topleft.# 2nd column Y
polygon(target2[,1], target2[,2], col =  adjustcolor("grey52",alpha.f = 0.4), border = "white")

target3 <- cbind(c(-2.7,-1.3,-1.3,-2.7), c(33.5,33.5,34.5,34.5)) # 1st column X : bottomleft, bottom right, topright, topleft.# 2nd column Y
polygon(target3[,1], target3[,2], col =  adjustcolor("grey52",alpha.f = 0.4), border = "white")

target4 <- cbind(c(-2.5,-1.3,-1.3,-2.5), c(32.5,32.5,33.5,33.5)) # 1st column X : bottomleft, bottom right, topright, topleft.# 2nd column Y
polygon(target4[,1], target4[,2], col =  adjustcolor("grey52",alpha.f = 0.4), border = "white")

target5 <- cbind(c(-3.4,-1.3,-1.3,-3.4), c(22.5,22.5,23.5,23.5)) # 1st column X : bottomleft, bottom right, topright, topleft.# 2nd column Y
polygon(target5[,1], target5[,2], col =  adjustcolor("grey52",alpha.f = 0.4), border = "white")

target6 <- cbind(c(-3.6,-1.3,-1.3,-3.6), c(18.5,18.5,19.5,19.5)) # 1st column X : bottomleft, bottom right, topright, topleft.# 2nd column Y
polygon(target6[,1], target6[,2], col =  adjustcolor("grey52",alpha.f = 0.4), border = "white")

target7 <- cbind(c(-2.6,-1.3,-1.3,-2.6), c(8.5,8.5,9.5,9.5)) # 1st column X : bottomleft, bottom right, topright, topleft.# 2nd column Y
polygon(target7[,1], target7[,2], col =  adjustcolor("grey52",alpha.f = 0.4), border = "white")

axis(2, c(1:nSpecies), labels = sp_sorted, las = 2, cex.axis = 0.9)

for(i in 1:nSpecies){
  plot.violins3(list(out$sims.list$b.a1[ ,as.numeric(coef_sorted$index[i])]),
                x = i,
                at = i,
                violin.width = 0.3,
                plot.ci = 0.95,
                col =  adjustcolor("grey52",alpha.f = 0.4),
                add = T,
                alpha = 1,
                scale.width = FALSE,
                border.col = "black",
                lwd.CI = 1,
                horizontal = TRUE,
                median = TRUE) }
segments(0, -1, 0, 38.4, col = "red")

# For indicating significant effects
over0<- as.numeric(data.table::between(0, out$q2.5$b.a1, out$q97.5$b.a1))
sig_TFM <- ifelse(over0 == 1, " ", "*")
sig_TFM <- as.data.frame(cbind(sp, sig_TFM))
sig_TFM_sp <- left_join(values_sorted_legend, sig_TFM, by = "sp")

text(-1, sort(seq(1:37)), labels = sig_TFM_sp$sig_TFM, cex = 1.1)
mtext("Beta", line = 2.2, side = 1, cex = 0.7) 

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
                col =  adjustcolor("grey52",alpha.f = 0.4),
                add = T,
                alpha = 1,
                scale.width = FALSE,
                border.col = "black",
                horizontal = TRUE,
                median = TRUE)}

segments(0, -1, 0, 38.4, col = "red")

over0<- as.numeric(data.table::between(0, out$q2.5$b.a2, out$q97.5$b.a2))
sig_TFM <- ifelse(over0 == 1, " ", "*")
sig_TFM <- as.data.frame(cbind(sp, sig_TFM))
sig_TFM_sp <- left_join(values_sorted_legend, sig_TFM, by = "sp")

text(-1, sort(seq(1:37)), labels = sig_TFM_sp$sig_TFM, cex = 1.1)

mtext("Beta", line = 2.2, side = 1, cex = 0.7) 

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
                col =  adjustcolor("grey52",alpha.f = 0.4),
                add = T,
                alpha = 1,
                scale.width = FALSE,
                border.col = "black",
                horizontal = TRUE,
                median = TRUE)}
segments(0, -1, 0, 38.4, col = "red")

over0<- as.numeric(data.table::between(0, out$q2.5$b.a3, out$q97.5$b.a3))
sig_TFM <- ifelse(over0 == 1, " ", "*")
sig_TFM <- as.data.frame(cbind(sp, sig_TFM))
sig_TFM_sp <- left_join(values_sorted_legend, sig_TFM, by = "sp")

text(-1, sort(seq(1:37)), labels = sig_TFM_sp$sig_TFM, cex = 1.1)

mtext("Beta", line = 2.2, side = 1, cex = 0.7) 


dev.off()

