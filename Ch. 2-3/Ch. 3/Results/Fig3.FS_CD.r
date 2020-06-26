
rm(list=ls())


library(rjags)
library(jagsUI)
library(dplyr)

source("D:/PhD/MyScripts/Ch. 2-3/Ch. 3/Results/Functions/ProcessCodaOutput.R")
source("D:/PhD/MyScripts/Ch. 2-3/Ch. 3/Results/Functions/plot.violins3.r")
source("D:/PhD/MyScripts/Ch. 2-3/Ch. 3/Results/Functions/DoScale.r")


# Load species analyzed in the model

setwd("D:/PhD/Third chapter/Data")
d <- read.csv("DataDS_ch3_15_19_READY_FIXED.csv")
bad_bp <- c("GACRI", "GATHE", "PADOM", "STSSP") # to remove all species with bad bp except MICAL and MECAL
d <- d[-which(d$Species %in% bad_bp), ]

sp <- as.character(unique(d$Species))
sp <- sort(sp)
nSpecies <- length(sp)

# Load the three chains
load("D:/PhD/Third chapter/Data/model/15.1.1/JagsOutFOR15.1.1a.RData")
outa <- out
load("D:/PhD/Third chapter/Data/model/15.1.1/JagsOutFOR15.1.1b.RData")
outb <- out
load("D:/PhD/Third chapter/Data/model/15.1.1/JagsOutFOR15.1.1c.RData")
outc <- out
class(outc)


out.list<- list()
out.list[[1]] <- as.mcmc(outa$samples[[1]])
out.list[[2]] <- as.mcmc(outb$samples[[1]])
out.list[[3]] <- as.mcmc(outc$samples[[1]])

out.list <- as.mcmc.list(out.list)
out <- ProcessCodaOutput(out.list)

# For legend

setwd("D:/PhD/Third chapter/Data")
leg <- read.csv("leg_species.csv", sep = ";")
leg <- leg[which(leg$codiEspecie %in% sp), c(1:3)]
leg <- arrange(leg, by = leg$codiEspecie)

# Create data frame with species - coefficients together
sp.df <- data.frame(sp = sp, bCropdiv = out$colnames.sims[grep("bCropdiv", out$colnames.sims)],
                    bFieldsize = out$colnames.sims[grep("bFieldsize", out$colnames.sims)])


coeff <- c("bCropdiv", "bFieldsize")
names <- c("bCropdiv", "bFieldsize")



# Plot Fig3
setwd("D:/PhD/Third chapter/Data/Results_species/15.1/15.1.1_DATA_GOODsp_resiN")
pdf(paste("15.1.1.ViolinWide_FS_CD.pdf"))

par(mfrow = c(1,3),
    mar = c(1,5,1.5,0),
    oma = c(3,4,3,1)) 

# Plot legend
plot(576, ylim = c(1, nSpecies+1), 
     xlim = c(-1,1.7), 
     axes = FALSE, ylab = " ")
text(-0.8, sort(seq(1:37), decreasing = TRUE), labels = leg$codiEspecie[1:37], cex = 0.7)
text(-0.2, sort(seq(1:37), decreasing = TRUE), labels = leg$nomActual[1:37], cex = 0.7, pos = 4)
mtext("Species", line = 1, side = 2, cex = 0.8) 

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
sp_sorted <-  values_sorted$sp
coef_sorted <- values_sorted[,-c(1,4)]

# Plot

plot(10, ylim = c(1, nSpecies+1), 
     xlim = c(-1.5,1.5), 
     type ="n", yaxt="n", xlab = "Beta", ylab = "", main = "Crop diversity")

axis(2, c(1:nSpecies), labels = sp_sorted, las = 2, cex.axis = 0.9)

for(i in 1:nSpecies){
  plot.violins3(list(out$sims.list$bCropdiv[ ,as.numeric(coef_sorted$index[i])]),
                x = i,
                at = i,
                violin.width = 0.3,
                col = "grey",
                add = T,
                alpha = 0.3,
                scale.width = FALSE,
                border.col = "grey",
                horizontal = TRUE) }
abline(v=0)

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
sp_sorted <-  values_sorted$sp
coef_sorted <- values_sorted[,-c(1,4)]

# Plot

plot(10, ylim = c(1, nSpecies+1), 
     xlim = c(-1.7,1.5), 
     type ="n", yaxt="n", xlab = "Beta", ylab = "", main = "Field size")

axis(2, c(1:nSpecies), labels = sp_sorted, las = 2, cex.axis = 0.9)

for(i in 1:nSpecies){
  plot.violins3(list(out$sims.list$bFieldsize[ ,as.numeric(coef_sorted$index[i])]),
                x = i,
                at = i,
                violin.width = 0.3,
                col = "grey",
                add = T,
                alpha = 0.3,
                scale.width = FALSE,
                border.col = "grey",
                horizontal = TRUE) }
abline(v=0)

dev.off()
