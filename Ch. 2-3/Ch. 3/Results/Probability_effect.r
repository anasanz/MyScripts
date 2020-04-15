
# Model 15.1.1

library(rjags)
library(jagsUI)
library(dplyr)

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

source("D:/PhD/MyScripts/Ch. 2-3/Ch. 3/Data/ProcessCodaOutput.R")

out <- ProcessCodaOutput(out.list)
outall <- out$sims.list # 3 chains together 
df.outall <- as.data.frame(outall)


# Probability of positive effect

sp.df.probpos <- data.frame(sp = sp, b.a1 = NA, b.a2 = NA, b.a3 = NA)

coeff <- c("b.a1", "b.a2", "b.a3")
names <- c("b.SG", "b.AES", "b.GREEN")
total.samples <- nrow(df.outall)

for (c in 1:length(coeff)){
  
variab <- df.outall[ ,grep(coeff[c], colnames(df.outall))]

for (i in 1:nSpecies){

sp.variable <- variab[,colnames(variab)[i]] 
positive <- sp.variable[sp.variable > 0] # Probability of positive effect: Using the posterior samples from the management parameters: % of all posterior samples > 0
prob_positive <- (length(positive)/total.samples)*100
sp.df.probpos[i,coeff[c]] <- prob_positive
} }

# Effect size

sp.df.effect <- data.frame(sp = sp, b.a1 = NA, b.a2 = NA, b.a3 = NA)

for (c in 1:length(coeff)){
  v1 <- data.frame(out$mean[names(out$mean) %in% coeff[c]])
  sp.df.effect[,coeff[c]] <-  v1
}

# Quitar MIMIG (fuera de criterio!!!)
sp.df.probpos <- sp.df.probpos[-which(sp.df.probpos$sp == "MIMIG"), ]
sp.df.effect <- sp.df.effect[-which(sp.df.effect$sp == "MIMIG"), ]

##################################################################
#####                        Plot                           ######
##################################################################

#### ♥ GRAPH 2D ####
setwd("D:/PhD/Third chapter/Data/Results")
pdf("2D_prob_effect.pdf")
par(mfrow = c(2,2),
    mar = c(1,3,2,1),
    oma = c(3,1,2,1))
plot(sp.df.probpos$b.a1, sp.df.probpos$b.a2, ylim = c(0,100), pch = 19, xlab = " ", ylab = " ")
abline(v = 75, col = "red", lty = "dashed")
abline(h = 75, col = "red", lty = "dashed")
mtext("Probability of positive effect", line = 1, side = 3)
mtext("AES", line = 3, side = 2, cex = 1)

plot(sp.df.effect$b.a1, sp.df.effect$b.a2, ylim = c(-0.2, 0.6), pch = 19, xlab = " ", ylab = " ")
abline(v = 0, col = "darkgrey", lty = "dashed")
abline(h = 0, col = "darkgrey", lty = "dashed")
mtext("Beta effect size", line = 1, side = 3)

plot(sp.df.probpos$b.a1, sp.df.probpos$b.a3, ylim = c(0,100), pch = 19, xlab = " ", ylab = " ")
abline(v = 75, col = "red", lty = "dashed")
abline(h = 75, col = "red", lty = "dashed")
mtext("GREEN", line = 3, side = 2, cex = 1)
mtext("TFM", line = 3, side = 1, cex = 1)
plot(sp.df.effect$b.a1, sp.df.effect$b.a3, ylim = c(-0.2, 0.6), pch = 19, xlab = " ", ylab = " ")
abline(h = 0, col = "darkgrey", lty = "dashed")
abline(v = 0, col = "darkgrey", lty = "dashed")
mtext("TFM", line = 3, side = 1, cex = 1)
dev.off()

#### ♥ GRAPH 2D COLOURED BY STEPPE INDEX ####

# ---- Load info ----

setwd("D:/PhD/Third chapter/Data")
mine <- read.csv("mi_index.csv", sep = ";")
colnames(mine)[1] <- "sp"
colnames(mine)[6] <- "steppe_index1"
mine <- mine[1:40,c(1,6)]

setwd("D:/PhD/Third chapter/Data")
fbi <- read.csv("index_fbi_csi.csv", sep = ";")
colnames(fbi)[1] <- "sp"
colnames(fbi)[4] <- "steppe_index2"
fbi <- fbi[1:40,c(1,4)]

index <- left_join(mine,fbi)
sp.df.probpos <- left_join(sp.df.probpos,index)
sp.df.effect <- left_join(sp.df.effect,index)


# Change values so that the colour recognizes the value
sp.df.probpos$steppe_index1 <- sp.df.probpos$steppe_index1 + 1
sp.df.probpos$steppe_index2 <- sp.df.probpos$steppe_index2 + 1
sp.df.probpos$steppe_index1[sp.df.probpos$steppe_index1 == 5] <- 4

palette_i1 <- c("green", "orange", "red", "brown")
palette_i2 <- c("green","orange","red")

plot(sp.df.probpos$b.a1, sp.df.probpos$b.a2, ylim = c(0,100), pch = 19, xlab = " ", ylab = " ", col=palette_i1[sp.df.probpos$steppe_index1]) # Use this one to produce graph 
plot(sp.df.probpos$b.a1, sp.df.probpos$b.a2, ylim = c(0,100), pch = 19, xlab = " ", ylab = " ", col=palette_i2[sp.df.probpos$steppe_index2])

# ---- Graph ----
# i1
setwd("D:/PhD/Third chapter/Data/Results")
pdf("2D_prob_effect_colours_i1.pdf")
par(mfrow = c(2,2),
    mar = c(1,3,2,1),
    oma = c(3,1,2,1))
plot(sp.df.probpos$b.a1, sp.df.probpos$b.a2, ylim = c(0,100), pch = 19, xlab = " ", ylab = " ", col=palette_i1[sp.df.probpos$steppe_index1])
abline(v = 75, col = "black", lty = "dashed")
abline(h = 75, col = "black", lty = "dashed")
mtext("Probability of positive effect", line = 1, side = 3)
mtext("AES", line = 3, side = 2, cex = 1)

plot(sp.df.effect$b.a1, sp.df.effect$b.a2, ylim = c(-0.2, 0.6), pch = 19, xlab = " ", ylab = " ", col=palette_i1[sp.df.probpos$steppe_index1])
abline(v = 0, col = "darkgrey", lty = "dashed")
abline(h = 0, col = "darkgrey", lty = "dashed")
mtext("Beta effect size", line = 1, side = 3)

plot(sp.df.probpos$b.a1, sp.df.probpos$b.a3, ylim = c(0,100), pch = 19, xlab = " ", ylab = " ", col=palette_i1[sp.df.probpos$steppe_index1])
abline(v = 75, col = "black", lty = "dashed")
abline(h = 75, col = "black", lty = "dashed")
mtext("GREEN", line = 3, side = 2, cex = 1)
mtext("TFM", line = 3, side = 1, cex = 1)
plot(sp.df.effect$b.a1, sp.df.effect$b.a3, ylim = c(-0.2, 0.6), pch = 19, xlab = " ", ylab = " ", col=palette_i1[sp.df.probpos$steppe_index1])
abline(h = 0, col = "darkgrey", lty = "dashed")
abline(v = 0, col = "darkgrey", lty = "dashed")
mtext("TFM", line = 3, side = 1, cex = 1)
dev.off()


# i2
setwd("D:/PhD/Third chapter/Data/Results")
pdf("2D_prob_effect_colours_i2.pdf")
par(mfrow = c(2,2),
    mar = c(1,3,2,1),
    oma = c(3,1,2,1))
plot(sp.df.probpos$b.a1, sp.df.probpos$b.a2, ylim = c(0,100), pch = 19, xlab = " ", ylab = " ", col=palette_i2[sp.df.probpos$steppe_index2])
abline(v = 75, col = "black", lty = "dashed")
abline(h = 75, col = "black", lty = "dashed")
mtext("Probability of positive effect", line = 1, side = 3)
mtext("AES", line = 3, side = 2, cex = 1)

plot(sp.df.effect$b.a1, sp.df.effect$b.a2, ylim = c(-0.2, 0.6), pch = 19, xlab = " ", ylab = " ", col=palette_i2[sp.df.probpos$steppe_index2])
abline(v = 0, col = "darkgrey", lty = "dashed")
abline(h = 0, col = "darkgrey", lty = "dashed")
mtext("Beta effect size", line = 1, side = 3)

plot(sp.df.probpos$b.a1, sp.df.probpos$b.a3, ylim = c(0,100), pch = 19, xlab = " ", ylab = " ", col=palette_i2[sp.df.probpos$steppe_index2])
abline(v = 75, col = "black", lty = "dashed")
abline(h = 75, col = "black", lty = "dashed")
mtext("GREEN", line = 3, side = 2, cex = 1)
mtext("TFM", line = 3, side = 1, cex = 1)
plot(sp.df.effect$b.a1, sp.df.effect$b.a3, ylim = c(-0.2, 0.6), pch = 19, xlab = " ", ylab = " ", col=palette_i2[sp.df.probpos$steppe_index2])
abline(h = 0, col = "darkgrey", lty = "dashed")
abline(v = 0, col = "darkgrey", lty = "dashed")
mtext("TFM", line = 3, side = 1, cex = 1)
dev.off()


#### ♥ GRAPH 2D HIGHLIGHT TARGET SPECIES ####
sp.df.probpos$target <- ifelse(sp.df.probpos$sp %in% c("COGAR", "BUOED", "CABRA", "PTALC", "TERAX", "MECAL", "PTORI"), 2,1)
sp.df.effect$target <- ifelse(sp.df.effect$sp %in% c("COGAR", "BUOED", "CABRA", "PTALC", "TERAX", "MECAL", "PTORI"),2,1)

palette_target <- c("blue","red")
plot(sp.df.probpos$b.a1, sp.df.probpos$b.a2, ylim = c(0,100), pch = 19, xlab = " ", ylab = " ", col = palette_target[sp.df.probpos$target]) # Use this one to produce graph 

setwd("D:/PhD/Third chapter/Data/Results")
pdf("2D_prob_effect_colours_target.pdf")
par(mfrow = c(2,2),
    mar = c(1,3,2,1),
    oma = c(3,1,2,1))
plot(sp.df.probpos$b.a1, sp.df.probpos$b.a2, ylim = c(0,100), pch = 19, xlab = " ", ylab = " ", col = palette_target[sp.df.probpos$target])
abline(v = 75, col = "black", lty = "dashed")
abline(h = 75, col = "black", lty = "dashed")
mtext("Probability of positive effect", line = 1, side = 3)
mtext("AES", line = 3, side = 2, cex = 1)

plot(sp.df.effect$b.a1, sp.df.effect$b.a2, ylim = c(-0.2, 0.6), pch = 19, xlab = " ", ylab = " ", col = palette_target[sp.df.probpos$target])
abline(v = 0, col = "darkgrey", lty = "dashed")
abline(h = 0, col = "darkgrey", lty = "dashed")
mtext("Beta effect size", line = 1, side = 3)

plot(sp.df.probpos$b.a1, sp.df.probpos$b.a3, ylim = c(0,100), pch = 19, xlab = " ", ylab = " ", col = palette_target[sp.df.probpos$target])
abline(v = 75, col = "black", lty = "dashed")
abline(h = 75, col = "black", lty = "dashed")
mtext("GREEN", line = 3, side = 2, cex = 1)
mtext("TFM", line = 3, side = 1, cex = 1)
plot(sp.df.effect$b.a1, sp.df.effect$b.a3, ylim = c(-0.2, 0.6), pch = 19, xlab = " ", ylab = " ", col = palette_target[sp.df.probpos$target])
abline(h = 0, col = "darkgrey", lty = "dashed")
abline(v = 0, col = "darkgrey", lty = "dashed")
mtext("TFM", line = 3, side = 1, cex = 1)
dev.off()



#### ♥ PLOT 3D ####

library(plot3D)
scatter3D(sp.df.probpos$b.a1, sp.df.probpos$b.a2, sp.df.probpos$b.a3)
scatter3D(sp.df.probpos$b.a1, sp.df.probpos$b.a2, sp.df.probpos$b.a3, theta = 15, phi = 20)
scatter3D(sp.df.probpos$b.a1, sp.df.probpos$b.a2, sp.df.probpos$b.a3, 
          phi = 0, bty = "g",
          pch = 20, cex = 2, ticktype = "detailed")
# Put SG as z
scatter3D(sp.df.probpos$b.a2, sp.df.probpos$b.a3,sp.df.probpos$b.a1, 
          phi = 0, bty = "g",
          pch = 20, cex = 2, ticktype = "detailed")
##

scatter3D(sp.df.probpos$b.a1, sp.df.probpos$b.a2, sp.df.probpos$b.a3, 
          phi = 0, bty = "g", type = "l", 
          ticktype = "detailed", lwd = 4)

scatter3D(sp.df.probpos$b.a1, sp.df.probpos$b.a2, sp.df.probpos$b.a3, 
          phi = 0, bty = "g",  type = "h", 
          ticktype = "detailed", pch = 19, cex = 0.5)
# Put SG as z
scatter3D(sp.df.probpos$b.a2, sp.df.probpos$b.a3, sp.df.probpos$b.a1, 
          phi = 0, bty = "g",  type = "h", 
          ticktype = "detailed", pch = 19, cex = 0.5)
          
scatter3D( sp.df.probpos$b.a1, sp.df.probpos$b.a2, sp.df.probpos$b.a3,
          pch = 16,
          ticktype = "detailed", theta = 30, d = 7)




# VOLCANO
mat <- as.matrix(sp.df.probpos[,-1])
persp3D(z = mat, facets = FALSE, curtain = TRUE)

