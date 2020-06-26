# ---- Long by groups of species ----

rm(list=ls())


library(rjags)
library(jagsUI)
library(dplyr)

source("D:/PhD/MyScripts/Ch. 2-3/Ch. 3/Results/Functions/ProcessCodaOutput.R")
source("D:/PhD/MyScripts/Ch. 2-3/Ch. 3/Results/Functions/DoScale.r")
source("D:/PhD/MyScripts/Ch. 2-3/Ch. 3/Results/Functions/plot.violins3.r")


# MODEL

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

# SPECIES

# Load species analyzed in the model

setwd("D:/PhD/Third chapter/Data")
d <- read.csv("DataDS_ch3_15_19_READY_FIXED.csv")
bad_bp <- c("GACRI", "GATHE", "PADOM", "STSSP") # to remove all species with bad bp except MICAL and MECAL
d <- d[-which(d$Species %in% bad_bp), ]

sp <- as.character(unique(d$Species))
sp <- sort(sp)
nSpecies <- length(sp)

# Divide species by groups
setwd("D:/PhD/Third chapter/Data")

sp_groups <- read.csv("index_groups_sp.csv", sep = ";")
# Target vS rest
species_target <- sp_groups$codiEspecie[which(sp_groups$Categories_target == 1)]
species_rest <- sp_groups$codiEspecie[which(sp_groups$Categories_target == 0)]

species_rest1 <- species_rest[1:17]
species_rest2 <- species_rest[18:33]

# Rest in categories
species_g <- sp_groups$codiEspecie[which(sp_groups$Categories_target ==  0 & sp_groups$Categories_graph == "G")]
species_fg <- sp_groups$codiEspecie[which(sp_groups$Categories_target ==  0 & sp_groups$Categories_graph == "FG")]
species_gn <- sp_groups$codiEspecie[which(sp_groups$Categories_target ==  0 & sp_groups$Categories_graph == "GN")]

leg <- read.csv("leg_species.csv", sep = ";")
leg <- leg[,c(1,2)]
colnames(leg)[1] <- "sp"


# Create data frame with species - coefficients together
sp.df <- data.frame(sp = sp, b.a3 = out$colnames.sims[grep("b.a3", out$colnames.sims)],
                    b.a2 = out$colnames.sims[grep("b.a2", out$colnames.sims)],
                    b.a1 = out$colnames.sims[grep("b.a1", out$colnames.sims)])

coeff <- c("b.a3", "b.a2", "b.a1")
names <- c("b.GREEN", "b.AES", "b.SG")

# ORDER SG

v1 <- data.frame(out$mean[names(out$mean) %in% coeff[3]])
v2 <- data.frame(sp.df[ ,colnames(sp.df) %in% coeff[3]])
values <- cbind(v1,v2)
colnames(values)[1] <- "mean"
colnames(values)[2] <- coeff[3]
values$index <- rownames(values)
values2 <- left_join(sp.df,values)
values_sorted <- arrange(values2, mean)

sp_sorted <-  values_sorted$sp
coef_sorted <- values_sorted[,-c(1,5)]
outall <- out$sims.list # 3 chains together


g <- list()
s <- list()
for (i in 1:nSpecies){
  for (c in 1:length(coeff)){
    sims_coef <- data.frame(outall[names(outall) %in% coeff[c]])
    colnames(sims_coef) <- values2[,which(colnames(values2) %in% coeff[c])]
    s[[c]] <- sims_coef[,which(colnames(sims_coef) %in% coef_sorted[i,c])]
  }
  g[[i]] <- s
}

### FOR LEGEND
values_sorted_LEG <- left_join(values_sorted, leg)

#### PLOT ####

# ---- Target ----

setwd("D:/PhD/Third chapter/Data/Results_species/15.1/15.1.1_DATA_GOODsp_resiN")
pdf(paste("15.1.1.ViolinLong_target.pdf"),6,6)

par(mfrow = c(1,2), 
    mar = c(3,0.5,3,0))


values_sorted_LEG_t <- values_sorted_LEG[which(values_sorted_LEG$sp %in% species_target), ]
sp_t <- as.numeric(rownames(values_sorted_LEG_t))
g1 <- g[sp_t]

nSpecies <- length(sp_t)
sp_sorted <- values_sorted_LEG_t$sp

# PLOT
plot(10, ylim = c(0, nSpecies*3), 
     xlim = c(-1,2.5), 
     type ="n", yaxt="n", xaxt="n", xlab = " ", ylab = "", main = " ")

mtext("Target species", line = 1, side = 3, cex = 1, font = 2) 

col=c("blue","darkorange","red")
offset <- c(-0.7,0,0.7)

abline(h=seq(1,nSpecies*3, by = 6) ,col=adjustcolor(grey(0.8),alpha = 0.5), lwd=50)

axis(2, at = seq(1,nSpecies*3, by =3), labels = sp_sorted, las = 2, cex.axis = 0.9, tick = FALSE, pos = 2.7)
axis(1, at = c(-1,-0.5,0,0.5,1,1.5), labels = c("-1","-0.5","0","0.5","1","1.5"), cex.axis = 0.9)

ii <- seq(1,nSpecies*3, by =3)
for(c in 1:3){
  for(i in 1:nSpecies){
    plot.violins3(list(g1[[i]][[c]]),
                  x = i,
                  at =  ii[i]+offset[c],
                  violin.width = 0.3,
                  col = col[c],
                  add = T,
                  alpha = 0.3,
                  scale.width = FALSE,
                  border.col = col[c],
                  horizontal = TRUE) }
}
abline(v=0)

plot(10, axes = FALSE)
legend("topleft", legend = c("Targeted fallow management", "AES", "Greening"), col = c("red", "darkorange", "blue"), fill = c("red", "darkorange", "blue"), bty = "n", cex = 0.9)


dev.off()


# ---- Rest ----

setwd("D:/PhD/Third chapter/Data/Results_species/15.1/15.1.1_DATA_GOODsp_resiN")
pdf(paste("15.1.1.ViolinLong_rest.pdf"),5,10)

par(mfrow = c(1,1), 
    mar = c(3,3,3,3))


values_sorted_LEG_r <- values_sorted_LEG[which(values_sorted_LEG$sp %in% species_rest), ]
sp_r <- as.numeric(rownames(values_sorted_LEG_r))
g2 <- g[sp_r]

nSpecies <- length(sp_r)
sp_sorted <- values_sorted_LEG_r$sp

# PLOT
plot(10, ylim = c(3, nSpecies*3-3), 
     xlim = c(-1,2.5), 
     type ="n", yaxt="n", xaxt="n", xlab = " ", ylab = "", main = " ")

mtext("Community species", line = 1, side = 3, cex = 1, font = 2) 

col=c("blue","darkorange","red")
offset <- c(-0.8,0,0.8)

abline(h=seq(1,nSpecies*3, by = 6) ,col=adjustcolor(grey(0.8),alpha = 0.5), lwd=20)

axis(2, at = seq(1,nSpecies*3, by =3), labels = sp_sorted, las = 2, cex.axis = 0.9, tick = FALSE, pos = 2.7)
axis(1, at = c(-1,-0.5,0,0.5,1,1.5), labels = c("-1","-0.5","0","0.5","1","1.5"), cex.axis = 0.9, pos = 0)

ii <- seq(1,nSpecies*3, by =3)
for(c in 1:3){
  for(i in 1:nSpecies){
    plot.violins3(list(g2[[i]][[c]]),
                  x = i,
                  at =  ii[i]+offset[c],
                  violin.width = 0.3,
                  col = col[c],
                  add = T,
                  alpha = 0.3,
                  scale.width = FALSE,
                  border.col = col[c],
                  horizontal = TRUE) }
}
abline(v=0)

dev.off()


# ---- Cereal specialist ----

values_sorted_LEG_cs <- values_sorted_LEG[which(values_sorted_LEG$sp %in% species_cs), ]
sp_cspecialist <- as.numeric(rownames(values_sorted_LEG_cs))
g2 <- g[sp_cspecialist]

nSpeciesGN <- nSpecies
nSpecies <- length(sp_cspecialist)
sp_sorted <- c(" ", " "," "," "," "," ", values_sorted_LEG_cs$sp)

# PLOT
plot(10, ylim = c(1, nSpeciesGN*3), 
     xlim = c(-1,2.5), 
     type ="n", yaxt="n",  xaxt="n", xlab = " ", ylab = "", main = " ")

mtext("Cereal specialist", line = 1, side = 3, cex = 1, font = 2) 

col=c("blue","darkorange","red")
offset <- c(-0.7,0,0.7)

abline(h = seq(19,nSpeciesGN*3, by = 6) ,col=adjustcolor(grey(0.8),alpha=0.5), lwd=50)

axis(2, at = seq(1,nSpeciesGN*3, by = 3), labels = sp_sorted, las = 2, cex.axis = 0.8, tick = FALSE, pos = 2.7)
axis(1, at = c(-1,-0.5,0,0.5,1,1.5), labels = c("-1","-0.5","0","0.5","1","1.5"), cex.axis = 0.9)

ii <- seq(19,nSpeciesGN*3, by =3)
for(c in 1:3){
  for(i in 1:nSpecies){
    plot.violins3(list(g2[[i]][[c]]),
                  x = i,
                  at =  ii[i]+offset[c],
                  violin.width = 0.3,
                  col = col[c],
                  add = T,
                  alpha = 0.3,
                  scale.width = FALSE,
                  border.col = col[c],
                  horizontal = TRUE) }
}
abline(v=0)

dev.off()

#####

setwd("D:/PhD/Third chapter/Data/Results_species/15.1/15.1.1_DATA_GOODsp_resiN")
pdf(paste("15.1.1.ViolinLong_Groups_G_FG.pdf"),7,7)

par(mfrow = c(1,2), 
    mar = c(3,1,3,1))

# ---- Generalists ----

values_sorted_LEG_g <- values_sorted_LEG[which(values_sorted_LEG$sp %in% species_g), ]
sp_generalist <- as.numeric(rownames(values_sorted_LEG_g))
g1 <- g[sp_generalist]

nSpecies <- length(sp_generalist)
sp_sorted <- values_sorted_LEG_g$sp

# PLOT
plot(10, ylim = c(1, nSpecies*3), 
     xlim = c(-1,2.5), 
     type ="n", yaxt="n",  xaxt="n", xlab = " ", ylab = "", main = " ")

mtext("Generalist", line = 1, side = 3, cex = 1, font = 2) 

col=c("blue","darkorange","red")
offset <- c(-0.7,0,0.7)

abline(h=seq(1,nSpecies*3, by = 6) ,col=adjustcolor(grey(0.8),alpha = 0.5), lwd=40)

axis(2, at = seq(1,nSpecies*3, by =3), labels = sp_sorted, las = 2, cex.axis = 0.8, tick = FALSE, pos = 2.7)
axis(1, at = c(-1,-0.5,0,0.5,1,1.5), labels = c("-1","-0.5","0","0.5","1","1.5"), cex.axis = 0.9)

ii <- seq(1,nSpecies*3, by =3)
for(c in 1:3){
  for(i in 1:nSpecies){
    plot.violins3(list(g1[[i]][[c]]),
                  x = i,
                  at =  ii[i]+offset[c],
                  violin.width = 0.3,
                  col = col[c],
                  add = T,
                  alpha = 0.3,
                  scale.width = FALSE,
                  border.col = col[c],
                  horizontal = TRUE) }
}
abline(v=0)

# ---- Farmland Generalists ----

values_sorted_LEG_fg <- values_sorted_LEG[which(values_sorted_LEG$sp %in% species_fg), ]
sp_fg <- as.numeric(rownames(values_sorted_LEG_fg))
g4 <- g[sp_fg]

nSpeciesG <- nSpecies
nSpecies <- length(sp_fg)
sp_sorted <- c(" "," "," "," "," "," ", values_sorted_LEG_fg$sp)

# PLOT
plot(10, ylim = c(1, nSpeciesG*3), 
     xlim = c(-1,2.5), 
     type ="n", yaxt="n",  xaxt="n", xlab = " ", ylab = "", main = " ")


mtext("Farmland generalist", line = 1, side = 3, cex = 1, font = 2) 

col=c("blue","darkorange","red")
offset <- c(-0.7,0,0.7)

abline(h = seq(19,nSpeciesG*3, by = 6) ,col=adjustcolor(grey(0.8),alpha=0.5), lwd = 40)

axis(2, at = seq(1,nSpeciesG*3, by = 3), labels = sp_sorted, las = 2, cex.axis = 0.8, tick = FALSE, pos = 2.7)
axis(1, at = c(-1,-0.5,0,0.5,1,1.5), labels = c("-1","-0.5","0","0.5","1","1.5"), cex.axis = 0.9)

ii <- seq(19,nSpeciesG*3, by =3)
for(c in 1:3){
  for(i in 1:nSpecies){
    plot.violins3(list(g4[[i]][[c]]),
                  x = i,
                  at =  ii[i]+offset[c],
                  violin.width = 0.3,
                  col = col[c],
                  add = T,
                  alpha = 0.3,
                  scale.width = FALSE,
                  border.col = col[c],
                  horizontal = TRUE) }
}
abline(v=0)


dev.off()

#Colours: "darkmagenta", "darkorange", "darkolivegreen4"