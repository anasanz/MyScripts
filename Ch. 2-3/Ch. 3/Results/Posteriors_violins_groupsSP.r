# ---- Long by groups of species ----

rm(list=ls())


library(rjags)
library(jagsUI)
library(dplyr)

source("D:/PhD/MyScripts/Ch. 2-3/Ch. 3/Results/Functions/ProcessCodaOutput.R")
source("D:/PhD/MyScripts/Ch. 2-3/Ch. 3/Results/Functions/plot.violins3.r")
source("D:/PhD/MyScripts/Ch. 2-3/Ch. 3/Results/Functions/DoScale.r")

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
sp_g <- sp_groups$codiEspecie[which(sp_groups$Categories_graph == "G")]
sp_fg <- sp_groups$codiEspecie[which(sp_groups$Categories_graph == "FG")]
sp_gn <- sp_groups$codiEspecie[which(sp_groups$Categories_graph == "GN")]
sp_cs <- sp_groups$codiEspecie[which(sp_groups$Categories_graph == "CS")]

leg <- read.csv("leg_species.csv", sep = ";")


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

# ---- Generalists ----

sp.df_g <- sp.df[sp.df$sp %in% sp_g, ]
sp_generalist <- as.numeric(rownames(sp.df_g))
g1 <- g[sp_generalist]

nSpecies <- length(sp_generalist)
sp_sorted <- sp.df_g$sp

setwd("D:/PhD/Third chapter/Data/Results_species/15.1/15.1.1_DATA_GOODsp_resiN")
pdf(paste("15.1.1.ViolinLong_G2.pdf"), width=5, height = 10)

# PLOT
plot(10, ylim = c(1, nSpecies*3), 
     xlim = c(-1,1.7), 
     type ="n", yaxt="n", xlab = "Beta", ylab = "", main = " ")

mtext("Species", line = 3, side = 2, cex = 0.8, outer = TRUE) 
axis(2, at = seq(1,nSpecies*3, by =3), labels = sp_sorted, las = 2, cex.axis = 0.9)
#x.pos <- seq(1,111, by = 3)
col=c("blue","darkorange","red")
offset <- c(-0.7,0,0.7)

abline(h=seq(1,nSpecies*3, by = 6) ,col=adjustcolor(grey(0.8),alpha=0.5), lwd=40)
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

dev.off()


#Colours: "darkmagenta", "darkorange", "darkolivegreen4"