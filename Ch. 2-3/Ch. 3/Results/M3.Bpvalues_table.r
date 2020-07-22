


#### TABLE BAYESIAN BP VALUES ####

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

# Species 
setwd("D:/PhD/Third chapter/Data")
leg <- read.csv("leg_species.csv", sep = ";")
leg <- leg[which(leg$codiEspecie %in% sp), c(1,2,6)]
leg <- arrange(leg, by = leg$codiEspecie)
colnames(leg)[1] <- "sp"



sp.df <- data.frame(sp = sp,
                    Bp.N.sp = out$colnames.sims[grep("Bp.N.sp", out$colnames.sims)],
                    Bp.Obs.sp = out$colnames.sims[grep("Bp.Obs.sp", out$colnames.sims)] )

# Bp.Obs.sp

v1 <- data.frame(out$mean[names(out$mean) %in% "Bp.Obs.sp"])
v2 <- data.frame(sp.df[ ,colnames(sp.df) %in% "Bp.Obs.sp"])
values <- cbind(v1,v2)
colnames(values)[1] <- "mean"
colnames(values)[2] <- "Bp.Obs.sp"
values2 <- left_join(sp.df,values)
df.bp_obs <- values2[,colnames(values2) %in% c("Bp.Obs.sp", "sp", "mean")]
colnames(df.bp_obs)[3] <- "Bp.obs"

# Bp.N.sp
v1 <- data.frame(out$mean[names(out$mean) %in% "Bp.N.sp"])
v2 <- data.frame(sp.df[ ,colnames(sp.df) %in% "Bp.N.sp"])
values <- cbind(v1,v2)
colnames(values)[1] <- "mean"
colnames(values)[2] <- "Bp.N.sp"
values2 <- left_join(sp.df,values)
df.bp_N <- values2[,colnames(values2) %in% c("Bp.N.sp", "sp", "mean")]
colnames(df.bp_N)[3] <- "Bp.ab"

df.bp <- left_join(df.bp_obs, df.bp_N, by = "sp")
df.bp <- df.bp[ ,-c(2,4)]
df.bp <- cbind(df.bp$sp, round(df.bp[,c(2,3)], 3)) 
colnames(df.bp)[1] <- "sp"
df <- left_join(df.bp,  leg, by = "sp")
df <- df[ ,-c(1,4)]
df <- df[ ,c(3,1,2)]
colnames(df)[1] <- "Species"

setwd("D:/PhD/Third chapter/Data/Results/Version 2")
write.csv(df, "TableM_bpvalues.csv")

# Check bad bp

bad_bp_obs <- df.bp_obs[which(df.bp_obs$mean < 0.1 | df.bp_obs$mean > 0.9), ]
nrow(bad_bp_obs)
bad_bp_N <- df.bp_N[which(df.bp_N$mean < 0.1 | df.bp_N$mean > 0.9), ]
nrow(bad_bp_N)


# Community bp
out$mean$Bp.Obs
out$mean$Bp.N
