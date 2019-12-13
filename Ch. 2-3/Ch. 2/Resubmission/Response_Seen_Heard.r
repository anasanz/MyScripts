
rm(list=ls())


# REVISION: DETECTION CURVES SEEN/HEARD #

# ---- 1. With 4 bins (0-25-50-100-200): With dataset for chapter 2 ----

# Species ch. 2 with all observations

setwd("C:/Users/ana.sanz/Documents/PhD/Second chapter/Data")

dat <- read.csv("DataDS_ch2_SEEN_HEARD.csv")

library(dplyr)
setwd("C:/Users/ana.sanz/Documents/PhD/Second chapter/Data") # Names of the species for plot
leg <- read.csv("leg_species.csv", sep = ";")
leg <- arrange(leg,codiEspecie)
spec <- leg$codiEspecie
spec_name <- leg$English

setwd("C:/Users/ana.sanz/Documents/PhD/Second chapter/Resubmission")
pdf("df_ch2_allobservations_4bins.pdf")

par(mfrow = c(4,3))
for (i in 1:length(spec)){
  hist(dat$distance[which(dat$Species %in% spec[i])],breaks = c(0,25,50,99,200), xlab = "Distance",
       main = spec_name[i], col = "grey", freq = FALSE) 
}
dev.off()

# Species ch. 2 with only seen observations

dat2 <- dat[which(dat$Obs_type == "V"), ]
dat2 <- dat2[ ,-which(colnames(dat2) %in% "Obs_type")]

setwd("C:/Users/ana.sanz/Documents/PhD/Second chapter/Resubmission")
pdf("df_ch2_seen_4bins.pdf")

par(mfrow = c(4,3))
for (i in 1:length(spec)){
  hist(dat2$distance[which(dat2$Species %in% spec[i])],breaks = c(0,25,50,99,200), xlab = "Distance",
       main = spec_name[i], col = "grey", freq = FALSE) 
}
dev.off()


# ---- 2. With 5 bins (0-25-50-100-200-500): With dataset from chapter 3

# Species ch. 2 with all observations

setwd("C:/Users/ana.sanz/Documents/PhD/Third chapter/Data")
dat3 <- read.csv("DataDS_ch3_allsp.csv", sep = ",")

setwd("C:/Users/ana.sanz/Documents/PhD/Second chapter/Resubmission")
pdf("df_ch2_allobservations_5bins.pdf")

par(mfrow = c(4,3))
for (i in 1:length(spec)){
  hist(dat3$distance[which(dat3$Species %in% spec[i])],breaks = c(0,25,50,99,200,500), xlab = "Distance",
       main = spec_name[i], col = "grey", freq = FALSE) 
}
dev.off()
