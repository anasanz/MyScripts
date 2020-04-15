
rm(list=ls())

library(dplyr)

# See correlation between landscape variables - detections 

############################
#      CROP DIVERSITY     #
############################

#### 1. Determine the number of crops where species have been detected #### 

setwd("D:/PhD/Third chapter/Data")
d <- read.csv("DataDS_ch3_15_19_READY_FIXED.csv")

us <- read.csv("Usos.csv", sep = ";")
colnames(us)[1] <- "Crop_type"
us <- us[,c(1,2,5,6,7)]

d <- left_join(d,us)
sp_use <- aggregate(Species ~ Use, data = d, FUN = summary)
str(sp_use)
sp_use$Species

# Use1 = Check with general crop categories 
use1 <- aggregate(Use ~ Species, data = d, FUN = summary)

crops <- function(x){
  p <- ifelse(x>0,1,0)
  div <- rowSums(p)
  print(div)
}

use1$pres_crops <- crops(use1[,-1])

# Use2 = Check with specific crop categories 
use2 <- aggregate(Crop_type ~ Species, data = d, FUN = summary)
use2$pres_crops <- crops(use2[,-1])

#### 2. Relate it to field size coefficients #### 

# LOAD SPECIES ANALYZED IN 14.2.1 AND RESULTS 

setwd("D:/PhD/Third chapter/Data")
d <- read.csv("DataDS_ch3_15_19_READY_FIXED.csv")

bad_bp <- c("GACRI", "GATHE", "PADOM", "STSSP") # to remove all species with bad bp except MICAL and MECAL
d <- d[-which(d$Species %in% bad_bp), ]

sp <- as.character(unique(d$Species))
sp <- sort(sp)

library(rjags)
library(jagsUI)
library(dplyr)

# Load the three chains
load("D:/PhD/Third chapter/Data/model/14.2.1/JagsOutFOR14.2.1a.RData")
outa <- out
load("D:/PhD/Third chapter/Data/model/14.2.1/JagsOutFOR14.2.1b.RData")
outb <- out
load("D:/PhD/Third chapter/Data/model/14.2.1/JagsOutFOR14.2.1c.RData")
outc <- out
class(outc)


out.list<- list()
out.list[[1]] <- outa$samples[[1]]
out.list[[2]] <- outb$samples[[1]]
out.list[[3]] <- outc$samples[[1]]

out.list <- as.mcmc.list(out.list)

source("D:/PhD/MyScripts/Ch. 2-3/Ch. 3/Data/ProcessCodaOutput.R")

out <- ProcessCodaOutput(out.list)

#### 3.  Calculate correlation ####

cd <- data.frame(Species = sp, beta_cd = out$mean$bCropdiv)

# With general use categories
cd1 <- left_join(cd, use1)
cd1 <- cd1[ ,c(1,2,4)]

m1 <- lm(beta_cd ~ pres_crops, data = cd1)
plot(beta_cd ~ pres_crops, data = cd1)
abline(m1)
summary(m1)

# With fine use categories

cd2 <- left_join(cd, use2)
cd2 <- cd2[ ,c(1,2,4)]

m2 <- lm(beta_cd ~ pres_crops , data = cd2)
plot(beta_cd ~ pres_crops, data = cd2)
abline(m2)
summary(m2)


############################
#         FIED SIZE        #
############################

sp_fs <- data.frame(Species = use2$Species, Edge = use2$Crop_type[,colnames(use2$Crop_type) %in% "M"])
fs <- data.frame(Species = sp, beta_fs = out$mean$bFieldsize)
fs1 <- left_join(fs, sp_fs)

# Ponderate by number of detections
detect <- as.data.frame(d %>% group_by(Species) %>% summarise(n()))

fs1$prop <- (fs1$Edge / detect$`n()`) * 100

m3 <- lm(beta_fs ~ prop, data = fs1)
plot(beta_fs ~ prop, data = fs1)
abline(m3)
summary(m3)
# The more proportion of positions in edges, the more selection towards big field sizes???