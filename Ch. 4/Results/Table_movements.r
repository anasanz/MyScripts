rm(list = ls())

library(dplyr)
library(rgdal)
library(tidyr)


setwd("D:/PhD/Fourth chapter/Results/Analisis3_bottleneck_effect")

## -------------------------------------------------
##                TABLE MOVEMENTS
## ------------------------------------------------- 

## -------------------------------------------------
##     Distance between consecutive positions
## ------------------------------------------------- 

eudist <- read.table("D:/PhD/Fourth chapter/Results/Analisis3_bottleneck_effect/Euclidean_distances.txt", header = T, dec = ",", sep = "\t")

# Summary statistics

mean_dist <- eudist %>%
  group_by(Period) %>%
  summarise(
    mean = mean(eu.dist, na.rm = TRUE),
    sd = sd(eu.dist, na.rm = TRUE),
  )

mean_dist <- as.data.frame(mean_dist)

## ---- Anova ----

d <- data.frame(y = eudist$eu.dist, period = eudist$Period)
m <- aov(y ~ period, data = d)
summary.lm(m)
par(mfrow = c(2,2))
plot(m) # Distribucion no es muy normal...
par(mfrow = c(1,1))
hist(d$y, breaks = 100)
m_dist <- summary(m)
tukey_dist <- TukeyHSD(m)

# Prove with logaritmic transformation
eudist$log_eudist <- log(eudist$eu.dist + 0.00001)
## ---- Anova ----
d <- data.frame(y = eudist$log_eudist, period = eudist$Period)
m <- aov(y ~ period, data = d)
summary.lm(m)
par(mfrow = c(2,2))
plot(m) # Distribucion mejora
par(mfrow = c(1,1))
hist(d$y, breaks = 100)
m_dist <- summary(m)
tukey_dist <- TukeyHSD(m)

## -------------------------------------------------
##         Proportion of change of field 
## ------------------------------------------------- 

campos <- read.table("D:/PhD/Fourth chapter/Results/Analisis3_bottleneck_effect/uso_campos.txt", header = T, dec = ",", sep = "\t")

# Summary statistics

change_loc <- campos %>%
  group_by(Period) %>%
  summarise(
    mean = mean(prop.cambios, na.rm = TRUE),
    sd = sd(prop.cambios, na.rm = TRUE))

change_loc <- as.data.frame(change_loc)

## ---- Anova ----

d <- data.frame(y = campos$prop.cambios, period = campos$Period)
m <- aov(y ~ period, data = d)
m_change <- summary(m)
tukey_change <- TukeyHSD(m)


## -------------------------------------------------
##                        MCP 
## ------------------------------------------------- 

mcp.hab <- read.table("D:/PhD/Fourth chapter/Results/Analisis3_bottleneck_effect/MCP_indiv_hab_avai.txt", header = T, dec = ",",
                      sep = "\t")
mcp.hab$MCP.area_ha <- mcp.hab$MCP.area/10000

# Summary statistics

mcp <- mcp.hab %>%
  group_by(Period) %>%
  summarise(
    mean = mean(MCP.area_ha, na.rm = TRUE),
    sd = sd(MCP.area_ha, na.rm = TRUE))

mcp <- as.data.frame(mcp)

## ---- Anova ----

d <- data.frame(y = mcp.hab$MCP.area, period = mcp.hab$Period)
m <- aov(y ~ period, data = d)
m_mcp <- summary(m)
tukey_mcp <- TukeyHSD(m)

## -------------------------------------------------
##                        FLYING
## ------------------------------------------------- 


d <- read.table("D:/PhD/Fourth chapter/GPS Cataluña/Ganga/Updated_24-2-2020/FINAL_ALLpos_no_regadio_ETRS89.txt", header = T, dec = ",",
                sep = "\t")

period <- c("Pre", "PreRep", "Rep")
fly <- list()

for (p in 1:length(period)){
  d_p <- d[which(d$period %in% period[p]), ]
  prop_fly <- (nrow(d_p[d_p$fly == 1, ])/nrow(d_p))*100
  fly[[p]] <- prop_fly
}


## ---- Nº flying positions per individual ----

data <- as.data.frame(matrix(NA, nrow = length(unique(d$Logger_ID)), ncol = 3))
rownames(data) <- unique(d$Logger_ID)


for (p in 1:length(period)){
  
  d_p <- d[which(d$period %in% period[p]), ]
  id <- unique(d_p$Logger_ID)
  
  for (i in 1:length(id)){
    
    d_p_id <- d_p[which(d_p$Logger_ID %in% id[i]), ]
    prop_fly <- round(((nrow(d_p_id[d_p_id$fly == 1, ]) + 0.0001)/nrow(d_p_id))*100, 3) # +0.00001 so that there is no error
    data[rownames(data) %in% id[i],p] <- prop_fly
  }
}

## ---- Mean and se ----

m_fly <- apply(data,2,mean, na.rm = TRUE)
sd_fly <- apply(data,2,sd, na.rm = TRUE)
mean_fly <- data.frame(Period = c("Pre", "PreRep", "Rep"), mean = m_fly, sd = sd_fly)


## ---- Anova ----

colnames(data) <- period
data2 <- gather(data, key = "Period", value = "Prop_fly")
data2$Period <- factor(data2$Period)
d <- data.frame(y = data2$Prop_fly, period = data2$Period)

d <- d[complete.cases(d), ]
m <- aov(y ~ period, data = d)
summary(m)
tukey_flying <- TukeyHSD(m)


## -------------------------------------------------
##                    Join
## ------------------------------------------------- 

mean_dist
m_dist 
tukey_dist2 <- data.frame(tukey_dist$period)
tukey_dist2 <- round(tukey_dist2,2)
tukey_dist2$CI <- paste(tukey_dist2$lwr,"-",tukey_dist2$upr, sep = "")
tukey_dist2 <- tukey_dist2[,c(1,5,4)]


change_loc
m_change 
tukey_change 
tukey_change2 <- data.frame(tukey_change$period)
tukey_change2 <- round(tukey_change2,2)
tukey_change2$CI <- paste(tukey_change2$lwr,"-",tukey_change2$upr, sep = "")
tukey_change2 <- tukey_change2[,c(1,5,4)]

mcp
m_mcp
tukey_mcp
tukey_mcp2 <- data.frame(tukey_mcp$period)
tukey_mcp2 <- round(tukey_mcp2,2)
tukey_mcp2$CI <- paste(tukey_mcp2$lwr,"-",tukey_mcp2$upr, sep = "")
tukey_mcp2 <- tukey_mcp2[,c(1,5,4)]

mean_fly
m_flying
tukey_flying
tukey_flying2 <- data.frame(tukey_flying$period)
tukey_flying2 <- round(tukey_flying2,2)
tukey_flying2$CI <- paste(tukey_flying2$lwr,"-",tukey_flying2$upr, sep = "")
tukey_flying2 <- tukey_flying2[,c(1,5,4)]

# 1. Save mean+sd

table1 <- cbind(mean_dist, change_loc, mcp, mean_fly)
table1 <- table1[,-c(1,4,7,10)]
table1 <- round(table1,2)
setwd("D:/PhD/Fourth chapter/Results/Analisis3_bottleneck_effect")
write.csv(table1, file = "Table_movement_est.csv")

#2. Save Tukey
table2 <- cbind(tukey_dist2, tukey_change2, tukey_mcp2, tukey_flying2)
setwd("D:/PhD/Fourth chapter/Results/Analisis3_bottleneck_effect")
write.csv(table2, file = "Table_movement_tukey.csv")
