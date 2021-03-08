
rm(list = ls())

library(dplyr)
library(rgdal)
library(tidyr)

stderr <- function(x, na.rm=FALSE) {
  if (na.rm) x <- na.omit(x)
  sqrt(var(x)/length(x))
}

setwd("D:/PhD/Fourth chapter/Results/Analisis3_bottleneck_effect")

## -------------------------------------------------
##                FIGURE MOVEMENTS
## ------------------------------------------------- 

## -------------------------------------------------
##     Distance between consecutive positions
## ------------------------------------------------- 

eudist <- read.table("D:/PhD/Fourth chapter/Results/Analisis3_bottleneck_effect/Euclidean_distances.txt", header = T, dec = ",", sep = "\t")

# Summary statistics

mean_dist <- eudist %>%
  group_by(Period) %>%
  summarise(
    count = n(),
    mean = mean(eu.dist, na.rm = TRUE),
    sd = sd(eu.dist, na.rm = TRUE),
    se = stderr(eu.dist, na.rm = TRUE)
  )

mean_dist <- as.data.frame(mean_dist)

setwd("D:/PhD/Fourth chapter/Results/Analisis3_bottleneck_effect")
pdf("Fig_movements.pdf", 7, 5)
par(mfrow = c(2,2),
    mar = c(2,2,2,0),
    oma = c(3,3,2,2))

plot(mean_dist$mean, ylim = c(200,400), xlim = c(0,4), axes = FALSE, xaxs="i", yaxs = "i",
     ylab = " ", xlab = " ", pch = 19)
axis(1, labels = c("PreBot", "Bot", "PostBot"), at = c(1,2,3))
axis(2, pos = 0.5)
mtext("Euclidean distance (m)", side = 2, line = 1, cex = 0.8)
x <- c(1,2,3)
segments(x, mean_dist$mean - mean_dist$se * 2, 
         x, mean_dist$mean + mean_dist$se * 2, 
         lwd = 1.5)
arrows(x, mean_dist$mean - mean_dist$se * 2, 
       x, mean_dist$mean + mean_dist$se * 2, 
       lwd = 1.5, angle = 90, code = 3, length = 0.05)

lines(mean_dist$mean, col = "blue")
#text(x = 1, y = 268.7975,labels = "*", cex = 2.5, col = "red")

## ---- Anova ----

d <- data.frame(y = eudist$eu.dist, period = eudist$Period)
m <- aov(y ~ period, data = d)
summary(m)
tukey <- TukeyHSD(m)
#plot(tukey)


## -------------------------------------------------
##         Proportion of change of field 
## ------------------------------------------------- 

campos <- read.table("D:/PhD/Fourth chapter/Results/Analisis3_bottleneck_effect/uso_campos.txt", header = T, dec = ",", sep = "\t")

# Summary statistics

change_loc <- campos %>%
  group_by(Period) %>%
  summarise(
    mean = mean(prop.cambios, na.rm = TRUE),
    sd = sd(prop.cambios, na.rm = TRUE),
    se = stderr(prop.cambios, na.rm = TRUE))

change_loc <- as.data.frame(change_loc)

plot(change_loc$mean, ylim = c(0,1), xlim = c(0,4), axes = FALSE, xaxs="i", yaxs = "i",
     ylab = " ", xlab = " ", pch = 19)
axis(1, labels = c("PreBot", "Bot", "PostBot"), at = c(1,2,3))
axis(2, pos = 0.5)
mtext("Field change rate (%)", side = 2, line = 1, cex = 0.8)
x <- c(1,2,3)
segments(x, change_loc$mean - change_loc$se * 2, 
         x, change_loc$mean + change_loc$se * 2, 
         lwd = 1.5)
arrows(x, change_loc$mean - change_loc$se * 2, 
       x, change_loc$mean + change_loc$se * 2, 
       lwd = 1.5, angle = 90, code = 3, length = 0.05)

lines(change_loc$mean, col = "blue")

## ---- Anova ----

d <- data.frame(y = campos$prop.cambios, period = campos$Period)
m <- aov(y ~ period, data = d)
summary(m)
tukey <- TukeyHSD(m)
#plot(tukey)


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
    sd = sd(MCP.area_ha, na.rm = TRUE),
    se = stderr(MCP.area_ha, na.rm = TRUE))

mcp <- as.data.frame(mcp)

plot(mcp$mean, ylim = c(0,1900), xlim = c(0,4), axes = FALSE, xaxs="i", yaxs = "i",
     ylab = " ", xlab = " ", pch = 19)
axis(1, labels = c("PreBot", "Bot", "PostBot"), at = c(1,2,3))
axis(2, pos = 0.5)
mtext("MCP area (ha)", side = 2, line = 1, cex = 0.8)
mtext("Period", side = 1, line = 2.5, cex = 0.8)

x <- c(1,2,3)
segments(x, mcp$mean - mcp$se * 2, 
         x, mcp$mean + mcp$se * 2, 
         lwd = 1.5)
arrows(x, mcp$mean - mcp$se * 2, 
       x, mcp$mean + mcp$se * 2, 
       lwd = 1.5, angle = 90, code = 3, length = 0.05)

lines(mcp$mean, col = "blue")

## ---- Anova ----

d <- data.frame(y = mcp.hab$MCP.area, period = mcp.hab$Period)
m <- aov(y ~ period, data = d)
summary(m)
tukey <- TukeyHSD(m)
#plot(tukey)

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
names(m_fly) <- period
stderr <- function(x, na.rm=FALSE) {
  if (na.rm) x <- na.omit(x)
  sqrt(var(x)/length(x))
}
se_fly <- apply(data,2,stderr, na.rm = TRUE)



plot(m_fly, ylim = c(0,6.5), xlim = c(0,4), axes = FALSE, xaxs="i", yaxs = "i",
     ylab = " ", xlab = " ", pch = 19)
axis(1, labels = c("PreBot", "Bot", "PostBot"), at = c(1,2,3))
axis(2, pos = 0.5)
mtext("Flying positions (%)", side = 2, line = 1, cex = 0.8)
mtext("Period", side = 1, line = 2.5, cex = 0.8)


x <- c(1,2,3)
segments(x, m_fly - se_fly * 2, 
         x,m_fly + se_fly * 2, 
         lwd = 1.5)
arrows(x, m_fly - se_fly * 2, 
       x, m_fly + se_fly * 2, 
       lwd = 1.5, angle = 90, code = 3, length = 0.05)

lines(m_fly, col = "blue")


## ---- Anova ----

colnames(data) <- period
data2 <- gather(data, key = "Period", value = "Prop_fly")
data2$Period <- factor(data2$Period)
d <- data.frame(y = data2$Prop_fly, period = data2$Period)

d <- d[complete.cases(d), ]
m <- aov(y ~ period, data = d)
summary(m)
TukeyHSD(m)
#plot(tukey)



dev.off()
