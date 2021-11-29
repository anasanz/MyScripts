
rm(list = ls())

library(rgdal)
library(dplyr)
library(tidyr)

setwd("D:/PhD/Fourth chapter/GPS Cataluña/Ganga/Updated_24-2-2020")
d <- readOGR("D:/PhD/Fourth chapter/GPS Cataluña/Ganga/Updated_24-2-2020", "XYgps_positions_with_flying_no_regadio")
d <- d@data
write.csv(d, file = )

## ---- Divide periods ----

d$period <- 0

# Pre-bottleneck (1st December – 25th February) 
d$period[d$Month %in% c(12,1,2)] <- "PreBot" # There is no positions from 25th feb

# Bottleneck (8th March – 31st May)
d$period[d$Month %in% c(3,4,5)] <- "Bot"
d$period[which(d$Month == 3 & d$Day < 8)] <- 0

# Post-Bottleneck (10th June – 31st August)
d$period[d$Month %in% c(6,7,8)] <- "PostBot"
d$period[which(d$Month == 6 & d$Day < 10)] <- 0

## ---- Subset periods -> Nº flying positions ----

d <- d[-which(d$period == 0), ] # Son mas que en el analisis potque estan las posiciones volando + posiciones en el regadío
d_fly <- d[which(d$fly == 1), ] # 858 volando

period <- c("PreBot", "Bot", "PostBot")
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
     ylab = "% flying positions", xlab = "Period", pch = 19)
axis(1, labels = c(" ", "PreBot", "Bot", "PostBot"), at = c(0,1,2,3))
axis(2)
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
plot(tukey)

