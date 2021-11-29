
rm(list = ls())

library(dplyr)
library(rgdal)
library(tidyr)
library(emmeans)

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
pdf("Fig_movements_lmm_groups.pdf", 8, 6)
par(mfrow = c(2,2),
    mar = c(2,2,2,0),
    oma = c(3,3,2,2))

plot(mean_dist$mean, ylim = c(200,400), xlim = c(0,4), axes = FALSE, xaxs="i", yaxs = "i",
     ylab = " ", xlab = " ", pch = 19)
axis(1, labels = c(" ", "Short", "Tall", "Stubble"), at = c(0.5,1,2,3))
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


## ---- Anova ----

# Random ID
eudist$log_eudist <- log(eudist$eu.dist + 0.00001)
m1 <- lme(log_eudist ~ Period, random = ~ 1|Bird.ID,
          data = eudist)
anova(m1)
emmeans(m1, list(pairwise ~ Period), adjust = "tukey")

# Random year and ID
m2 <- lme(log_eudist ~ Period, random = list(Bird.ID = ~ 1, Year = ~ 1),
          data = eudist)
anova(m2)
emmeans(m2, list(pairwise ~ Period), adjust = "tukey")

AIC(m1,m2)

# ---- Add signigfficant catgegories
pos_letter <- mean_dist$mean + mean_dist$se * 2 + 15
text(x = 1, y = pos_letter[1],labels = "A", cex = 1.5, col = "black")
text(x = 2, y = pos_letter[2],labels = "B", cex = 1.5, col = "black")
text(x = 3, y = pos_letter[3],labels = "B", cex = 1.5, col = "black")



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
axis(1, labels = c(" ", "Short", "Tall", "Stubble"), at = c(0.5,1,2,3))
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

# Random ID

m1 <- lme(prop.cambios ~ Period, random = ~ 1|Bird.ID,
          data = campos)
anova(m1)
emmeans(m1, list(pairwise ~ Period), adjust = "tukey")

# Random year and ID
m2 <- lme(prop.cambios ~ Period, random = list(Bird.ID = ~ 1, Year = ~ 1),
          data = campos)
anova(m2)
emmeans(m2, list(pairwise ~ Period), adjust = "tukey")

AIC(m1,m2)

# ---- Add signigfficant catgegories
pos_letter <- change_loc$mean + change_loc$se * 2 + 0.10
text(x = 1, y = pos_letter[1],labels = "A", cex = 1.5, col = "black")
text(x = 2, y = pos_letter[2],labels = "A", cex = 1.5, col = "black")
text(x = 3, y = pos_letter[3],labels = "B", cex = 1.5, col = "black")

pos_letter2 <- change_loc$mean + change_loc$se * 2 + 0.20
text(x = 2, y = pos_letter2[2],labels = "B", cex = 1.5, col = "black")



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
axis(1, labels = c(" ", "Short", "Tall", "Stubble"), at = c(0.5,1,2,3))
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

# Random ID

m1 <- lme(MCP.area ~ Period, random = ~ 1|Bird.ID,
          data = mcp.hab)
anova(m1)
emmeans(m1, list(pairwise ~ Period), adjust = "tukey")

# Random year and ID
m2 <- lme(MCP.area ~ Period, random = list(Bird.ID = ~ 1, Year = ~ 1),
          data = mcp.hab)
anova(m2)
emmeans(m2, list(pairwise ~ Period), adjust = "tukey")

AIC(m1,m2)

## -------------------------------------------------
##                        Flying  
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
names(m_fly) <- period
stderr <- function(x, na.rm=FALSE) {
  if (na.rm) x <- na.omit(x)
  sqrt(var(x)/length(x))
}
se_fly <- apply(data,2,stderr, na.rm = TRUE)

plot(m_fly, ylim = c(0,6.5), xlim = c(0,4), axes = FALSE, xaxs="i", yaxs = "i",
     ylab = " ", xlab = " ", pch = 19)
axis(1, labels = c(" ", "Short", "Tall", "Stubble"), at = c(0.5,1,2,3))
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


## ---- Nº flying positions per individual and year ----
d$Year[which(d$Year == 2016)] <- 2017

data_year <- list()
id <- unique(d_p$Logger_ID)
y <- c(2017,2018,2019)

  for (x in 1:length(y)){
    
    data <- as.data.frame(matrix(NA, nrow = length(unique(d$Logger_ID)), ncol = 3))
    rownames(data) <- unique(d$Logger_ID)
    
    d_p_year <- d[which(d$Year %in% y[x]), ]
    
    for (p in 1:length(period)){
      
      d_p <- d_p_year[which(d_p_year$period %in% period[p]), ]
    
      for (i in 1:length(id)){
      
      d_p_id <- d_p[which(d_p$Logger_ID %in% id[i]), ]
      if(nrow(d_p_id) == 0) next
      prop_fly <- round(((nrow(d_p_id[d_p_id$fly == 1, ]) + 0.0001)/nrow(d_p_id))*100, 3) # +0.00001 so that there is no error
      data[rownames(data) %in% id[i],p] <- prop_fly
      }
    }
  data$ID <- rownames(data)
  data_year[[x]] <- data
}

data_year <- lapply(data_year, setNames, nm = c(period,"ID"))
data_year <- lapply(data_year, gather, Period, proportion, Pre:Rep)

data_year[[1]]$Year <- 2017
data_year[[2]]$Year <- 2018
data_year[[3]]$Year <- 2019

fly_ID_year <- do.call(rbind, data_year)


## ---- Anova ----

# Random ID
fly_ID_year <- fly_ID_year[complete.cases(fly_ID_year), ]

m1 <- lme(proportion ~ Period, random = ~ 1|ID,
          data = fly_ID_year)
anova(m1)
emmeans(m1, list(pairwise ~ Period), adjust = "tukey")

# Random year and ID
m2 <- lme(proportion ~ Period, random = list(ID = ~ 1, Year = ~ 1),
          data = fly_ID_year)
anova(m2)
emmeans(m2, list(pairwise ~ Period), adjust = "tukey")

AIC(m1,m2)




dev.off()
