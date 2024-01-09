rm(list=ls())

library(dplyr)
library(rgdal)
library(tidyr)
library(emmeans)
library(nlme)

stderr <- function(x, na.rm=FALSE) {
  if (na.rm) x <- na.omit(x)
  sqrt(var(x)/length(x))
}

## ---- Load data ----

setwd("D:/PhD/Fourth chapter/Data/kernel_homerange_analyisis3")

# Load KDE estimates

models <- read.table("D:/PhD/Fourth chapter/Data/kernel_homerange_analyisis3/1_kernels_periodo_modelos_reddata.txt", header = T, dec = ",")
dat <- read.table("D:/PhD/Fourth chapter/Data/kernel_homerange_analyisis3/1_kernels_periodo_reddata.txt", header = T, dec = ",")

id <- data.frame(do.call('rbind', strsplit(as.character(dat$IND),'.',fixed=TRUE)))
kde <- cbind(dat,id)
colnames(kde)[c(8:10)] <- c("ID", "Period", "Year")
kde$Period <- as.factor(kde$Period)

# Transform to same units (The ones in ha to km2)

kde_units <- kde

kde_units[which(kde_units$unidades == "area (hectares)"), c(1,2,3)] <- kde_units[which(kde_units$unidades == "area (hectares)"), c(1,2,3)]/100
kde_units$unidades <- "area (square kilometers)"



# ID raros: CIP02.Pre.2016, CIP03.PreRep.2017, CIP04.PreRep.2017, CIP04.Rep.2017 (0.5 y 0.6), PIC17.PreRep.2018

## -------------------------------------------------
##                    K50
## ------------------------------------------------- 

kde50 <- kde_units[which(kde_units$vol == 0.50), ] # Area en km2

# Summary statistics

k <- kde50 %>%
  group_by(Period) %>%
  summarise(
    mean = mean(est),
    sd = sd(est),
    se = stderr(est))

k <- as.data.frame(k)

plot(k$mean, ylim = c(0,6), xlim = c(0,4), axes = FALSE, xaxs="i", yaxs = "i",
     ylab = " ", xlab = " ", pch = 19)
axis(1, labels = c(" ", "Short", "Tall", "Stubble"), at = c(0.5,1,2,3))
axis(2, pos = 0.5)
mtext("Area (km2)", side = 2, line = 1, cex = 0.8)
mtext("Period", side = 1, line = 2.5, cex = 0.8)

x <- c(1,2,3)
segments(x, k$mean - k$se * 2, 
         x, k$mean + k$se * 2, 
         lwd = 1.5)
arrows(x, k$mean - k$se * 2, 
       x, k$mean + k$se * 2, 
       lwd = 1.5, angle = 90, code = 3, length = 0.05)

lines(k$mean, col = "blue")

## ---- Anova ----

# Random ID

m1 <- lme(est ~ Period, random = ~ 1|ID,
          data = kde50)
anova(m1)
emmeans(m1, list(pairwise ~ Period), adjust = "tukey")

# Random year and ID
m2 <- lme(est ~ Period, random = list(ID = ~ 1, Year = ~ 1),
          data = kde50)
anova(m2)
emmeans(m2, list(pairwise ~ Period), adjust = "tukey")

AIC(m1,m2)

## -------------------------------------------------
##                    K95
## ------------------------------------------------- 

kde95 <- kde_units[which(kde_units$vol == 0.95), ] # Area en km2

# Summary statistics

k <- kde95 %>%
  group_by(Period) %>%
  summarise(
    mean = mean(est),
    sd = sd(est),
    se = stderr(est))

k <- as.data.frame(k)

plot(k$mean, ylim = c(0,30), xlim = c(0,4), axes = FALSE, xaxs="i", yaxs = "i",
     ylab = " ", xlab = " ", pch = 19)
axis(1, labels = c(" ", "Short", "Tall", "Stubble"), at = c(0.5,1,2,3))
axis(2, pos = 0.5)
mtext("Area (km2)", side = 2, line = 1, cex = 0.8)
mtext("Period", side = 1, line = 2.5, cex = 0.8)

x <- c(1,2,3)
segments(x, k$mean - k$se * 2, 
         x, k$mean + k$se * 2, 
         lwd = 1.5)
arrows(x, k$mean - k$se * 2, 
       x, k$mean + k$se * 2, 
       lwd = 1.5, angle = 90, code = 3, length = 0.05)

lines(k$mean, col = "blue")

## ---- Anova ----

# Random ID

m1 <- lme(est ~ Period, random = ~ 1|ID,
          data = kde95)
anova(m1)
emmeans(m1, list(pairwise ~ Period), adjust = "tukey")

# Random year and ID
m2 <- lme(est ~ Period, random = list(ID = ~ 1, Year = ~ 1),
          data = kde95)
anova(m2)
emmeans(m2, list(pairwise ~ Period), adjust = "tukey")

AIC(m1,m2)
