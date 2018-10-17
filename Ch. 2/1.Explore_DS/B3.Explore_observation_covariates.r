
rm(list=ls())

library(rgdal)
library(dplyr)
library(tidyr)
library(sp)

setwd("C:/Users/Ana/Documents/PhD/Second chapter/Data")
dat <- read.csv("DataDS_ready.csv")


# ---- All Species ----

# All sp - Temperature
dat_temp <- dat[-which(dat$Temp > 40),]
hist(dat_temp$Temp, main = "All sp - Temperature") # 15ºC looks like the best

# All sp - Distance with different winds

xtabs(~Wind, dat)

par(mfrow = c(3,3),
    oma = c(3,3,4,1) + 0.1,
    mar = c(2,2,2,0) + 0.1)

w0 <- dat[which(dat$Wind == 0), ]
hist(w0$distance, breaks = c(0,25,50,99,200), xlab = "Distance bins (x)", col = "grey", main = "All sp - Wind 0",
     freq = FALSE)

w1 <- dat[which(dat$Wind == 1), ]
hist(w1$distance, breaks = c(0,25,50,99,200), xlab = "Distance bins (x)", col = "grey", main = "All sp - Wind 1",
     freq = FALSE) 

w2 <- dat[which(dat$Wind == 2), ]
hist(w2$distance, breaks = c(0,25,50,99,200), xlab = "Distance bins (x)", col = "grey", main = "All sp - Wind 2",
     freq = FALSE)

w3 <- dat[which(dat$Wind == 3), ]
hist(w3$distance, breaks = c(0,25,50,99,200), xlab = "Distance bins (x)", col = "grey", main = "All sp - Wind 3",
     freq = FALSE)

w4 <- dat[which(dat$Wind == 4), ]
hist(w4$distance, breaks = c(0,25,50,99,200), xlab = "Distance bins (x)", col = "grey", main = "All sp - Wind 4",
     freq = FALSE)

w5 <- dat[which(dat$Wind == 5), ]
hist(w5$distance, breaks = c(0,25,50,99,200), xlab = "Distance bins (x)", col = "grey", main = "All sp - Wind 5",
     freq = FALSE)

w6 <- dat[which(dat$Wind == 5), ]
hist(w5$distance, breaks = c(0,25,50,99,200), xlab = "Distance bins (x)", col = "grey", main = "All sp - Wind 6",
     freq = FALSE)

# The number of detections slightly decreases for all distances except the 1sr bin


