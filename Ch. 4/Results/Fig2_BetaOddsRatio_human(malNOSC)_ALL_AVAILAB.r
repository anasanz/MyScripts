## -------------------------------------------------
##           FIGURE ODDS RATIO ANALYSIS 1
## ------------------------------------------------- 

rm(list=ls())


library(dplyr)


period <- c("Pre", "PreRep", "Rep")
avail <- c("A1", "A2", "A3", "Aprop1", "Aprop2", "Aprop0.5", "Aprop0.25")


temp <- setwd("D:/PhD/Fourth chapter/Results/RSF_Ana/Analisis 1/results_rspf")
temp <- list.files(pattern = "*.csv")
data <- lapply(temp, read.csv, sep = ";")

#for(xxx in 1:length(avail)){ 
xxx = 1
file <- grep(avail[xxx], temp) # Files with one type of availability sampling
d <- data[file]
d_names <- temp[file]

p1 <- d[[1]]
p1 <- p1[,-5]
p1$period <- "p1"
p1 <- p1[nrow(p1):1, ]
p1[,1] <- c("DistAsp", "Slope", "DistGrav", "NatVeg", "Forest", "Herb.irri", "Fruit.irri", "Almond", "Olive", "Cereal", "Fallow", "Intercept")

p2 <- d[[2]]
p2 <- p2[,-5]
p2$period <- "p2"
p2 <- p2[nrow(p2):1, ]
p2[,1] <- c("DistAsp", "Slope", "DistGrav", "NatVeg", "Forest", "Herb.irri", "Fruit.irri", "Almond", "Olive", "Cereal", "Fallow", "Intercept")

p3 <- d[[3]]
p3 <- p3[,-5]
p3$period <- "p3"
p3 <- p3[nrow(p3):1, ]
p3[,1] <- c("DistAsp", "Slope", "DistGrav", "NatVeg", "Forest", "Herb.irri", "Fruit.irri", "Almond", "Olive", "Cereal", "Fallow", "Intercept")


allperiods <- rbind(p1,p2,p3)
colnames(allperiods) <- c("X", "Estimate", "SE", "p", "period")
p <- c("p1","p2","p3")

# Calculate Confint
allperiods$lower <- NA
allperiods$upper <- NA

for (i in 1:length(p)){
  data.period <- allperiods[which(allperiods$period == p[i]), ]
  lower <- data.period$Estimate - 1.96 * data.period$SE
  upper <- data.period$Estimate + 1.96 * data.period$SE
  allperiods$lower[which(allperiods$period == p[i])] <- lower
  allperiods$upper[which(allperiods$period == p[i])] <- upper
}
## -------------------------------------------------
##             Distancia a carreteras
## ------------------------------------------------- 


## ---- Generate predicted values for continuous variables ----

avail <- c("A1", "A2", "A3", "Aprop1", "Aprop2", "Aprop0.5", "Aprop0.25")

temp <- setwd("D:/PhD/Fourth chapter/Results/RSF_Ana/Analisis 1/data_matrix")
temp <- list.files(pattern = "*.csv")
data <- lapply(temp, read.csv, sep = ",")

#for(xxx in 1:length(avail)){ 
xxx = 1
file <- grep(avail[xxx], temp) # Files with one type of availability sampling
d <- data[file]
d_names <- temp[file]

datos <- d[[1]]

d <- datos[ ,c(10:20)] # Remove NA
datos <- datos[complete.cases(d), ]

# Inverse of logit to back-transform 
inv.logit <- function(inValues) {
  1.0 / (1.0 + exp(-inValues))}


p_data <- c("Pre", "PreRep", "Rep")
p_coef <- c("p1", "p2", "p3")
titles <- c("Pre-Bottleneck", "Bottleneck", "Post-Bottleneck")

setwd("D:/PhD/Fourth chapter/Results/Figures/Fig2_continuous/A1/Odds ratio")

pdf("Distance_asp.pdf", 9, 4)
par(mfrow = c(1,3))

for (i in 1:length(p_data)){
  
  datos_period <- datos[which(datos$periodo == p_data[i]), ]
  
  newdata_asp <- as.data.frame(lapply(lapply(datos_period[, c(10:19,21)], mean), rep, 250))
  newdata_asp$asp_new <- seq(min(datos_period$carreteras), max(datos_period$carreteras), length.out = 250) # Aqui REMOVE na
  
  p <- allperiods[which(allperiods$period == p_coef[i] ), ]
  
  pred <- p[p$X == 'Intercept',2] * newdata_asp$otherhersec + 
                      p[p$X == 'Fallow',2] * newdata_asp$barbecho + 
                      p[p$X == 'Cereal',2] * newdata_asp$cereal + 
                      p[p$X == 'Olive',2] * newdata_asp$olivo + 
                      p[p$X == 'Almond',2] * newdata_asp$almendro + 
                      p[p$X == 'Fruit.irri',2] * newdata_asp$frutreg + 
                      p[p$X == 'Herb.irri',2] * newdata_asp$herreg + 
                      p[p$X == 'Forest',2] * newdata_asp$forestal + 
                      p[p$X == 'NatVeg',2] * newdata_asp$vegnat + 
                      p[p$X == 'DistGrav',2] * newdata_asp$caminos + 
                      p[p$X == 'Slope',2] * newdata_asp$Slope + 
                      p[p$X == 'DistAsp',2] * newdata_asp$asp_new
  
  
  pred_lci <- p[p$X == 'Intercept',6] * newdata_asp$otherhersec + 
                          p[p$X == 'Fallow',6] * newdata_asp$barbecho + 
                          p[p$X == 'Cereal',6] * newdata_asp$cereal + 
                          p[p$X == 'Olive',6] * newdata_asp$olivo + 
                          p[p$X == 'Almond',6] * newdata_asp$almendro + 
                          p[p$X == 'Fruit.irri',6] * newdata_asp$frutreg + 
                          p[p$X == 'Herb.irri',6] * newdata_asp$herreg + 
                          p[p$X == 'Forest',6] * newdata_asp$forestal + 
                          p[p$X == 'NatVeg',6] * newdata_asp$vegnat + 
                          p[p$X == 'DistGrav',6] * newdata_asp$caminos + 
                          p[p$X == 'Slope',6] * newdata_asp$Slope + 
                          p[p$X == 'DistAsp',6] * newdata_asp$asp_new
  
  pred_uci <- p[p$X == 'Intercept',7] * newdata_asp$otherhersec + 
                          p[p$X == 'Fallow',7] * newdata_asp$barbecho + 
                          p[p$X == 'Cereal',7] * newdata_asp$cereal + 
                          p[p$X == 'Olive',7] * newdata_asp$olivo + 
                          p[p$X == 'Almond',7] * newdata_asp$almendro + 
                          p[p$X == 'Fruit.irri',7] * newdata_asp$frutreg + 
                          p[p$X == 'Herb.irri',7] * newdata_asp$herreg + 
                          p[p$X == 'Forest',7] * newdata_asp$forestal + 
                          p[p$X == 'NatVeg',7] * newdata_asp$vegnat + 
                          p[p$X == 'DistGrav',7] * newdata_asp$caminos + 
                          p[p$X == 'Slope',7] * newdata_asp$Slope + 
                          p[p$X == 'DistAsp',7] * newdata_asp$asp_new
  
  plot(-15, ylim=c(min(pred_lci),max(pred_uci)),xlim = c(min(newdata_asp$asp_new), max(newdata_asp$asp_new)), type="l", main = titles[i], xlab = "Distance to asphalted roads (log)", ylab = "Odds ratio")
  
  polygon( x = c(newdata_asp$asp_new, rev(newdata_asp$asp_new)),
           y = c(pred_lci, rev(pred_uci)), 
           col = adjustcolor(c("grey"),alpha.f = 0.6),
           border = NA)
  points(pred ~ newdata_asp$asp_new, type = "l")
  
  
}

dev.off()


## -------------------------------------------------
##             Distancia a caminos
## ------------------------------------------------- 


## ---- Generate predicted values for continuous variables ----

#for(xxx in 1:length(avail)){ 
xxx = 1
file <- grep(avail[xxx], temp) # Files with one type of availability sampling
d <- data[file]
d_names <- temp[file]

datos <- d[[1]]

d <- datos[ ,c(10:20)] # Remove NA
datos <- datos[complete.cases(d), ]


# Inverse of logit to back-transform 
inv.logit <- function(inValues) {
  1.0 / (1.0 + exp(-inValues))}


p_data <- c("Pre", "PreRep", "Rep")
p_coef <- c("p1", "p2", "p3")
titles <- c("Pre-Bottleneck", "Bottleneck", "Post-Bottleneck")

setwd("D:/PhD/Fourth chapter/Results/Figures/Fig2_continuous/A1/Odds ratio")

pdf("Distance_grav.pdf", 9, 4)
par(mfrow = c(1,3))

for (i in 1:length(p_data)){
  
  datos_period <- datos[which(datos$periodo == p_data[i]), ]
  
  newdata_grav <- as.data.frame(lapply(lapply(datos_period[, c(10:20)], mean), rep, 250))
  newdata_grav$grav_new <- seq(min(datos_period$caminos), max(datos_period$caminos), length.out = 250)
  
  p <- allperiods[which(allperiods$period == p_coef[i] ), ]
  
  pred <- p[p$X == 'Intercept',2] * newdata_grav$otherhersec + 
                      p[p$X == 'Fallow',2] * newdata_grav$barbecho + 
                      p[p$X == 'Cereal',2] * newdata_grav$cereal + 
                      p[p$X == 'Olive',2] * newdata_grav$olivo + 
                      p[p$X == 'Almond',2] * newdata_grav$almendro + 
                      p[p$X == 'Fruit.irri',2] * newdata_grav$frutreg + 
                      p[p$X == 'Herb.irri',2] * newdata_grav$herreg + 
                      p[p$X == 'Forest',2] * newdata_grav$forestal + 
                      p[p$X == 'NatVeg',2] * newdata_grav$vegnat + 
                      p[p$X == 'DistGrav',2] * newdata_grav$grav_new + 
                      p[p$X == 'Slope',2] * newdata_grav$Slope +  
                      p[p$X == 'DistAsp',2] * newdata_grav$carreteras
  
  
  #pred + inv.logit(p[p$X == 'DistAsp',2] * newdata_grav$carreteras)
  # PERODO 3: The "problem" is that the distance to asphalted road is already very high
  
  pred_lci <- p[p$X == 'Intercept',6] * newdata_grav$otherhersec + 
                          p[p$X == 'Fallow',6] * newdata_grav$barbecho + 
                          p[p$X == 'Cereal',6] * newdata_grav$cereal + 
                          p[p$X == 'Olive',6] * newdata_grav$olivo + 
                          p[p$X == 'Almond',6] * newdata_grav$almendro + 
                          p[p$X == 'Fruit.irri',6] * newdata_grav$frutreg + 
                          p[p$X == 'Herb.irri',6] * newdata_grav$herreg + 
                          p[p$X == 'Forest',6] * newdata_grav$forestal + 
                          p[p$X == 'NatVeg',6] * newdata_grav$vegnat + 
                          p[p$X == 'DistGrav',6] * newdata_grav$grav_new + 
                          p[p$X == 'Slope',6] * newdata_grav$Slope + 
                          p[p$X == 'DistAsp',6] * newdata_grav$carreteras
  
  pred_uci <- p[p$X == 'Intercept',7] * newdata_grav$otherhersec + 
                          p[p$X == 'Fallow',7] * newdata_grav$barbecho + 
                          p[p$X == 'Cereal',7] * newdata_grav$cereal + 
                          p[p$X == 'Olive',7] * newdata_grav$olivo + 
                          p[p$X == 'Almond',7] * newdata_grav$almendro + 
                          p[p$X == 'Fruit.irri',7] * newdata_grav$frutreg + 
                          p[p$X == 'Herb.irri',7] * newdata_grav$herreg + 
                          p[p$X == 'Forest',7] * newdata_grav$forestal + 
                          p[p$X == 'NatVeg',7] * newdata_grav$vegnat + 
                          p[p$X == 'DistGrav',7] * newdata_grav$grav_new + 
                          p[p$X == 'Slope',7] * newdata_grav$Slope + 
                          p[p$X == 'DistAsp',7] * newdata_grav$carreteras
  
  plot(-15, ylim=c(min(pred_lci),max(pred_uci)),xlim = c(0, max(newdata_grav$grav_new)), type="l", main = titles[i], xlab = "Distance to gravel roads (log)", ylab = "Odds ratio")
  
  polygon( x = c(newdata_grav$grav_new, rev(newdata_grav$grav_new)),
           y = c(pred_lci, rev(pred_uci)), 
           col = adjustcolor(c("grey"),alpha.f = 0.6),
           border = NA)
  points(pred ~ newdata_grav$grav_new, type = "l")
  
}

dev.off()

## -------------------------------------------------
##             Slope
## ------------------------------------------------- 


## ---- Generate predicted values for continuous variables ----

xxx = 1
file <- grep(avail[xxx], temp) # Files with one type of availability sampling
d <- data[file]
d_names <- temp[file]

datos <- d[[1]]

d <- datos[ ,c(10:20)] # Remove NA
datos <- datos[complete.cases(d), ]


# Inverse of logit to back-transform 
inv.logit <- function(inValues) {
  1.0 / (1.0 + exp(-inValues))}


p_data <- c("Pre", "PreRep", "Rep")
p_coef <- c("p1", "p2", "p3")
titles <- c("Pre-Bottleneck", "Bottleneck", "Post-Bottleneck")

setwd("D:/PhD/Fourth chapter/Results/Figures/Fig2_continuous/A1/Odds ratio")

pdf("Slope.pdf", 9, 4)
par(mfrow = c(1,3))

for (i in 1:length(p_data)){
  
  datos_period <- datos[which(datos$periodo == p_data[i]), ]
  
  newdata_slope <- as.data.frame(lapply(lapply(datos_period[, c(10:18,20,21)], mean), rep, 250))
  newdata_slope$slope_new <- seq(min(datos_period$Slope), max(datos_period$Slope), length.out = 250)
  
  p <- allperiods[which(allperiods$period == p_coef[i] ), ]
  
  pred <- p[p$X == 'Intercept',2] * newdata_slope$otherhersec + 
                      p[p$X == 'Fallow',2] * newdata_slope$barbecho + 
                      p[p$X == 'Cereal',2] * newdata_slope$cereal + 
                      p[p$X == 'Olive',2] * newdata_slope$olivo + 
                      p[p$X == 'Almond',2] * newdata_slope$almendro + 
                      p[p$X == 'Fruit.irri',2] * newdata_slope$frutreg + 
                      p[p$X == 'Herb.irri',2] * newdata_slope$herreg + 
                      p[p$X == 'Forest',2] * newdata_slope$forestal + 
                      p[p$X == 'NatVeg',2] * newdata_slope$vegnat + 
                      p[p$X == 'DistGrav',2] * newdata_slope$caminos + 
                      p[p$X == 'Slope',2] * newdata_slope$slope_new + 
                      p[p$X == 'DistAsp',2] * newdata_slope$carreteras
  
  pred_lci <- p[p$X == 'Intercept',6] * newdata_slope$otherhersec + 
                          p[p$X == 'Fallow',6] * newdata_slope$barbecho + 
                          p[p$X == 'Cereal',6] * newdata_slope$cereal + 
                          p[p$X == 'Olive',6] * newdata_slope$olivo + 
                          p[p$X == 'Almond',6] * newdata_slope$almendro + 
                          p[p$X == 'Fruit.irri',6] * newdata_slope$frutreg + 
                          p[p$X == 'Herb.irri',6] * newdata_slope$herreg + 
                          p[p$X == 'Forest',6] * newdata_slope$forestal + 
                          p[p$X == 'NatVeg',6] * newdata_slope$vegnat + 
                          p[p$X == 'DistGrav',6] * newdata_slope$caminos + 
                          p[p$X == 'Slope',6] * newdata_slope$slope_new + 
                          p[p$X == 'DistAsp',6] * newdata_slope$carreteras
  
  pred_uci <- p[p$X == 'Intercept',7] * newdata_slope$otherhersec + 
                          p[p$X == 'Fallow',7] * newdata_slope$barbecho + 
                          p[p$X == 'Cereal',7] * newdata_slope$cereal + 
                          p[p$X == 'Olive',7] * newdata_slope$olivo + 
                          p[p$X == 'Almond',7] * newdata_slope$almendro + 
                          p[p$X == 'Fruit.irri',7] * newdata_slope$frutreg + 
                          p[p$X == 'Herb.irri',7] * newdata_slope$herreg + 
                          p[p$X == 'Forest',7] * newdata_slope$forestal + 
                          p[p$X == 'NatVeg',7] * newdata_slope$vegnat + 
                          p[p$X == 'DistGrav',7] * newdata_slope$caminos + 
                          p[p$X == 'Slope',7] * newdata_slope$slope_new + 
                          p[p$X == 'DistAsp',7] * newdata_slope$carreteras
  
  plot(-15, ylim=c(min(pred_lci),max(pred_uci)),xlim = c(min(datos_period$Slope), max(datos_period$Slope)), type="l", main = titles[i], xlab = "Slope", ylab = "Odds ratio")
  
  polygon( x = c(newdata_slope$slope_new, rev(newdata_slope$slope_new)),
           y = c(pred_lci, rev(pred_uci)), 
           col = adjustcolor(c("grey"),alpha.f = 0.6),
           border = NA)
  points(pred ~ newdata_slope$slope_new, type = "l")
  
}

dev.off()


