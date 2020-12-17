## -------------------------------------------------
##           Plot interaction ANALISIS 2
## ------------------------------------------------- 

rm(list=ls())

library(dplyr)


temp <- setwd("D:/PhD/Fourth chapter/Results/RSF_Ana/Analisis 2/results_prov_rspf/For plotting")

p1 <- read.csv("St2_Pre.csv", sep = ";")
p2 <- read.csv("St2_PreRep.csv", sep = ";")
p3 <- read.csv("St2_Rep.csv", sep = ";")


p1 <- p1[,-5]
p1$period <- "p1"
p1 <- p1[nrow(p1):1, ]
p1[,1] <- c("Manag*Lin", "Slope", "DistLin", "Manag", "No.Manag", "Intercept")

p2 <- p2[,-5]
p2$period <- "p2"
p2 <- p2[nrow(p2):1, ]
p2[,1] <- c("Manag*Lin", "Slope", "DistLin", "Manag", "No.Manag", "Intercept")

p3 <- p3[,-5]
p3$period <- "p3"
p3 <- p3[nrow(p3):1, ]
p3[,1] <- c("Manag*Lin", "Slope", "DistLin", "Manag", "No.Manag", "Intercept")


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
##            Interaction Manag*Lin
## ------------------------------------------------- 


## ---- Generate predicted values for continuous variables ----

setwd("D:/PhD/Fourth chapter/Results/RSF_Ana/Analisis 2/data_matrix")
datos <- read.csv("dm_random_loc_A1_ANALISIS2.csv")

d <- datos[ ,c(10:18)] # Remove NA
datos <- datos[complete.cases(d), ]

# Inverse of logit to back-transform 
inv.logit <- function(inValues) {
  1.0 / (1.0 + exp(-inValues))}


p_data <- c("Pre", "PreRep", "Rep")
p_coef <- c("p1", "p2", "p3")
titles <- c("Pre-Bottleneck", "Bottleneck", "Post-Bottleneck")


## -------------------------------------------------
##           Distance to linear features
## ------------------------------------------------- 

 par(mfrow = c(1,3))

for (i in 1:length(p_data)){

  datos_period <- datos[which(datos$periodo == p_data[i]), ]
  
  # Use the SCALED co-variates from the model (I scale again because I didn't save them but are exactly the same values than introduced in the model)
  
  mean.lin <- mean(datos_period$linear)
  sd.lin <- sd(datos_period$linear)
  datos_period$linear.st <- (datos_period$linear - mean.lin) / sd.lin
  
  mean.slop <- mean(datos_period$Slope)
  sd.slop <- sd(datos_period$Slope)
  datos_period$slope.st <- (datos_period$Slope - mean.slop) / sd.slop
  
  newdat.1 <- expand.grid(linear.st = seq(min(datos_period$linear.st), max(datos_period$linear.st), length.out = 250),
                          slope.st = mean(datos_period$slope.st))
  
  p <- allperiods[which(allperiods$period == p_coef[i] ), ]
  
  # When categorical (management) variable es 0
  
  pred1 <- inv.logit(p[p$X == 'Intercept',2] +
                      p[p$X == 'No.Manag',2] * 0 + 
                      p[p$X == 'Manag',2] * 0 + # When categorical variable is 0 (no manag)
                      p[p$X == 'DistLin',2] * newdat.1$linear.st + 
                      p[p$X == 'Slope',2] * newdat.1$slope.st + 
                      p[p$X == 'Manag*Lin',2] * newdat.1$linear.st * 0 ) # When categorical variable is 0 (no manag)
  
  pred_lci1 <- inv.logit(p[p$X == 'Intercept',6] +
                      p[p$X == 'No.Manag',6] * 0 +
                      p[p$X == 'Manag',6] * 0 + # When categorical variable is 0 (no manag)
                      p[p$X == 'DistLin',6] * newdat.1$linear.st + 
                      p[p$X == 'Slope',6] * newdat.1$slope.st + 
                      p[p$X == 'Manag*Lin',6] * newdat.1$linear.st * 0 ) # When categorical variable is 0 (no manag)
  
  
  pred_uci1 <- inv.logit(p[p$X == 'Intercept',7] +
                      p[p$X == 'No.Manag',7] * 0 +
                      p[p$X == 'Manag',7] * 0 + # When categorical variable is 0 (no manag)
                      p[p$X == 'DistLin',7] * newdat.1$linear.st + 
                      p[p$X == 'Slope',7] * newdat.1$slope.st + 
                      p[p$X == 'Manag*Lin',7] * newdat.1$linear.st * 0 ) # When categorical variable is 0 (no manag)
  
  # When categorical variable es 1

  pred2 <- inv.logit(p[p$X == 'Intercept',2] + 
                       p[p$X == 'No.Manag',2] * 0 + 
                       p[p$X == 'Manag',2] * 1 + # When categorical variable is 1 (manag)
                       p[p$X == 'DistLin',2] * newdat.1$linear.st + 
                       p[p$X == 'Slope',2] * newdat.1$slope.st + 
                       p[p$X == 'Manag*Lin',2] * newdat.1$linear.st * 1 )
  
  pred_lci2 <- inv.logit(p[p$X == 'Intercept',6] +
                           p[p$X == 'No.Manag',6] * 0 + 
                           p[p$X == 'Manag',6] * 1 + # When categorical variable is 1 (manag)
                           p[p$X == 'DistLin',6] * newdat.1$linear.st + 
                           p[p$X == 'Slope',6] * newdat.1$slope.st + 
                           p[p$X == 'Manag*Lin',6] * newdat.1$linear.st * 1 ) # When categorical variable is 0 (no manag)
  
  
  pred_uci2 <- inv.logit(p[p$X == 'Intercept',7] + 
                           p[p$X == 'No.Manag',7] * 0 + 
                           p[p$X == 'Manag',7] * 1 + # When categorical variable is 1 (manag)
                           p[p$X == 'DistLin',7] * newdat.1$linear.st + 
                           p[p$X == 'Slope',7] * newdat.1$slope.st + 
                           p[p$X == 'Manag*Lin',7] * newdat.1$linear.st * 1 ) # When categorical variable is 0 (no manag)
  
  # Unscale linear.st to plot it
  
  newdat.1$lin_unscaled <- newdat.1$linear.st*sd.lin + mean.lin
  
  plot(-15, ylim=c(0,1),xlim = c(0, max(newdat.1$lin_unscaled)), type="l", main = "", xlab = "Distance to human linear features (log)", ylab = "Probability of presence")
  
  #polygon( x = c(newdata_inter$lin_new_unscaled, rev(newdata_inter$lin_new_unscaled)),
  #         y = c(pred_lci1, rev(pred_uci1)), 
  #         col = adjustcolor(c("grey"),alpha.f = 0.6),
  #         border = NA)
  points(datos_period$STATUS ~ datos_period$linear, col = "grey")
  points(pred1 ~ newdat.1$lin_unscaled, type = "p")
  points(pred2 ~ newdat.1$lin_unscaled, type = "p", col = "red")
  
  
}

# Probabilidades predichas super bajas, incluso cuando en la interacción se incluye la variable categorica (en rojo)
# Alomejor es por lo que dices que al ser la variable dummy no lo estamos representando bien?
# Aunque en el link que te he mandado lo hace asi tambien..

# He hecho el mismo modelo en un GLM con las mismas variables. El efecto es parecido pero el intercepto sigue siendo
# mucho mas alto, me pregunto si en las rspf hay algo que nos estamos perdiendo...o tambien en el glm se comparan
# todos los puntos a la vez, puede que tenga que ver con eso.

# GLM CON LAS MISMAS VARIABLES

par(mfrow = c(1,3))

for (i in 1:length(p_data)){
  
  datos_period <- datos[which(datos$periodo == p_data[i]), ]

# Use the SCALED co-variates from the model (I scale again because I didn't save them but are exactly the same values than introduced in the model)

mean.lin <- mean(datos_period$linear)
sd.lin <- sd(datos_period$linear)
datos_period$linear.st <- (datos_period$linear - mean.lin) / sd.lin

mean.slop <- mean(datos_period$Slope)
sd.slop <- sd(datos_period$Slope)
datos_period$slope.st <- (datos_period$Slope - mean.slop) / sd.slop

newdat.1 <- expand.grid(linear.st = seq(min(datos_period$linear.st), max(datos_period$linear.st), length.out = 250),
                        slope.st = mean(datos_period$slope.st))

#GLM

 x1 <- glm(datos_period$STATUS~datos_period$si_manag*datos_period$linear.st + datos_period$no_manag + datos_period$slope.st , family = "binomial")
 
 pred1.1 <- inv.logit(x1$coefficients[1] + 
                        x1$coefficients[2] * 0 + # When categorical (si_manag) variable is 0 
                        x1$coefficients[3] * newdat.1$linear.st + 
                        x1$coefficients[4] * 0 + 
                        x1$coefficients[5] * newdat.1$slope.st +
                        x1$coefficients[6] * newdat.1$linear.st * 0 ) # When categorical (si_manag) variable is 0 
 
 pred2.1 <- inv.logit(x1$coefficients[1] + 
                        x1$coefficients[2] * 1 + # When categorical (si_manag) variable is 1 (manag)
                        x1$coefficients[3] * newdat.1$linear.st + 
                        x1$coefficients[4] * 0 + 
                        x1$coefficients[5] * newdat.1$slope.st +
                        x1$coefficients[6] * newdat.1$linear.st * 1 ) # When categorical (si_manag) variable is 1 (manag)
 
 # Unscale linear.st to plot it
 
 newdat.1$lin_unscaled <- newdat.1$linear.st*sd.lin + mean.lin
 
 plot(-15, ylim=c(0,1),xlim = c(0, max(newdat.1$lin_unscaled)), type="l", main = "", xlab = "Distance to human linear features (log)", ylab = "Probability of presence")
 
 points(datos_period$STATUS ~ datos_period$linear, col = "grey")
 points(pred1.1 ~ newdat.1$lin_unscaled, type = "p")
 points(pred2.1 ~ newdat.1$lin_unscaled, type = "p", col = "red")
 
}
 
 
 
  