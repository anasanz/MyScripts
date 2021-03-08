## -------------------------------------------------
##           Plot interaction ANALISIS 2
## ------------------------------------------------- 

library(ResourceSelection)

## -------------------------------------------------
##            Interaction Manag*Lin
## ------------------------------------------------- 


## ---- Generate predicted values for continuous variables ----

setwd("D:/PhD/Fourth chapter/Results/RSF_Ana/Analisis 2/data_matrix")
datos <- read.csv("dm_random_loc_A1_ANALISIS2.csv")

d <- datos[ ,c(10:18)] # Remove NA
datos <- datos[complete.cases(d), ]

# Inverse of logit to back-transform 
#inv.logit <- function(inValues) {
#  1.0 / (1.0 + exp(-inValues))}


#p_data <- c("Pre", "PreRep", "Rep")
#p_coef <- c("p1", "p2", "p3")
#titles <- c("Pre-Bottleneck", "Bottleneck", "Post-Bottleneck")


## -------------------------------------------------
##           Distance to linear features
## ------------------------------------------------- 

datos_period <- datos[which(datos$periodo == "Pre"), ]

# En las RSPF escalé las variables justo antes de correrlo (tu lo hacias con la media y SD de todos los datos, y lo hago para los de cada periodo porque me parecia mejor para el modelo)

mean.lin <- mean(datos_period$linear)
sd.lin <- sd(datos_period$linear)
datos_period$linear.st <- (datos_period$linear - mean.lin) / sd.lin

datos_period_Pre <- datos_period
habitats <- apply(datos_period_Pre[,c(10, 16, 17)], 1, sum)
which(habitats!=1)
habitats[1689]
datos_period_Pre[which(habitats!=1),]

datos_period_Pre <- datos_period_Pre[which(habitats==1),]

n.B <- 100 
match.use.avai <- datos_period_Pre$ID_Year 

RSF.linear.p1 <- rspf(STATUS ~ as.factor(si_manag) + as.factor(no_manag) + linear +
                 linear*as.factor(no_manag) + linear*as.factor(si_manag) ,
               data = datos_period_Pre, m = match.use.avai, B = n.B, link = "logit")

glm.linear.p1 <- glm(STATUS ~ as.factor(si_manag) + as.factor(no_manag) + linear +
                 linear*as.factor(no_manag) + linear*as.factor(si_manag) ,
               data = datos_period_Pre, family = binomial(link = "logit"))

summary(RSF.linear.p1)
summary(glm.linear.p1)


RSF.slope.p1 <- rspf(STATUS ~ as.factor(si_manag) + as.factor(no_manag) + Slope +
                        Slope*as.factor(no_manag) + Slope*as.factor(si_manag) ,
                      data = datos_period_Pre, m = match.use.avai, B = n.B, link = "logit")

glm.slope.p1 <- glm(STATUS ~ as.factor(si_manag) + as.factor(no_manag) + Slope +
                       Slope*as.factor(no_manag) + Slope*as.factor(si_manag) ,
                     data = datos_period_Pre, family = binomial(link = "logit"))

summary(RSF.slope.p1)
summary(glm.slope.p1)


newdat.linear.1 <- expand.grid(no_manag = factor(1, levels = c(0,1)),
                        si_manag = factor(0, levels = c(0,1)),
                        linear = seq(min(datos_period_Pre$linear), 
                                        max(datos_period_Pre$linear), length.out = 250))
newdat.slope.1 <- expand.grid(no_manag = factor(1, levels = c(0,1)),
                               si_manag = factor(0, levels = c(0,1)),
                               Slope = seq(min(datos_period_Pre$Slope), 
                                               max(datos_period_Pre$Slope), length.out = 250))

pred.rspf.linear.1 <- predict.rsf(RSF.linear.p1, newdata = newdat.linear.1, type = "response")
pred.glm.linear.1 <- predict(glm.linear.p1, newdata = newdat.linear.1, type = "response")
pred.rspf.slope.1 <- predict.rsf(RSF.slope.p1, newdata = newdat.slope.1, type = "response")
pred.glm.slope.1 <- predict(glm.slope.p1, newdata = newdat.slope.1, type = "response")
par(mfrow=c(2,2))
plot(newdat.linear.1$linear, pred.rspf.linear.1)
plot(newdat.linear.1$linear, pred.glm.linear.1)
plot(newdat.slope.1$Slope, pred.rspf.slope.1)
plot(newdat.slope.1$Slope, pred.glm.slope.1)


# GLM solo var categ?ricas
glm.2 <- glm(STATUS ~ as.factor(si_manag) + as.factor(no_manag),
                    data = datos_period_Pre, family = binomial(link = "logit"))
rspf2 <- rspf(STATUS ~ as.factor(si_manag) + as.factor(no_manag),
             data = datos_period_Pre, m = 0, link = "logit") ## No se puede
rsf2 <- rsf(STATUS ~ as.factor(si_manag) + as.factor(no_manag),
              data = datos_period_Pre, m = 0) ## S? se puede
summary(rsf2)  
summary(glm.2) ## Mismo summary entre rsf y glm

### DE MOMENTO, HASTA AQU?, ANA. EL RESTO SON LINEAS PARA CONTINUAR UNA VEZ ME DES EL OK




##########################################################################################
##########################################################################################
newdat.2 <- expand.grid(no_manag = factor(0, levels = c(0,1)),
                        si_manag = factor(1, levels = c(0,1)),
                        linear.st = seq(min(datos_period_Pre$linear.st), 
                                        max(datos_period_Pre$linear.st), length.out = 250))

predichos2 <- predict.rsf(RSF_p1, newdata = newdat.2, type = "response")
predichos2.rspf <- predict.rsf(RSF_p1, newdata = newdat.2, type = "response")
predichos2.glm <- predict(glm_p1, newdata = newdat.2, type = "response")
plot(newdat.2$linear.st, predichos2.rspf)
plot(newdat.2$linear.st, predichos2.glm)



# Periodo 2
datos_period2 <- datos[which(datos$periodo == "PreRep"), ]

# En las RSPF escalé las variables justo antes de correrlo (tu lo hacias con la media y SD de todos los datos, y lo hago para los de cada periodo porque me parecia mejor para el modelo)

mean.lin2 <- mean(datos_period2$linear)
sd.lin2 <- sd(datos_period2$linear)
datos_period2$linear.st <- (datos_period2$linear - mean.lin2) / sd.lin2

datos_period_PreRep <- datos_period2
habitats2 <- apply(datos_period_PreRep[,c(10, 16, 17)], 1, sum)
which(habitats2!=1)
datos_period_PreRep[which(habitats2!=1),]

datos_period_PreRep2 <- datos_period_PreRep[which(habitats==1),]

match.use.avai2 <- datos_period_PreRep2$ID_Year 

RSF_p2 <- rspf(STATUS ~ as.factor(si_manag) + as.factor(no_manag) + Slope +
                 Slope*as.factor(no_manag) + Slope*as.factor(si_manag) ,
               data = datos_period_PreRep2, m = match.use.avai2, B = n.B, link = "logit")

glm_p2 <- glm(STATUS ~ as.factor(si_manag) + as.factor(no_manag) + linear.st +
                linear.st*as.factor(no_manag) + linear.st*as.factor(si_manag) ,
              data = datos_period_PreRep2, family = binomial(link = "logit"))

summary(RSF_p2)
summary(glm_p2)

summary(datos_period_PreRep2[datos_period_PreRep2$STATUS==1, "linear.st"])
summary(datos_period_PreRep2[datos_period_PreRep2$STATUS==0, "linear.st"])
boxplot(datos_period_PreRep2$linear.st~datos_period_PreRep2$STATUS)

summary(datos_period_PreRep2[datos_period_PreRep2$STATUS==1, "Slope"])
summary(datos_period_PreRep2[datos_period_PreRep2$STATUS==0, "Slope"])
boxplot(datos_period_PreRep2$Slope~datos_period_PreRep2$STATUS)

newdat.1.2 <- expand.grid(no_manag = factor(1, levels = c(0,1)),
                        si_manag = factor(0, levels = c(0,1)),
                        Slope = seq(min(datos_period_PreRep2$Slope), 
                                        max(datos_period_PreRep2$Slope), length.out = 250))

predichos2.1.rspf <- predict.rsf(RSF_p2, newdata = newdat.1.2, type = "response")
predichos2.1.glm <- predict(glm_p2, newdata = newdat.1.2, type = "response")
plot(newdat.1.2$Slope, predichos2.1.rspf)
plot(newdat.1.2$linear.st, predichos2.1.glm)


######################################################################################

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
 
 
 
  