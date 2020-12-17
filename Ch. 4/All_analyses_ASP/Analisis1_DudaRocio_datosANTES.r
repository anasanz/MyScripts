## -------------------------------------------------
##                    ANÁLISIS 1
## ------------------------------------------------- 

library(ResourceSelection)

## ---- Cargar datos ----

# Cargo los resultados de la rspf

setwd("D:/PhD/Fourth chapter/Results/RSF_habitatsel3/othersec_bootstrap_IDyear")

p1 <- read.csv("RSF.results.pre.othersec.IDyear.csv", sep = ",")
p2 <- read.csv("RSF.results.prerep.othersec.IDyear.csv", sep = ",")
p3 <- read.csv("RSF.results.rep.othersec.IDyear.csv", sep = ",")


p1 <- p1[,-5]
p1$period <- "p1"
p1 <- p1[nrow(p1):1, ]
p1[,1] <- c("DistAsp", "Slope", "DistGrav", "NatVeg", "Forest", "Herb.irri", "Fruit.irri", "Almond", "Olive", "Cereal", "Fallow", "Intercept")

p2 <- p2[,-5]
p2$period <- "p2"
p2 <- p2[nrow(p2):1, ]
p2[,1] <- c("DistAsp", "Slope", "DistGrav", "NatVeg", "Forest", "Herb.irri", "Fruit.irri", "Almond", "Olive", "Cereal", "Fallow", "Intercept")

p3 <- p3[,-5]
p3$period <- "p3"
p3 <- p3[nrow(p3):1, ]
p3[,1] <- c("DistAsp", "Slope", "DistGrav", "NatVeg", "Forest", "Herb.irri", "Fruit.irri", "Almond", "Olive", "Cereal", "Fallow", "Intercept")


allperiods <- rbind(p1,p2,p3)
colnames(allperiods) <- c("X", "Estimate", "SE", "p", "period")
p <- c("p1","p2","p3")

# Cargo matriz de datos 

data <- read.table("D:/PhD/Fourth chapter/Data/2_matrix_RSPF.txt", header = T, dec = ",")
data$ID_Year <- paste(data$Logger_ID, data$year, sep = "_") 

d <- data[ ,c(9:24)] # Mo habia NA
data <- data[complete.cases(d), ]

## ---- Análisis y predicted values ----

# PERIODO 1 (PRE)

datos_period <- data[which(data$periodo == "Pre"), ]

# Aquí ya están las variables escaladas

datos_period_Pre <- datos_period

# 1. Recopilar resultados de las rspf (de la "buena", de una corrida una vez, de un glm normal)

p <- allperiods[which(allperiods$period == "p1" ), ] # Coeficientes "buenos" bootstraping, loops ...

# Voy a correr otra vez la rspf (solo 1 vez con bootstraping, deberia de dar coeficientes muy similares a cuando lo corrimos muchas veces): SOLO POR COMPROBAR

n.B <- 100 
match.use.avai <- datos_period_Pre$ID_Year 

RSF_p1 <- rspf(STATUS ~ as.factor(barbecho) + as.factor(cereal) + as.factor(olivo) + 
                 as.factor(almendro) + as.factor(frutreg) + as.factor(herreg) + 
                 as.factor(forestal) + as.factor(vegnat) + caminos.st + slope.st + 
                 carreteras.st, data = datos_period_Pre, m = match.use.avai, B = n.B)

RSF_p1$coefficients # De la 1 rspf
p  # De 100 rspf 
# Los coeficientes son muy parecidos (logicamente, solo estoy descartando errores...)

# Ahora voy a hacer un glm normal (tendría que ser parecido)
glm_p1 <- glm(STATUS ~ as.factor(barbecho) + as.factor(cereal) + as.factor(olivo) + 
                as.factor(almendro) + as.factor(frutreg) + as.factor(herreg) + 
                as.factor(forestal) + as.factor(vegnat) + caminos.st + slope.st + 
                carreteras.st, data = datos_period_Pre, family = "binomial")

glm_p1$coefficients # Cambia un poquillo, pero los valores son razonables

# 2. Calcular predicted values

# DISTANCIA A CARRETERAS

newdata_asp <- as.data.frame(lapply(lapply(datos_period_Pre[, c(22,24)], mean), rep, 250)) # Mean of caminos y slope
newdata_asp$asp_new <- seq(min(datos_period_Pre$carreteras.st), max(datos_period_Pre$carreteras.st), length.out = 250) # Calores crecientes de carreteras

inv.logit <- function(inValues) { # To backtransform (from linear response to probabilities from 0-1)
  1.0 / (1.0 + exp(-inValues))}

# Predicted values de la rspf original ("buena")

pred1_p1 <- inv.logit(p[p$X == 'Intercept',2] + 
                        p[p$X == 'Fallow',2] * 0 + 
                        p[p$X == 'Cereal',2] * 0 + 
                        p[p$X == 'Olive',2] * 0 + 
                        p[p$X == 'Almond',2] * 0 + 
                        p[p$X == 'Fruit.irri',2] * 0 + 
                        p[p$X == 'Herb.irri',2] * 0 + 
                        p[p$X == 'Forest',2] * 0 + 
                        p[p$X == 'NatVeg',2] * 0 + 
                        p[p$X == 'DistGrav',2] * newdata_asp$caminos.st + 
                        p[p$X == 'Slope',2] * newdata_asp$slope.st + 
                        p[p$X == 'DistAsp',2] * newdata_asp$asp_new)

pred1_p1 # Super bajos!! Asumo que los resultados de RSF_p1 serán igual

# Predicted values del glm

pred2_p1 <- inv.logit(glm_p1$coefficients[1] + 
                        glm_p1$coefficients[2] * 0 + 
                        glm_p1$coefficients[3] * 0 + 
                        glm_p1$coefficients[4] * 0 + 
                        glm_p1$coefficients[5] * 0 + 
                        glm_p1$coefficients[6] * 0 + 
                        glm_p1$coefficients[7] * 0 + 
                        glm_p1$coefficients[8] * 0 + 
                        glm_p1$coefficients[9] * 0 + 
                        glm_p1$coefficients[10] * newdata_asp$caminos.st + 
                        glm_p1$coefficients[11] * newdata_asp$slope.st + 
                        glm_p1$coefficients[12] * newdata_asp$asp_new)

pred2_p1 # Un poco mas altos

#Vamos a plotear el tema...

plot(-15, ylim=c(0,1),xlim = c(min(newdata_asp$asp_new), max(newdata_asp$asp_new)), main = "p1", xlab = "Distance to asphalted roads (log)", ylab = "Probability of presence")

points(datos_period_Pre$STATUS ~ datos_period_Pre$carreteras.st, col = "grey") # Raw data
table(datos_period_Pre$STATUS) # Está bastante equilibrado

points(pred1_p1 ~ newdata_asp$asp_new, type = "l", col = "red") # Pero mira que predicción más baja!! 
points(pred2_p1 ~ newdata_asp$asp_new, type = "l", col = "blue") # Esta también es baja, pero no tanto...el coeficiente es un poco raro para este periodo, mira el periodo 2 

# He intentado predecir los valores manualmente porque con las rspf no tenemos el modelo, si no los coeficientes directamente
# Voy a intentarlo con la funcion predict() a ver si lo estoy haciendo bien (con distancia a carreteras en periodos 1 y 2 tb)


pred_glm_p1 <- predict(glm_p1, type = "response")
pred_rsf_p1 <- predict(RSF_p1, type = "response")  # No se por qué da más valores que datos hay en el modelo
pred_rsf_p1 <- predict.rsf(RSF_p1, type = "response", part = "all") # En esta funcion de internet funciona si pones part = "all"

plot(-15, ylim=c(0,1),xlim = c(min(datos_period_Pre$carreteras.st), max(datos_period_Pre$carreteras.st)), main = "p1", xlab = "Distance to asphalted roads (log)", ylab = "Probability of presence")
points(datos_period_Pre$STATUS ~ datos_period_Pre$carreteras.st, col = "grey") # Raw data

points(pred_glm_p1 ~ datos_period_Pre$carreteras.st, type = "p", col = "blue")
points(pred_rsf_p1 ~ datos_period_Pre$carreteras.st, type = "p", col = "red") # Aqui la rspf los predice más bajos, voy a hacerlo con una nueva data frame como antes

# Predict from predict() function y nueva data frame
colnames(newdata_asp)[3] <- "carreteras.st"
newdata_asp$barbecho <- factor(0, levels = c(0,1)) # Pongo lo de factor porque si no la funcion predict de la rspf se queja
newdata_asp$cereal <- factor(0, levels = c(0,1))
newdata_asp$olivo <- factor(0, levels = c(0,1))
newdata_asp$almendro <- factor(0, levels = c(0,1))
newdata_asp$frutreg <- factor(0, levels = c(0,1))
newdata_asp$herreg <- factor(0, levels = c(0,1))
newdata_asp$forestal <- factor(0, levels = c(0,1))
newdata_asp$vegnat <- factor(0, levels = c(0,1))


pred_glm_p1_newdata <- predict(glm_p1, newdata = newdata_asp, type = "response")
pred_rsf_p1_newdata <- predict.rsf(RSF_p1, newdata = newdata_asp, type = "response", part = "all")

plot(-15, ylim=c(0,1),xlim = c(min(datos_period_Pre$carreteras.st), max(datos_period_Pre$carreteras.st)), main = "p1", xlab = "Distance to asphalted roads (log)", ylab = "Probability of presence")
points(datos_period_Pre$STATUS ~ datos_period_Pre$carreteras.st, col = "grey") # Raw data

points(pred_glm_p1_newdata ~ newdata_asp$carreteras.st, type = "l", col = "blue")
points(pred_rsf_p1_newdata ~ newdata_asp$carreteras.st, type = "l", col = "red") # El resultado es igual que prediciendo manualmente, la rspf predice valores muy bajos

##########################################################################################################################

# PERIODO 2 (PREREP)

datos_period <- data[which(data$periodo == "PreRep"), ]

# Aquí ya están las variables escaladas

datos_period_PreRep <- datos_period

# 1. Recopilar resultados de las rspf (de la "buena", de una corrida una vez, de un glm normal)

p <- allperiods[which(allperiods$period == "p2" ), ] # Coeficientes "buenos" bootstraping, loops ...

# Voy a correr otra vez la rspf (solo 1 vez con bootstraping, deberia de dar coeficientes muy similares a cuando lo corrimos muchas veces): SOLO POR COMPROBAR

n.B <- 100 
match.use.avai <- datos_period_PreRep$ID_Year 

RSF_p2 <- rspf(STATUS ~ as.factor(barbecho) + as.factor(cereal) + as.factor(olivo) + 
                 as.factor(almendro) + as.factor(frutreg) + as.factor(herreg) + 
                 as.factor(forestal) + as.factor(vegnat) + caminos.st + slope.st + 
                 carreteras.st, data = datos_period_PreRep, m = match.use.avai, B = n.B)

RSF_p2$coefficients # De la 1 rspf
p  # De 100 rspf 
# Los coeficientes son muy parecidos (logicamente, solo estoy descartando errores...)

# Ahora voy a hacer un glm normal (tendría que ser parecido)
glm_p2 <- glm(STATUS ~ as.factor(barbecho) + as.factor(cereal) + as.factor(olivo) + 
                as.factor(almendro) + as.factor(frutreg) + as.factor(herreg) + 
                as.factor(forestal) + as.factor(vegnat) + caminos.st + slope.st + 
                carreteras.st, data = datos_period_PreRep, family = "binomial")

glm_p2$coefficients #Aqui si que cambia bastante

# 2. Calcular predicted values

# DISTANCIA A CARRETERAS

newdata_asp <- as.data.frame(lapply(lapply(datos_period_PreRep[, c(22,24)], mean), rep, 250)) # Mean of caminos y slope
newdata_asp$asp_new <- seq(min(datos_period_PreRep$carreteras.st), max(datos_period_PreRep$carreteras.st), length.out = 250) # Calores crecientes de carreteras

inv.logit <- function(inValues) { # To backtransform (from linear response to probabilities from 0-1)
  1.0 / (1.0 + exp(-inValues))}

# Predicted values de la rspf original ("buena")

pred1_p2 <- inv.logit(p[p$X == 'Intercept',2] + 
                        p[p$X == 'Fallow',2] * 0 + 
                        p[p$X == 'Cereal',2] * 0 + 
                        p[p$X == 'Olive',2] * 0 + 
                        p[p$X == 'Almond',2] * 0 + 
                        p[p$X == 'Fruit.irri',2] * 0 + 
                        p[p$X == 'Herb.irri',2] * 0 + 
                        p[p$X == 'Forest',2] * 0 + 
                        p[p$X == 'NatVeg',2] * 0 + 
                        p[p$X == 'DistGrav',2] * newdata_asp$caminos.st + 
                        p[p$X == 'Slope',2] * newdata_asp$slope.st + 
                        p[p$X == 'DistAsp',2] * newdata_asp$asp_new)

pred1_p2 # Super bajos!! Asumo que los resultados de RSF_p1 serán igual

# Predicted values del glm

pred2_p2 <- inv.logit(glm_p2$coefficients[1] + 
                        glm_p2$coefficients[2] * 0 + 
                        glm_p2$coefficients[3] * 0 + 
                        glm_p2$coefficients[4] * 0 + 
                        glm_p2$coefficients[5] * 0 + 
                        glm_p2$coefficients[6] * 0 + 
                        glm_p2$coefficients[7] * 0 + 
                        glm_p2$coefficients[8] * 0 + 
                        glm_p2$coefficients[9] * 0 + 
                        glm_p2$coefficients[10] * newdata_asp$caminos.st + 
                        glm_p2$coefficients[11] * newdata_asp$slope.st + 
                        glm_p2$coefficients[12] * newdata_asp$asp_new)

pred2_p1 # Estos son bastante más altos

#Vamos a plotear el tema...

plot(-15, ylim=c(0,1),xlim = c(min(newdata_asp$asp_new), max(newdata_asp$asp_new)), main = "p2", xlab = "Distance to asphalted roads (log)", ylab = "Probability of presence")

points(datos_period_PreRep$STATUS ~ datos_period_PreRep$carreteras.st) # Raw data
table(datos_period_PreRep$STATUS) # Está bastante equilibrado

points(pred1_p2 ~ newdata_asp$asp_new, type = "l", col = "red") # Pero mira que predicción más baja!! 
points(pred2_p2 ~ newdata_asp$asp_new, type = "l", col = "blue") # Ésta es bastante más alta, 

# Lo que las diferencia es sobre todo el intercepto...no entiendo nada xD
RSF_p2$coefficients
glm_p2$coefficients

# He intentado predecir los valores manualmente porque con las rspf no tenemos el modelo, si no los coeficientes directamente
# Voy a intentarlo con la funcion predict() a ver si lo estoy haciendo bien (con distancia a carreteras en periodo 2 tb)

pred_glm_p2 <- predict(glm_p2, type = "response")
pred_rsf_p2 <- predict(RSF_p2, type = "response")  # No se por qué da más valores que datos hay en el modelo
pred_rsf_p2 <- predict.rsf(RSF_p2, type = "response", part = "all") # En esta funcion de internet funciona si pones part = "all"

plot(-15, ylim=c(0,1),xlim = c(min(datos_period_PreRep$carreteras.st), max(datos_period_PreRep$carreteras.st)), main = "p1", xlab = "Distance to asphalted roads (log)", ylab = "Probability of presence")
points(datos_period_PreRep$STATUS ~ datos_period_PreRep$carreteras.st, col = "grey") # Raw data

points(pred_glm_p2 ~ datos_period_PreRep$carreteras.st, type = "p", col = "blue")
points(pred_rsf_p2 ~ datos_period_PreRep$carreteras.st, type = "p", col = "red") # Aqui la rspf los predice más bajos, voy a hacerlo con una nueva data frame como antes

# Predict from predict() function y nueva data frame
colnames(newdata_asp)[3] <- "carreteras.st"
newdata_asp$barbecho <- factor(0, levels = c(0,1)) # Pongo lo de factor porque si no la funcion predict de la rspf se queja
newdata_asp$cereal <- factor(0, levels = c(0,1))
newdata_asp$olivo <- factor(0, levels = c(0,1))
newdata_asp$almendro <- factor(0, levels = c(0,1))
newdata_asp$frutreg <- factor(0, levels = c(0,1))
newdata_asp$herreg <- factor(0, levels = c(0,1))
newdata_asp$forestal <- factor(0, levels = c(0,1))
newdata_asp$vegnat <- factor(0, levels = c(0,1))


pred_glm_p2_newdata <- predict(glm_p2, newdata = newdata_asp, type = "response")
pred_rsf_p2_newdata <- predict.rsf(RSF_p2, newdata = newdata_asp, type = "response", part = "all")

plot(-15, ylim=c(0,1),xlim = c(min(datos_period_PreRep$carreteras.st), max(datos_period_PreRep$carreteras.st)), main = "p2", xlab = "Distance to asphalted roads (log)", ylab = "Probability of presence")
points(datos_period_PreRep$STATUS ~ datos_period_PreRep$carreteras.st, col = "grey") # Raw data

points(pred_glm_p2_newdata ~ newdata_asp$carreteras.st, type = "l", col = "blue")
points(pred_rsf_p2_newdata ~ newdata_asp$carreteras.st, type = "l", col = "red") # El resultado es igual que prediciendo manualmente, la rspf predice valores muy bajos

