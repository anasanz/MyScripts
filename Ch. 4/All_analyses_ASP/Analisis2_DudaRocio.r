
## -------------------------------------------------
##                    ANÁLISIS 1
## ------------------------------------------------- 

library(ResourceSelection)

## ---- Cargar datos ----

# Cargo los resultados de la rspf

setwd("D:/PhD/Fourth chapter/Results/RSF_Ana/Analisis 1/results_rspf")

p1 <- read.csv("Results_Pre_dm_random_loc_A1.csv", sep = ";")
p2 <- read.csv("Results_PreRep_dm_random_loc_A1.csv", sep = ";")
p3 <- read.csv("Results_Rep_dm_random_loc_A1.csv", sep = ";")


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

setwd("D:/PhD/Fourth chapter/Results/RSF_Ana/Analisis 1/data_matrix")
data <- read.csv("dm_random_loc_A1.csv")

d <- data[ ,c(10:21)] # Remove NA (solo 10 observationes para las que se ha extraido NA en carreteras no se por qué, no tiene importancia)
data <- data[complete.cases(d), ]

## ---- Análisis y predicted values ----

# PERIODO 1 (PRE)

  datos_period <- data[which(data$periodo == "Pre"), ]
  
  # En las RSPF escalé las variables justo antes de correrlo (tu lo hacias con la media y SD de todos los datos, y lo hago para los de cada periodo porque me parecia mejor para el modelo)
  
  mean.carr <- mean(datos_period$carreteras)
  sd.carr <- sd(datos_period$carreteras)
  datos_period$carreteras.st <- (datos_period$carreteras - mean.carr) / sd.carr
  
  mean.cam <- mean(datos_period$caminos)
  sd.cam <- sd(datos_period$caminos)
  datos_period$caminos.st <- (datos_period$caminos - mean.cam) / sd.cam
  
  mean.slop <- mean(datos_period$Slope)
  sd.slop <- sd(datos_period$Slope)
  datos_period$slope.st <- (datos_period$Slope - mean.slop) / sd.slop
  
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
  # Los coeficientes son IGUALES (y as? es como deb?a ser xD)
  
  # Ahora voy a hacer un glm normal (tendría que ser parecido)
  glm_p1 <- glm(STATUS ~ as.factor(barbecho) + as.factor(cereal) + as.factor(olivo) + 
                  as.factor(almendro) + as.factor(frutreg) + as.factor(herreg) + 
                  as.factor(forestal) + as.factor(vegnat) + caminos.st + slope.st + 
                  carreteras.st, data = datos_period_Pre, family = "binomial")
  
  glm_p1$coefficients # Cambia un poquillo, pero los valores son razonables
  
  # Ok, empecemos primero con glm, para probar
  
  # 2. Calcular predicted values
  
  # DISTANCIA A CARRETERAS. Aqu? propongo otra forma de generar la matriz de nuevos datos
  # utilizo la funci?n predict fijando el resto de variables (he elegido barbecho como
  # habitat utilizado).

  newdat.1 <- expand.grid(almendro = factor(0, levels = c(0,1)),
                          barbecho = factor(1, levels = c(0,1)),
                          cereal = factor(0, levels = c(0,1)),
                          frutreg = factor(0, levels = c(0,1)),
                          herreg = factor(0, levels = c(0,1)),
                          olivo = factor(0, levels = c(0,1)),
                          otherhersec = factor(0, levels = c(0,1)),
                          vegnat = factor(0, levels = c(0,1)),
                          forestal = factor(0, levels = c(0,1)),
                          carreteras.st = seq(min(datos_period_Pre$carreteras.st), max(datos_period_Pre$carreteras.st), length.out = 250),
                          slope.st = mean(datos_period_Pre$slope.st),
                          caminos.st = mean(datos_period_Pre$caminos.st))
  head(newdat.1)
  predichos <- predict(glm_p1, newdata = newdat.1, type = "response")
  plot(newdat.1$carreteras.st, predichos)
  # me parece que no se parece mucho a las gr?ficas que me mandas en el pdf, lo que pasa es
  # que no tengo muy claro si esas figuras salen de predecir en glm o para rsf.
  # si cambias el uso del suelo, la gr?fica tambi?n var?a ya que el modelo tiene en cuenta
  # el valor tanto del intercepto como del efecto de la variable categ?rica. Con barbecho
  # = 1 la probabilidad m?xima es muy alta pero con almendro, la probabilidad m?xima es 
  # muy baja (0,040).
  
  predichosrspf <- predict.rsf(RSF_p1, newdata = newdat.1, type = "response") 
  plot(newdat.1$carreteras.st, predichosrspf)
  # L?gicamente, la funci?n no es exactamente igual que en el glm pero s? salen valores
  # predichos altos con barbecho. Si utilizamos barbecho, de nuevo, cambian muchos los
  # valores predichos (m?s bajos que para el glm).
  
  # No podemos obviar el tama?o del efecto que tiene cada variable en el modelo. La
  # probabilidad de presencia aumenta much?simo cuando est? en barbecho, mientras que cae
  # mucho cuando est? en almendro y el efecto de las carreteras es menor que el de las
  # variables categ?ricas.
  
  
# CON SLOPE
  
  newdat.2 <- expand.grid(almendro = factor(0, levels = c(0,1)),
                          barbecho = factor(1, levels = c(0,1)),
                          cereal = factor(0, levels = c(0,1)),
                          frutreg = factor(0, levels = c(0,1)),
                          herreg = factor(0, levels = c(0,1)),
                          olivo = factor(0, levels = c(0,1)),
                          otherhersec = factor(0, levels = c(0,1)),
                          vegnat = factor(0, levels = c(0,1)),
                          forestal = factor(0, levels = c(0,1)),
                          slope.st = seq(min(datos_period_Pre$slope.st), max(datos_period_Pre$slope.st), length.out = 250),
                          carreteras.st = mean(datos_period_Pre$carreteras.st),
                          caminos.st = mean(datos_period_Pre$caminos.st))
  head(newdat.2)
  predichos2 <- predict(glm_p1, newdata = newdat.2, type = "response")
  plot(newdat.2$slope.st, predichos2)

  predichosrspf2 <- predict.rsf(RSF_p1, newdata = newdat.2, type = "response") 
  plot(newdat.2$slope.st, predichosrspf2)

