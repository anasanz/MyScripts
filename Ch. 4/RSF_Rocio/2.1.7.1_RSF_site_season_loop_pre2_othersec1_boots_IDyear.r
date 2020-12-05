#-----------------------------------------------------------------------------------------
#                     RESOURCE SELECTION PROBABILITY FUNCTIONS
#                         PERIODO PRE (HASTA 25 DE FEBRERO)
#                              ROCIO TARJUELO
#-----------------------------------------------------------------------------------------

# LIBRARIES-------------------------------------------------------------------------------
library(ResourceSelection)
#library(gstat)
#library(sp)

rm(list = ls())

# FUNCTIONS-------------------------------------------------------------------------------
# methods(summary)
# getAnywhere(summary.rsf)
# An R function using the one of "summary.rsf"

#-----------------------------------------------------------------------------------------
#         START ANALYSIS
#-----------------------------------------------------------------------------------------

# Load data-------------------------------------------------------------------------------

datos <- read.table("D:/Ana/Data/chapter4/2_matrix_RSPF.txt", header = T, dec = ",")

# ASP: Columna ID_year para comparar puntos de cada ID y año (si no, se comparan los usados
# de un individuo con todos los disponibles, ya que el código es el mismo aunque sean de dis-
# tintos años)
datos$ID_Year <- paste(datos$Logger_ID, datos$year, sep = "_") 

# Seleccionar s?lo los datos para el periodo pre
datos.rspf <- subset(datos, periodo%in%"Pre")

# Par?metros para la RSPF ----------------------------------------------------------------
# Loops para la RSPF
# Number of boostrapping = 100
n.B <- 100
# Number of loops to calculate average p-values and stardard errors = 50
n.loop <- 50
# Number of explanatory variables
n.var <- 11

# Analisis teniendo en cuenta los a?os y los individuos para la disponibilidad,
# donde los puntos aleatorios solo pueden caer en el del individuo MCP -------------------
n.year.id <- table(list(datos.rspf[datos.rspf$STATUS == 1, "year"], 
                        datos.rspf[datos.rspf$STATUS == 1, "Logger_ID"]))
n2017.id <- n.year.id[1, ]
n2018.id <- n.year.id[2, ]
n2019.id <- n.year.id[3, ]

n.id <- 1:length(levels(datos[datos.rspf$STATUS == 1, "Logger_ID"]))

unique(datos[datos.rspf$STATUS == 1, "Logger_ID"]) == colnames(n.year.id)  #No mismo orden

# Aqu? no hay que "desordenar" la matriz de puntos aleatorios, ya que est? construida 
# ordenada por a?o y logger_id para observaciones y en los aleatorios por a?o y puntos
# que caen en los MCPs de cada individuo. Es decir, es igual a la estructura indicada
# el siguiente objeto: match.use.avai

# Estructura para comparar observaciones y puntos aleatorios
match.use.avai <- rep(c(rep(colnames(n.year.id), as.numeric(n2017.id)), 
                        rep(colnames(n.year.id), as.numeric(n2018.id)),
                        rep(colnames(n.year.id), as.numeric(n2019.id))), 2)

which((datos.rspf$Logger_ID == match.use.avai) == F)  # Mismo orden

# Como ya sabemos que está en orden (comprobado con año y ID por separado vale
# uso directamente la columna ID_year)
match.use.avai <- datos.rspf$ID_Year


RSF.pval <- matrix(NA, nrow = n.var + 1, ncol = n.loop)
# N_var+1, the number of explanatory variables plus the intercept
RSF.SE <- matrix(NA, nrow = n.var + 1, ncol = n.loop)
RSF.Est <- matrix(NA, nrow = n.var + 1, ncol = n.loop)
AIC <- numeric()
for (i in 1:n.loop){
  RSF <- rspf(STATUS ~ as.factor(barbecho) + as.factor(cereal) + as.factor(olivo) + 
                as.factor(almendro) + as.factor(frutreg) + as.factor(herreg) + 
                as.factor(forestal) + as.factor(vegnat) + caminos.st + slope.st + 
                carreteras.st, data = datos.rspf, m = match.use.avai, B = n.B)
  summ.RSF <- summary(RSF)
  RSF.pval[, i] <- summ.RSF$coefficients[, 4]
  RSF.SE[,i] <- summ.RSF$coefficients[, 2]
  RSF.Est[,i] <- summ.RSF$coefficients[, 1]
  AIC[i] <- AIC(RSF)
  print(i)
}

RSF.pval.mean <- round(apply(RSF.pval, 1, mean), 4)
names(RSF.pval.mean) <- rownames(summ.RSF$coefficients)

RSF.SE.mean <- round(apply(RSF.SE, 1, mean), 3)
names(RSF.SE.mean) <- rownames(summ.RSF$coefficients)

RSF.Est.mean <- round(apply(RSF.Est, 1, mean), 3)
names(RSF.Est.mean) <- rownames(summ.RSF$coefficients)
round(RSF.Est[, 1], 3)

AIC.mean <- mean(AIC)

results <- cbind(RSF.Est.mean, RSF.SE.mean, RSF.pval.mean, AIC.mean)



setwd("D:/Ana/Results/chapter4")
write.csv(results, file = "RSF.results.pre.othersec.IDyear.csv")


