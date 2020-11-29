#-----------------------------------------------------------------------------------------
#                     RESOURCE SELECTION PROBABILITY FUNCTIONS
#                         PERIODO PRE (HASTA 25 DE FEBRERO)
#                              ROCIO TARJUELO
#-----------------------------------------------------------------------------------------

# LIBRARIES-------------------------------------------------------------------------------
library(ResourceSelection)
#library(gstat)
#library(sp)

# FUNCTIONS-------------------------------------------------------------------------------
# methods(summary)
# getAnywhere(summary.rsf)
# An R function using the one of "summary.rsf"

#-----------------------------------------------------------------------------------------
#         START ANALYSIS
#-----------------------------------------------------------------------------------------

# Load data-------------------------------------------------------------------------------

datos <- read.table("data/2_matrix_RSPF.txt", header = T, dec = ",")

# Seleccionar sólo los datos para el periodo pre
datos.use.rspf <- subset(datos, periodo%in%"Pre" & STATUS == 1)
datos.avai.rspf <- subset(datos, periodo%in%"Pre" & STATUS == 0)

# Parámetros para la RSPF ----------------------------------------------------------------
# Loops para la RSPF
# Number of boostrapping = 100
n.B <- 100
# Number of loops to calculate average p-values and stardard errors = 50
n.loop <- 50
# Number of explanatory variables
n.var <- 11


# Tengo que "desordenar" la matriz de puntos aleatorios, ya que está construida ordenada 
# por año y logger_id. Esto no es relevante en el primer análisis, donde sólo se tiene en
# cuenta el año (ya que coge todos los puntos aleatorios para el bootstrapping), pero
# sí para el segundo análisis, donde se tiene en cuenta el ID sólo de las observaciones,
# y no de en qué MCP caen los aleatorios.
datos.avai.rspf.ord <- datos.avai.rspf[order(datos.avai.rspf$year,
                                             datos.avai.rspf$Field1), ]
# Unir matrices
datos.tot <- rbind.data.frame(datos.use.rspf, datos.avai.rspf.ord)

# Analisis solo teniendo en cuenta los años para la disponibilidad -----------------------
n.year <- table(datos.use.rspf$year)
n2017 <- n.year[1]
n2018 <- n.year[2]
n2019 <- n.year[3]

# Estructura para comparar observaciones y puntos aleatorios
match.use.avai1 <- rep(c(rep(2017, n2017), rep(2018, n2018), rep(2019, n2019)), 2)


RSF1.pval <- matrix(NA, nrow = n.var + 1, ncol = n.loop)
# N_var+1, the number of explanatory variables plus the intercept
RSF1.SE <- matrix(NA, nrow = n.var + 1, ncol = n.loop)
RSF1.Est <- matrix(NA, nrow = n.var + 1, ncol = n.loop)
AIC <- numeric()
for (i in 1:n.loop){
  RSF <- rspf(STATUS ~ as.factor(barbecho) + as.factor(cereal) + as.factor(olivouse) + 
                as.factor(almendro) + as.factor(frutreg) + as.factor(herreguse) + 
                as.factor(vegnat) + as.factor(otherhersec) + caminos.st + slope.st + 
                carreteras.st, data = datos.tot, m = match.use.avai1)
  summ.RSF <- summary(RSF)
  RSF1.pval[, i] <- summ.RSF$coefficients[, 4]
  RSF1.SE[,i] <- summ.RSF$coefficients[, 2]
  RSF1.Est[,i] <- summ.RSF$coefficients[, 1]
  AIC[i] <- AIC(RSF)
  print(i)
}

RSF1.pval.mean <- round(apply(RSF1.pval, 1, mean), 4)
names(RSF1.pval.mean) <- names(RSF$coefficients)

RSF1.SE.mean <- round(apply(RSF1.SE, 1, mean), 3)
names(RSF1.SE.mean) <- names(RSF$coefficients)

round(RSF1.Est[, 1], 3)

AIC

# Analisis solo teniendo en cuenta los años y los individuos para la disponibilidad,
# aunque los puntos aleatorios pueden caer en cualquier parte del MCP --------------------
n.year.id <- table(list(datos.use.rspf$year, datos.use.rspf$Logger_ID))
n2017.id <- n.year.id[1,]
n2018.id <- n.year.id[2,]
n2019.id <- n.year.id[3,]

n.id <- 1:length(levels(datos.use.rspf$Logger_ID))

levels(datos.use.rspf$Logger_ID) == colnames(n.year.id)  # Mismo orden

# Estructura para comparar observaciones y puntos aleatorios
match.use.avai2 <- rep(c(rep(n.id, as.numeric(n2017.id)), rep(n.id, as.numeric(n2018.id)),
                        rep(n.id, as.numeric(n2019.id))), 2)

RSF2.pval <- matrix(NA, nrow = n.var + 1, ncol = n.loop)
# N_var+1, the number of explanatory variables plus the intercept
RSF2.SE <- matrix(NA, nrow = n.var + 1, ncol = n.loop)
RSF2.Est <- matrix(NA, nrow = n.var + 1, ncol = n.loop)
AIC2 <- numeric()
for (i in 1:n.loop){
  RSF <- rspf(STATUS ~ as.factor(barbecho) + as.factor(cereal) + as.factor(olivouse) + 
                as.factor(almendro) + as.factor(frutreg) + as.factor(herreguse) + 
                as.factor(vegnat) + as.factor(otherhersec) + caminos.st + slope.st + 
                carreteras.st, data = datos.tot, m = match.use.avai2)
  summ.RSF <- summary(RSF)
  RSF2.pval[, i] <- summ.RSF$coefficients[, 4]
  RSF2.SE[,i] <- summ.RSF$coefficients[, 2]
  RSF2.Est[,i] <- summ.RSF$coefficients[, 1]
  AIC2[i] <- AIC(RSF)
  print(i)
}

RSF2.pval.mean <- round(apply(RSF2.pval, 1, mean), 4)
names(RSF2.pval.mean) <- rownames(summ.RSF$coefficients)

RSF2.SE.mean <- round(apply(RSF2.SE, 1, mean), 3)
names(RSF2.SE.mean) <- rownames(summ.RSF$coefficients)

round(RSF2.Est[, 1], 3)

AIC2



##########################################################################################
# Evaluación del modelo
# Hosmer-Lemeshow test para bondad del ajuste del modelo. Valores bajos de p indican que
# el modelo es malo.
mod1.x <- datos.pre$STATUS
mod1.y <- fitted(RSF)
hoslem.test(mod1.x, mod1.y)
mep(RSF)


# Calculo manual de residuos. R = observed- predicted value
#res <- mod1.x - mod1.y
# Solo deja calcular los residuos cuando se utiliza la disponibilidad global

res.var <- cbind.data.frame(res = as.numeric(residuals(RSF)), 
                        x = datos.pre$x, 
                        y = datos.pre$y)
coordinates(res.var) = ~ x + y
v <- variogram(res~1, res.var)
fit.variogram(v, vgm(c("Exp", "Mat", "Sph")))
plot(v)

# Calculo manual de residuos. R = observed- predicted value
library(sp)
data(meuse)
coordinates(meuse) = ~x+y
head(meuse)
plot(variogram(log(zinc)~1, meuse))

##########################################################################################
