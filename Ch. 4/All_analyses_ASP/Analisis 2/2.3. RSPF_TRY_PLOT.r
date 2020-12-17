
## -------------------------------------------------
##            RSPF - Three periods
##  
## ------------------------------------------------- 

# St 1: used ~ no_manag + si_manag + linear.st + slope.st 
# St 1_2: used ~ no_manag + si_manag + caminos.st + slope.st 
# St 1_3: used ~ no_manag + si_manag + carreteras.st + slope.st 
# St 2: used ~ no_manag + si_manag*linear.st + slope.st

rm(list = ls())

library(ResourceSelection)
library(interactions)


temp <- setwd("D:/PhD/Fourth chapter/Results/RSF_Ana/Analisis 2/data_matrix")
temp <- list.files(pattern = "*.csv")
data <- lapply(temp, read.csv, sep = ",")

# Loop for different sets of random locations

#for(xxx in 1:length(temp)){
xxx = 1
datos <- data[[xxx]]
periods <- c("Pre", "PreRep", "Rep")

d <- datos[ ,c(10,18)] # Remove NA
datos <- datos[complete.cases(d), ] # No habia

# for (p in 1:length(periods)){

#PERIOD 1
p = 1
datos.rspf <- subset(datos, periodo %in% periods[p])

# Scale continuous variables here (once you have the subset with the data that you will introduce in the model)

mean.carr <- mean(datos.rspf$carreteras)
sd.carr <- sd(datos.rspf$carreteras)
datos.rspf$carreteras.st <- (datos.rspf$carreteras - mean.carr) / sd.carr

mean.cam <- mean(datos.rspf$caminos)
sd.cam <- sd(datos.rspf$caminos)
datos.rspf$caminos.st <- (datos.rspf$caminos - mean.cam) / sd.cam

mean.lin <- mean(datos.rspf$linear)
sd.lin <- sd(datos.rspf$linear)
datos.rspf$linear.st <- (datos.rspf$linear - mean.lin) / sd.lin

mean.slop <- mean(datos.rspf$Slope)
sd.slop <- sd(datos.rspf$Slope)
datos.rspf$slope.st <- (datos.rspf$Slope - mean.slop) / sd.slop


# Parameters

n.B <- 100 # Number of boostrapping 
n.var <- 5 # Number of explanatory variables
match.use.avai <- datos.rspf$ID_Year # Estructura para comparar los datos: Puntos de el mismo individuo y año (En un periodo)

# RSPF

#datos.rspf$no_manag <- as.factor(as.numeric(datos.rspf$no_manag))
datos.rspf$si_manag <- as.factor(as.numeric(datos.rspf$si_manag))

RSF_pre <- rspf(STATUS ~ no_manag + si_manag*linear.st + slope.st, data = datos.rspf, m = match.use.avai, B = n.B)

class(datos.rspf$si_manag)
levels(datos.rspf$si_manag)
interact_plot(RSF_pre, pred = linear.st, modx = si_manag, data = datos.rspf)


## ---- Periodo 2 ----

p = 2
datos.rspf <- subset(datos, periodo %in% periods[p])

# Scale continuous variables here (once you have the subset with the data that you will introduce in the model)

mean.carr <- mean(datos.rspf$carreteras)
sd.carr <- sd(datos.rspf$carreteras)
datos.rspf$carreteras.st <- (datos.rspf$carreteras - mean.carr) / sd.carr

mean.cam <- mean(datos.rspf$caminos)
sd.cam <- sd(datos.rspf$caminos)
datos.rspf$caminos.st <- (datos.rspf$caminos - mean.cam) / sd.cam

mean.lin <- mean(datos.rspf$linear)
sd.lin <- sd(datos.rspf$linear)
datos.rspf$linear.st <- (datos.rspf$linear - mean.lin) / sd.lin

mean.slop <- mean(datos.rspf$Slope)
sd.slop <- sd(datos.rspf$Slope)
datos.rspf$slope.st <- (datos.rspf$Slope - mean.slop) / sd.slop


# Parameters

n.B <- 100 # Number of boostrapping 
n.var <- 5 # Number of explanatory variables
match.use.avai <- datos.rspf$ID_Year # Estructura para comparar los datos: Puntos de el mismo individuo y año (En un periodo)

# RSPF

#datos.rspf$no_manag <- as.factor(as.numeric(datos.rspf$no_manag))
datos.rspf$si_manag <- as.factor(as.numeric(datos.rspf$si_manag))

RSF_prerep <- rspf(STATUS ~ no_manag + si_manag*linear.st + slope.st, data = datos.rspf, m = match.use.avai, B = n.B)

levels(datos.rspf$si_manag)
interact_plot(RSF_prerep, pred = linear.st, modx = si_manag, data = datos.rspf)


## ---- Periodo 3 ----

p = 3
datos.rspf <- subset(datos, periodo %in% periods[p])

# Scale continuous variables here (once you have the subset with the data that you will introduce in the model)

mean.carr <- mean(datos.rspf$carreteras)
sd.carr <- sd(datos.rspf$carreteras)
datos.rspf$carreteras.st <- (datos.rspf$carreteras - mean.carr) / sd.carr

mean.cam <- mean(datos.rspf$caminos)
sd.cam <- sd(datos.rspf$caminos)
datos.rspf$caminos.st <- (datos.rspf$caminos - mean.cam) / sd.cam

mean.lin <- mean(datos.rspf$linear)
sd.lin <- sd(datos.rspf$linear)
datos.rspf$linear.st <- (datos.rspf$linear - mean.lin) / sd.lin

mean.slop <- mean(datos.rspf$Slope)
sd.slop <- sd(datos.rspf$Slope)
datos.rspf$slope.st <- (datos.rspf$Slope - mean.slop) / sd.slop


# Parameters

n.B <- 100 # Number of boostrapping 
n.var <- 5 # Number of explanatory variables
match.use.avai <- datos.rspf$ID_Year # Estructura para comparar los datos: Puntos de el mismo individuo y año (En un periodo)

# RSPF

#datos.rspf$no_manag <- as.factor(as.numeric(datos.rspf$no_manag))
datos.rspf$si_manag <- as.factor(as.numeric(datos.rspf$si_manag))

RSF_rep <- rspf(STATUS ~ no_manag + si_manag*linear.st + slope.st, data = datos.rspf, m = match.use.avai, B = n.B)

levels(datos.rspf$si_manag)

setwd("D:/PhD/Fourth chapter/Results/RSF_Ana/Analisis 2")

pdf("St2.pdf", 7, 4)
par(mfrow=c(2,2))
interact_plot(RSF_pre, pred = linear.st, modx = si_manag, data = datos.rspf)
interact_plot(RSF_prerep, pred = linear.st, modx = si_manag, data = datos.rspf)
interact_plot(RSF_rep, pred = linear.st, modx = si_manag, data = datos.rspf)
dev.off()

plot(2)
