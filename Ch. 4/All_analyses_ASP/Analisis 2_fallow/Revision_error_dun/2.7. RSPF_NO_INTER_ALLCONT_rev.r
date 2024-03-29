## -------------------------------------------------
##            RSPF - Three periods
##   4 structures to compare with AIC for each period 
## ------------------------------------------------- 


rm(list = ls())
library(ResourceSelection)

## ---- used ~ no_manag + si_manag + slope ----

setwd("D:/PhD/Fourth chapter/Results/RSF_Ana/Analisis 2/data_matrix/Revision")
datos <- read.csv("dm_random_loc_A1.csv", sep = ",")

periods <- c("Pre", "PreRep", "Rep")

d <- datos[ ,c(10:18)] # Remove NA
datos <- datos[complete.cases(d), ] 

#for (p in 1:length(periods)){
  p = 1 # Only necessary for period 1
  datos.rspf <- subset(datos, periodo %in% periods[p])
  
  # Scale continuous variables here (once you have the subset with the data that you will introduce in the model)
  
  mean.carr <- mean(datos.rspf$carreteras)
  sd.carr <- sd(datos.rspf$carreteras)
  datos.rspf$carreteras.st <- (datos.rspf$carreteras - mean.carr) / sd.carr
  
  mean.cam <- mean(datos.rspf$caminos)
  sd.cam <- sd(datos.rspf$caminos)
  datos.rspf$caminos.st <- (datos.rspf$caminos - mean.cam) / sd.cam
  
  mean.slop <- mean(datos.rspf$Slope)
  sd.slop <- sd(datos.rspf$Slope)
  datos.rspf$slope.st <- (datos.rspf$Slope - mean.slop) / sd.slop
  
  # Parameters
  
  n.B <- 100 # Number of boostrapping 
  n.loop <- 50 # Number of loops to calculate average p-values and stardard errors = 50
  n.var <- 5 # Number of explanatory variables
  match.use.avai <- datos.rspf$ID_Year # Estructura para comparar los datos: Puntos de el mismo individuo y año (En un periodo)
  
  # RSPF
  
  RSF.pval <- matrix(NA, nrow = n.var + 1, ncol = 1) # N_var+1, the number of explanatory variables plus the intercept
  RSF.SE <- matrix(NA, nrow = n.var + 1, ncol = 1)
  RSF.Est <- matrix(NA, nrow = n.var + 1, ncol = 1)
  AIC <- numeric()
  
  for (i in 1:n.loop){
    RSF <- rspf(STATUS ~ as.factor(no_manag) + as.factor(si_manag) + caminos.st + slope.st + 
                  carreteras.st, data = datos.rspf, m = match.use.avai, 
                B = n.B)
    
    summ.RSF <- summary(RSF)
    
    RSF.pval[, 1] <- summ.RSF$coefficients[, 4]
    RSF.SE[,1] <- summ.RSF$coefficients[, 2]
    RSF.Est[,1] <- summ.RSF$coefficients[, 1]
    AIC[1] <- AIC(RSF)
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
  
  
  # Save
  
  setwd("D:/PhD/Fourth chapter/Results/RSF_Ana/Analisis 2/results_prov_rspf/Analisis_AIC2")
  write.csv(results, file = paste("Analisis2_slope_Results_", periods[p],"_", "dm_A1.csv", sep = ""))
  
#}

