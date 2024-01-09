## -------------------------------------------------
##                RSPF - Three periods
## ------------------------------------------------- 

rm(list = ls())

library(ResourceSelection)

temp <- setwd("D:/PhD/Fourth chapter/Results/RSF_Ana/Analisis 1/data_matrix/Revision")
temp <- list.files(pattern = "*.csv")
data <- lapply(temp, read.csv, sep = ",")

##REV: I only did it for a set of locations
# Loop for different sets of random locations

#for(xxx in 1:length(temp)){
  xxx = 1
  datos <- data[[xxx]]
  periods <- c("Pre", "PreRep", "Rep")
  
  d <- datos[ ,c(10,20)] # Remove NA
  datos <- datos[complete.cases(d), ]
  
  x <- datos[which(datos$STATUS == 1), ] # Just to check number of obs
  
  # Check correlation
  correlation <- cor(datos[ ,c(10:21)]) 
  which(correlation > 0.4 & correlation < 1)
  
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
    n.var <- 11 # Number of explanatory variables
    match.use.avai <- datos.rspf$ID_Year # Estructura para comparar los datos: Puntos de el mismo individuo y año (En un periodo)
    
    # RSPF
    
    RSF.pval <- matrix(NA, nrow = n.var + 1, ncol = n.loop) # N_var+1, the number of explanatory variables plus the intercept
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
    
    # Save
    
    #temp[xxx] <- substr(temp[xxx],1,nchar(temp[xxx])-4) # Name for saving
    setwd("D:/Ana/Results/chapter4/results_rspf")
    write.csv(results, file = paste("Results_", periods[p],"_", temp[xxx], sep = ""))
  #}
  
#}