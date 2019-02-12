
rm(list=ls())

library(rtrim)

setwd("C:/Users/ana.sanz/OneDrive/PhD/Second chapter/TRIM/Dades")

ambSG <- read.csv("Ambit_SG.csv", header = TRUE, sep = ";")
zepaSG <- read.csv("ZEPA_SG.csv", header = TRUE, sep = ";")
occSG <- read.csv("ZEPA_occidentals.csv", header = TRUE, sep = ";")
oriSG <- read.csv("ZEPA_orientals.csv", header = TRUE, sep = ";")

count_data <- rbind(ambSG, zepaSG, occSG, oriSG)

sp <- unique(count_data$sp)
ambit <- unique(count_data$id_ambit)

m <- as.data.frame(matrix(NA, ncol = 14, nrow = 9)) # nrow es numero de años * numero de zonas en la que quiero estimar tendencias (para una especie)
colnames(m) <- c("sp", "sector", "year", "index", "index_se", "IC95up", "IC95low", "coef_trend", "coef_trend_se", "ptrend", "pgof", "model_fit", "annual_trend", "description")


#TRIM ALRUF----

for (i in 1:length(sp)){
  
  for (j in 1:length(ambit)){
    
    # Preparar matriz donde voy a almacenar los datos
    data <- count_data[which(count_data$sp == sp[i] & count_data$id_ambit == ambit[j] ), ]  # Subset de la especie y el ámbito a analizar
    m$sp <- sp[i]                                                                           
    m$sector <- ambit[j]  
    
    # Correr modelo 2
    m2 <- trim(count ~ site + year, data = data, model = 2, serialcor = TRUE, overdisp = TRUE) 
    
    # Cálculo del índice 
    i2 <- index(m2, which="imputed")  
    m[ ,3:5] <- index(m2, which="imputed")
    
    # Cálculo de CI del índice
     for (k in 1:nrow(m)) {
       
       m$IC95up[k] <- m$index[k] + m$index_se[k] * 1.96
       m$IC95low[k] <- m$index[k] - m$index_se[k] * 1.96
     }
    
    # Cálculo del coeficiente de tendencia (average beta coefficient). Cojo el "multiplicative", que da una mejor interpretación
    coef <- coefficients(m2)
    m$coef_trend <- coef$mul
    m$coef_trend_se <- coef$se_mul
    
    
    # p-value del coeficiente de tendencia (p < 0.05 = tendencia significativa)
    wald_p <- wald(m2) 
    m$ptrend <- wald_p$slope$p 
    
    # Goodness of fit (p < 0.05 = el modelo no se ajusta )
    m$pgof <- gof$chi2$p
    m$model_fit <- ifelse (m$pgof[1] > 0.05, "yes", "no" )
    
    # Calcular % de cambio de la población (annual trend)
    m$annual_trend <- (coef$mul - 1) * 100
  
    # Describir la tendencia
    
    if()
    
    m$description <- ifelse (m$ptrend[1] > 0.05, "yes", "no" )
      
      
  }
}
ALRUF<- subset(ambSG, sp == "ALRUF")
m$sp <- "ALRUF"
m2 <- trim(count ~ site + year, data = ALRUF, model = 2, serialcor = TRUE, overdisp = TRUE)
i2 <- index(m2, which="imputed")



# summarize the model
summary(m2)


#Retrieve goodness-of-fit

gof <- gof(m2)
gof$chi2$p

#Extract the coefficients

coefficients(m2)
index(m3, which="both")

#Plot with overall slope

plot(overall(m1))

plot(i1)

# tendencia anual: (Mulplicative - 1)*100
(0.803705 - 1)*100



