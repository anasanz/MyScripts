
rm(list=ls())

library(rtrim)

setwd("C:/Users/ana.sanz/OneDrive/PhD/Second chapter/TRIM/Dades")

#####################################################################################################################################################
#####                                                      BASE DE DATOS                                                                      #######
#####################################################################################################################################################

ambSG <- read.csv("Ambit_SG.csv", header = TRUE, sep = ";")
zepaSG <- read.csv("ZEPA_SG.csv", header = TRUE, sep = ";")
occSG <- read.csv("ZEPA_occidentals.csv", header = TRUE, sep = ";")
oriSG <- read.csv("ZEPA_orientals.csv", header = TRUE, sep = ";")

count_data <- rbind(ambSG, zepaSG, occSG, oriSG)

sp <- unique(count_data$sp)
sp <- sp[-which(sp %in% c("CAENS", "PTALC"))] # Quitar las que dan problemas (si el loop no funciona, con el comando )

ambit <- unique(count_data$id_ambit)

# Para almacenar los valores de cada ámbito para una especie
m <- as.data.frame(matrix(NA, ncol = 14, nrow = 9)) # nrow es numero de años * numero de zonas en la que quiero estimar tendencias (para una especie)
colnames(m) <- c("sp", "sector", "year", "index", "index_se", "IC95up", "IC95low", "coef_trend", "coef_trend_se", "ptrend", "pgof", "model_fit", "annual_trend", "description")

# Para juntar todos los ámbitos para una especie
g <- list()

# Para juntar todas las especies
h <- list()

# TRIM 

for (i in 1:length(sp)){
  
  for (j in 1:length(ambit)){
    
    # Preparar matriz donde voy a almacenar los datos
    data <- count_data[which(count_data$sp == sp[i] & count_data$id_ambit == ambit[j] ), ]  # Subset de la especie y el ámbito a analizar
    m$sp <- sp[i]                                                                           
    m$sector <- ambit[j]  
    
    # Correr modelo 2
    m1 <- trim(count ~ site + year, data = data, model = 2, serialcor = TRUE, overdisp = TRUE)
    
    sc <- serial_correlation(m1)  # Si el valor de sc es < 0 hay que poner FALSE en el modelo para que no lo tenga en cuenta
    sc <- ifelse( sc > 0, TRUE, FALSE) 
    
    ov <- overdispersion(m1)  # Si el valor de ov es < 1 hay que poner FALSE en el modelo para que no lo tenga en cuenta
    ov <- ifelse( ov >= 1, TRUE, FALSE) 
    
    m2 <- trim(count ~ site + year, data = data, model = 2, serialcor = sc, overdisp = ov) # Modelo con valores de sc y ov ajustados (los warnings que salgan estan corregidos)

    
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
    gof <- gof(m2)
    m$pgof <- gof$chi2$p
    m$model_fit <- ifelse (m$pgof[1] > 0.05, "yes", "no" )
    
    # Calcular % de cambio de la población (annual trend)
    m$annual_trend <- (coef$mul - 1) * 100
  
    # Describir la tendencia (Siguiendo estrictamente las recomendaciones del ICO)
    
    if(m$ptrend[1] > 0.05 & m$annual_trend[1] < 5) { m$description <- "Estable" } # Si la tendencia no es significativa
    if(m$ptrend[1] > 0.05 & m$annual_trend[1] > 5) { m$description <- "Tendència incerta" }
    if(m$ptrend[1] < 0.05 & m$annual_trend[1] < -5) { m$description <- "Disminució forta" } # Si la tendencia es significativa
    if(m$ptrend[1] < 0.05 & m$annual_trend[1] < 0 & m$annual_trend > -5) { m$description <- "Disminució moderada" }
    if(m$ptrend[1] < 0.05 & m$annual_trend[1] > 0 & m$annual_trend < 5) { m$description <- "Augment moderat" }
    if(m$ptrend[1] < 0.05 & m$annual_trend[1] > 5) { m$description <- "Augment moderat" }
    
    # Almacenar en lista todos los datos de la especie
    
    g[[j]] <- data.frame(m) 
    all_ambits <- do.call("rbind", g) # Convertir lista en data.frame
  }
  h[[i]] <- all_ambits
  all_data <- do.call("rbind", h)
}

setwd("C:/Users/ana.sanz/OneDrive/PhD/Second chapter/TRIM/Resultats")
write.csv(all_data, "Farmdindis_TRIM.csv")

#####################################################################################################################################################
#####                                                      PLOT                                                                               #######
#####################################################################################################################################################

setwd("C:/Users/ana.sanz/OneDrive/PhD/Second chapter/TRIM/Resultats")
dat <- read.csv("Farmdindis_TRIM.csv")

# Redondear tendencia para plot
dat$annual_trend <- round(dat$annual_trend, digits = 0)

# Cambiar nombre de los sectores para hacer los plots
dat$sector <- as.character(dat$sector)
dat$sector[which(dat$sector == "Gral")] <- "Àmbit Segarra - Garrigues"
dat$sector[which(dat$sector == "ZEPA_gral")] <- "ZEPAs Àmbit Segarra - Garrigues"
dat$sector[which(dat$sector == " ZEPA_occidental")] <- "ZEPAs occidentals"
dat$sector[which(dat$sector == "ZEPA_oriental")] <- "ZEPAs orientals"

# Vector con nombre de las especies en el mismo orden para hacer los plots

sp_cientif <- c("Alectoris rufa", "Burhinus oedicnemus", "Calandrella brachydactyla", "Clamator glandarius", "Coturnix coturnix",
                "Coracias garrulus", "Corvus monedula", "Columba oenas", "Galerida cristata", "Galerida theklae", "Lanius meridionalis",
                "Larus michahellis", "Lanius senator", "Melanocorypha calandra", "Emberiza calandra", "Pica pica",
                 "Streptopelia turtur", "Tetrax tetrax")

yrs <- c(2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018)
x <- c(0.5,1.5:8.5)


sp <- unique(dat$sp)
sector <- unique(dat$sector)

for (i in 1:length(sp)){
  
  par(mfrow = c(2,2))
  mtext(sp[i], line = 3, side = 3, cex = 1.5)
  
  for (j in 1:length(sector)){
    
    data <- dat[which(dat$sp == sp[i] & dat$sector == sector[j] ), ]
    plot(-100,ylim = c(0,2), xlim=c(0,9),
         pch = 21, ylab = " ", xlab = " ", axes = FALSE)
    mtext(sector[j], line = 2, side = 3, cex = 1)
    mtext(paste(data$description[1],"(",data$annual_trend,"%",")"), line = 1, side = 3, cex = 0.7, col = "olivedrab3")
    clip(0,9,0,2) # Porque si no las lineas horizontales se añaden en todo el plot
    abline(h = c(0, 0.5, 1, 1.5, 2), col = "lightgrey")
    axis(2, at = c(0, 0.5, 1, 1.5, 2), tick = 0)
    axis(1, at = x, tick = 0, labels = yrs)
    points(x,data$index, type = "l", col = "olivedrab3", lwd = 3)
    points(x,data$IC95up, type = "l", col = "olivedrab3", lwd = 3, lty = 2)
    points(x,data$IC95low, type = "l", col = "olivedrab3", lwd = 3, lty = 2)
    par(xpd=T, mar=par()$mar+c(0,0,0,0)) # Para poder poner la leyenda fuera del plot
    legend(x=1,y=-0.7, legend = c("Índice poblacional", "IC"),
           horiz = TRUE, lty = c(1,2), lwd = c(3,3), seg.len = 3, bty = "n",
           col = "olivedrab3", pch = 18, cex = 0.9 )
  } }


plot(overall(m1))




