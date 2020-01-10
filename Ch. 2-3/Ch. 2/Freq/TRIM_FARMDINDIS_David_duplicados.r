
rm(list=ls())

library(rtrim)

#setwd("D:/Mis documentos/Projectes 2019/Infrastructures/Farmdindis/Script_TRIM_Ana")
setwd("C:/Users/Ana/Documents/PhD/PhD/Second chapter/TRIM/Dades2019")


#####################################################################################################################################################
#####                                                      BASE DE DATOS                                                                      #######
#####################################################################################################################################################

ambSG <- read.csv("Ambit_SG.csv", header = TRUE, sep = ";")
zepaSG <- read.csv("ZEPA_SG.csv", header = TRUE, sep = ";")
occSG <- read.csv("ZEPA_occidentals.csv", header = TRUE, sep = ";")
oriSG <- read.csv("ZEPA_orientals.csv", header = TRUE, sep = ";")

count_data <- rbind(ambSG, zepaSG, occSG, oriSG)

# Mejor cambiar los -1 a NA#
count_data$count[which(count_data$count == -1)] <- NA 
# Si hay duplicados#
count_data <- count_data[-which(duplicated(count_data)), ] 

sp <- unique(count_data$sp)
sp <- sp[-which(sp %in% c("CAENS","PTALC"))] # Quitar las que dan problemas (si el loop no funciona, con el comando print(i))

ambit <- unique(count_data$id_ambit)

# Para almacenar los valores de cada Ã¡mbito para una especie. Recordar sumar + 1 en nrow cada a?o
m <- as.data.frame(matrix(NA, ncol = 14, nrow = 10)) # nrow es numero de aÃ±os * numero de zonas en la que quiero estimar tendencias (para una especie)
colnames(m) <- c("sp", "sector", "year", "index", "index_se", "IC95up", "IC95low", "coef_trend", "coef_trend_se", "ptrend", "pgof", "model_fit", "annual_trend", "description")

# Para juntar todos los Ã¡mbitos para una especie
g <- list()

# Para juntar todas las especies
h <- list()

# TRIM 

for (i in 1:length(sp)){
  
  for (j in 1:length(ambit)){
    
    # Preparar matriz donde voy a almacenar los datos
    data <- count_data[which(count_data$sp == sp[i] & count_data$id_ambit == ambit[j] ), ]  # Subset de la especie y el Ã¡mbito a analizar
    m$sp <- sp[i]                                                                           
    m$sector <- ambit[j]  
    
    # Correr modelo 2
    m1 <- trim(count ~ site + year, data = data, model = 2, serialcor = TRUE, overdisp = TRUE)
    
    sc <- serial_correlation(m1)  # Si el valor de sc es < 0 hay que poner FALSE en el modelo para que no lo tenga en cuenta
    sc <- ifelse( sc > 0, TRUE, FALSE) 
    
    ov <- overdispersion(m1)  # Si el valor de ov es < 1 hay que poner FALSE en el modelo para que no lo tenga en cuenta
    ov <- ifelse( ov >= 1, TRUE, FALSE) 
    
    m2 <- trim(count ~ site + year, data = data, model = 2, serialcor = sc, overdisp = ov) # Modelo con valores de sc y ov ajustados (los warnings que salgan estan corregidos)

    
    # CÃ¡lculo del ?ndice 
    i2 <- index(m2, which="imputed")  
    m[ ,3:5] <- index(m2, which="imputed")
    
    # CÃ¡lculo de CI del ?ndice
     for (k in 1:nrow(m)) {
       
       m$IC95up[k] <- m$index[k] + m$index_se[k] * 1.96
       m$IC95low[k] <- m$index[k] - m$index_se[k] * 1.96
     }
    
    # CÃ¡lculo del coeficiente de tendencia (average beta coefficient). Cojo el "multiplicative", que da una mejor interpretaciÃ³n
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
    
    # Calcular % de cambio de la poblaci?n (annual trend)
    m$annual_trend <- (coef$mul - 1) * 100
  
    # Describir la tendencia (Siguiendo estrictamente las recomendaciones del ICO)
    
    if(m$ptrend[1] > 0.05 & m$annual_trend[1] < 5) { m$description <- "Estable" } # Si la tendencia no es significativa
    if(m$ptrend[1] > 0.05 & m$annual_trend[1] > 5) { m$description <- "Tendència incerta" }
    if(m$ptrend[1] < 0.05 & m$annual_trend[1] < -5) { m$description <- "Disminució forta" } # Si la tendencia es significativa
    if(m$ptrend[1] < 0.05 & m$annual_trend[1] < 0 & m$annual_trend > -5) { m$description <- "Disminució moderada" }
    if(m$ptrend[1] < 0.05 & m$annual_trend[1] > 0 & m$annual_trend < 5) { m$description <- "Augment moderat" }
    if(m$ptrend[1] < 0.05 & m$annual_trend[1] > 5) { m$description <- "Augment fort" }
    
    # Almacenar en lista todos los datos de la especie
    
    g[[j]] <- data.frame(m) 
    all_ambits <- do.call("rbind", g) # Convertir lista en data.frame
  }
  h[[i]] <- all_ambits
  all_data <- do.call("rbind", h)
}

which_na <- all_data[which(is.na(all_data$index)), ] # Para mirar quÃ© especies tiene NA en el Ã­ndice y luego eliminarlas
                                                      # para que no te den problemas en el plot

# Quitar LAMIC porque no estima bien el Ã­ndice
all_data <- all_data[-which(all_data$sp=="LAMIC"), ] 

# Quitar CAGLA de la zepa oriental porque no estima bien el Ã­ndice
all_data <- all_data[-which(all_data$sp == "CLGLA" & all_data$sector == "ZEPA_oriental"), ]

setwd("C:/Users/Ana/Documents/PhD/PhD/Second chapter/TRIM/Dades2019")

#setwd("D:/Mis documentos/Projectes 2019/Infrastructures/Farmdindis/Script_TRIM_Ana")

write.csv(all_data, "Farmdindis_TRIM_David.csv")

#####################################################################################################################################################
#####                                                      PLOT                                                                               #######
#####################################################################################################################################################


setwd("D:/Mis documentos/Projectes 2019/Infrastructures/Farmdindis/Script_TRIM_Ana")

dat <- read.csv("Farmdindis_TRIM.csv")
dat_na <- dat[which(is.na(dat)), ] # Mirar si hay na! Si hay na, no funcionarÃ¡

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
                "Lanius senator", "Melanocorypha calandra", "Emberiza calandra", "Pica pica",
                 "Streptopelia turtur", "Tetrax tetrax")
sp_catala <- c("Perdiu roja", "Torlit", "Terrerola vulgar", "Cucut reial", "Guatlla",
                "Gaig blau", "Gralla", "Xixella", "Cogullada vulgar", "Cogullada fosca", "Botxí meridional",
                "Capsigrany", "Calàndria", "Cruixidell", "Garsa",
                "Tórtora", "Sisó")

# Recordar afegir  l'any en curs a years i allargar  la x...el proper any serà 10.5 
yrs <- c(2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019)
x <- c(0.5,1.5:9.5)


sp <- unique(dat$sp)
sector <- unique(dat$sector)

for (i in 1:length(sp)){
  pdf(paste("TRIM",sp[i],".pdf"))
  par(mfrow = c(2,2))
  par(mar=c(5, 2, 4, 2) + 0.1, oma = c(0,0,3,0))
  
  for (j in 1:length(sector)){
    
    data <- dat[which(dat$sp == sp[i] & dat$sector == sector[j] ), ]
    
    if(nrow(data) == 0) # Esto lo aÃ±ado para que si un sector no tiene datos porque no se haya podido calcular, pase al siguiente y no de error
      next
    
    plot(-100,ylim = c(min(data$IC95low),max(data$IC95up)), xlim=c(0,9),# Recordar augmentar (+1) el l?mit superior de xlim cada any   
         pch = 21, ylab = " ", xlab = " ", axes = FALSE)
    mtext(sector[j], line = 2, side = 3, cex = 1)
    mtext(paste(data$description[1],"(",data$annual_trend,"%",")"), line = 1, side = 3, cex = 0.7, col = "olivedrab3")
    
    # Para aÃ±adir las lineas horizontales. Recordar augmentar cada any el segon numero de clip
    clip(0,10,min(data$IC95low),max(data$IC95up)+0.2) # Porque si no las lineas horizontales se aÃ±aden en todo el plot
    dif <- diff(c(round(min(data$IC95low), digits = 1), round(max(data$IC95up), digits = 1))) 
    div <- dif/4
    location_lines <- seq(from = round(min(data$IC95low), digits = 1), to = round(max(data$IC95up), digits = 1), by = div)
    abline(h = location_lines, col = "lightgrey") # AÃ±ado lineas (siempre 4 lineas puestas en distintos sitios dependiendo del indice)
    
    axis(2, at = seq(round(min(data$IC95low), digits = 0),round(max(data$IC95up), digits = 0), by = 0.5), tick = 0, line = -1.5) # Las labels en y cambian segun la especie (distintos indices)
    axis(1, at = x, tick = 0, labels = yrs)
    
    points(x,data$index, type = "l", col = "olivedrab3", lwd = 2)
    points(x,data$IC95up, type = "l", col = "olivedrab3", lwd = 2, lty = 6)
    points(x,data$IC95low, type = "l", col = "olivedrab3", lwd = 2, lty = 6)
    
  }
  especie <- sp_catala[i]
  especie <- as.character(especie)
  mtext(bquote("Tendencia poblacional de"~ .(especie)~(italic(.(sp_cientif[i])))), 
        line = 0, side = 3, cex = 1.2, col = "olivedrab", outer = TRUE)
  dev.off()
  }

# Plot leyenda para aÃ±adir
par(mfrow = c(1,1))
pdf("legend.pdf")
plot(-100,
     pch = 21, ylab = " ", xlab = " ", axes = FALSE)
legend("topright", legend = c("Índice poblacional", "IC"),
       horiz = TRUE, lty = c(1,6), lwd = c(2,2), seg.len = 2,
       col = "olivedrab3", pch = 18, cex = 0.9, bty = "n")
dev.off()
