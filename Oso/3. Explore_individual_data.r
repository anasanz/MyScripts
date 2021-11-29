
## -------------------------------------------------
##                    Explore data
## ------------------------------------------------- 

rm(list = ls())

library(sp)
library(rgdal)
library(dplyr)
library(tidyr)
library(viridis) 
library(RColorBrewer)

setwd("D:/Oso/Datos")
os <- read.csv("Seguiment_Ossos_Pirineus_1996_2020_taula_final.csv", header = TRUE, row.names = NULL)

os <- os[-which(is.na(os$X)), ] # Remove NA
os <- os[,-1] 

coordinates(os) <- os[,c("x_long","y_lat")] # Spatial object
os@proj4string <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

# Load basemap
map1 <- readOGR(dsn = "D:/Oso/Datos/GIS/Countries", layer = "clip_pyros2")

plot(map1, col = "grey")
points(os, col = "red", pch = 19)

# Load info database
info <- read.csv("Info_individuals.csv", header = TRUE, row.names = NULL, sep = ";")
info <- info[,c(4,5,8,9)]


## ---- Summary identified individuals ----

osdat <- os@data

# Nº obs per individual, and identify those with > 10 obs
os_nobs <- data.frame(osdat %>% group_by(Confirmed_Individual) %>% summarise(n()))
id_more10 <- os_nobs$Confirmed_Individual[os_nobs$n.. > 9]

# Nº obs per year
os_sum <- data.frame(osdat %>% group_by(Confirmed_Individual, Year) %>% summarise(n()))
colnames(os_sum)[3] <- "n_obs"
os_sum <- spread(os_sum, Year, n_obs)
os_sum[is.na(os_sum)] <- 0

## ---- Plot observations identified individuals ----

os <- os[-which(os$Confirmed_Individual == "Indetermined"), ] # Remove indetermined

# Save different GIS layer per individual to explore in arcgis
id <- unique(os$Confirmed_Individual)

#for (i in 1:length(id)){
#  os_id <- os[which(os$Confirmed_Individual == id[i]), ] # Select individual
#  writeOGR(os_id, "D:/Oso/Datos/GIS/Seguiment_GIS_layer/Identified_individuals", paste("ID_", id[i], sep = ""), driver = "ESRI Shapefile")
#}



#### MALES ####

os_males <- os[which(os$Sex == "M"), ]
id_males <- unique(os_males$Confirmed_Individual)
id_males <- id_males[id_males %in% id_more10] # ID males with more than 10 observations (all)

# Visual 1 (Three categories: Cub, subadult, adult)  

pdf("ID_Males1.pdf")  
par(mfrow = c(3,3),
    oma = c(3,3,5,2),
    mar = c(1,1,1,0))

for (i in 1:length(id_males)) {
  os_id <- os_males[which(os_males$Confirmed_Individual == id_males[i]), ] # Select individual
  os_id_adult <- os_id[which(os_id$Age_class == "Adult"), ]
  os_id_subadult <- os_id[which(os_id$Age_class == "Subadult"), ]
  os_id_cub <- os_id[which(os_id$Age_class == "Cub0" | os_id$Age_class == "Cub1" | os_id$Age_class == "Cub2"), ]
  
  plot(map1, col = "lightgrey", border = "grey")
  mtext(id_males[i], side = 3, line = 0, cex = 1)
  
  points(os_id_adult, col = adjustcolor("purple", alpha.f = 1), pch = 19)
  points(os_id_subadult, col = adjustcolor("orange", alpha.f = 0.8), pch = 19)
  points(os_id_cub, col = adjustcolor("darkgreen", alpha.f = 1), lwd = 1.8, pch = 8)
  
  mtext("Males", side = 3, line = 1, cex = 1.2, outer = TRUE)
}

dev.off()

# Visual 2: Three categories (Cub, Subadult, Adult) and gradient of colours within

# Create three color palettes
color_cub <- c("lightyellow1", "yellow", "gold", "orange")
col_subadult <- c("darkorange1", "orangered","firebrick1", "red1")
color_adult <- c("red3", "red4", "darkred", "black")

setwd("D:/Oso/Datos/Plots")

pdf("ID_Males2.pdf")  
par(mfrow = c(3,3),
    oma = c(3,3,5,2),
    mar = c(1,1,1,0))

for (i in 1:length(id_males)) {
  os_id <- os_males[which(os_males$Confirmed_Individual == id_males[i]), ] # Select individual
  os_id_adult <- os_id[which(os_id$Age_class == "Adult"), ]
  os_id_subadult <- os_id[which(os_id$Age_class == "Subadult"), ]
  os_id_cub <- os_id[which(os_id$Age_class == "Cub0" | os_id$Age_class == "Cub1" | os_id$Age_class == "Cub2"), ]
  
  plot(map1, col = "lightgrey", border = "grey")
  mtext(id_males[i], side = 3, line = 0, cex = 1)

  points(os_id_adult, col = color_adult, pch = 19)
  points(os_id_subadult, col = col_subadult, pch = 19)
  points(os_id_cub, col = color_cub, pch = 19)
  
  mtext("Males", side = 3, line = 1, cex = 1.2, outer = TRUE)
}

dev.off()



