## -------------------------------------------------
##  MAP AVAILABILITY HABITAT SELECTION ANALISIS 1 
## ------------------------------------------------- 


rm(list=ls())

library(rgdal)
library(raster)
library(dplyr)
library(sp)
library(rgeos)

# ---- LOAD USOS ----

# Layers managed in ARCGIS:
# 1. clip_sigpac_usos_4326 y clip_dun17_usos_4326.shp to WGS_1984_UTM_Zone_31N
# 2. New layer sigpac with only pastos forest caminos: clip_sigpac_pastosforestcaminos_WGS_1984_UTM_Zone_31N
# 3. Clip clip_dun17_usos_WGS_1984_UTM_Zone_31N.shp and clip_sigpac_pastosforestcaminos_WGS_1984_UTM_Zone_31N AGAIN with MCP (better for map)
# 4. Merge clipMCP_dun17_usos_WGS_1984_UTM_Zone_31N and clipMCP_sigpac_pastosforestcaminos_WGS_1984_UTM_Zone_31N in one layer : clipMCP17_sigpacdun_WGS_1984_UTM_Zone_31N
# 5. LOAD


map17 <- readOGR("D:/PhD/Fourth chapter/Data/GIS/Capas_Rocío/usos", "clipMCP17_sigpacdun_WGS_1984_UTM_Zone_31N")
map18 <- readOGR("D:/PhD/Fourth chapter/Data/GIS/Capas_Rocío/usos", "clipMCP18_sigpacdun_WGS_1984_UTM_Zone_31N")
map19 <- readOGR("D:/PhD/Fourth chapter/Data/GIS/Capas_Rocío/usos", "clipMCP19_sigpacdun_WGS_1984_UTM_Zone_31N")

# ---- LOAD RESULTS (By period, because I will plot by period) ----

setwd("D:/PhD/Fourth chapter/Results/RSF_Ana/Analisis 1/results_rspf")

p1 <- read.csv("Results_Pre_dm_random_loc_A1.csv", sep = ";")
p1 <- p1[,-5]
p1$period <- "p1"
p1 <- p1[nrow(p1):1, ]
p1[,1] <- c("DistAsp", "Slope", "DistGrav", "NatVeg", "Forest", "Herb.irri", "Fruit.irri", "Almond", "Olive", "Cereal", "Fallow", "Intercept")
colnames(p1) <- c("X", "Estimate", "SE", "p", "period")

p2 <- read.csv("Results_PreRep_dm_random_loc_A1.csv", sep = ";")
p2 <- p2[,-5]
p2$period <- "p2"
p2 <- p2[nrow(p2):1, ]
p2[,1] <- c("DistAsp", "Slope", "DistGrav", "NatVeg", "Forest", "Herb.irri", "Fruit.irri", "Almond", "Olive", "Cereal", "Fallow", "Intercept")
colnames(p2) <- c("X", "Estimate", "SE", "p", "period")

p3 <- read.csv("Results_Rep_dm_random_loc_A1.csv", sep = ";")
p3 <- p3[,-5]
p3$period <- "p3"
p3 <- p3[nrow(p3):1, ]
p3[,1] <- c("DistAsp", "Slope", "DistGrav", "NatVeg", "Forest", "Herb.irri", "Fruit.irri", "Almond", "Olive", "Cereal", "Fallow", "Intercept")
colnames(p3) <- c("X", "Estimate", "SE", "p", "period")


periods <- list(p1, p2, p3)

for (j in 1:length(periods)){
  
  p <- periods[[j]] 
  
  for (i in 1:nrow(p)){
    if (p$p[i] > 0.05) {p$colour[i] = "grey"} else {
      p$colour[i] <- ifelse (p$Estimate[i] > 0, "blue", "red") 
    }}
  
  p$colour[which(p$X == "DistGrav")] <- "red" # To plot roads in red (they select positively distance to roads)
  colnames(p)[colnames(p) == "X"] <- "uso_mapa"
  
  periods[[j]] <- p
  
}

# ---- Sort out columns (one with USO, same name than in results) ----

maps <- c(map17, map18, map19)


for (i in 1:length(maps)){ 
  
  maps[[i]]@data$uso_mapa <- NA
  
  maps[[i]]@data$uso_mapa[which(maps[[i]]@data$pastos == "PASTOS")] <- "NatVeg"
  maps[[i]]@data$uso_mapa[which(maps[[i]]@data$pastos == "FORESTAL")] <- "Forest"
  maps[[i]]@data$uso_mapa[which(maps[[i]]@data$us == "CA")] <- "DistGrav"
  maps[[i]]@data$uso_mapa[which(maps[[i]]@data$uso == "HERBACEOS DE REGADIO")] <- "Herb.irri"
  maps[[i]]@data$uso_mapa[which(maps[[i]]@data$uso == "FRUTALES DE REGADIO")] <- "Fruit.irri"
  maps[[i]]@data$uso_mapa[which(maps[[i]]@data$uso == "ALMENDRO")] <- "Almond"
  maps[[i]]@data$uso_mapa[which(maps[[i]]@data$uso == "OLIVO")] <- "Olive"
  maps[[i]]@data$uso_mapa[which(maps[[i]]@data$uso == "CEREAL")] <- "Cereal"
  maps[[i]]@data$uso_mapa[which(maps[[i]]@data$uso == "BARBECHO")] <- "Fallow"
  
  # maps[[i]]@data[is.na(maps[[i]]@data$uso_mapa), ] # Estos son "OTROS HERBÁCEOS DE SECANO"
  
}


## ---- Calculate surface under red and blue every year and period ----

# Change colnames periods in new data to join with maps
periods2 <- periods
for (j in 1:length(periods2)){
  colnames(periods2[[j]])[c(2,3,4,6)] <- paste(colnames(periods2[[j]])[c(2,3,4,6)], periods2[[j]]$period[1], sep = "_") 
}

# In each layer, join a column with the color of polygons each period  
for (i in 1:length(maps)){
  for (j in 1:length(periods2)){
    maps[[i]]@data <- left_join(maps[[i]]@data, periods2[[j]], by = "uso_mapa") # Pego la información de selección para cada periodo en la capa
  }}

# In each layer, calculate area of red and blue
# Modify map layers in new object
col <- c("blue", "red")

data_allyears <- list()
data <- data.frame(Select = c(NA,NA,NA), Avoid = c(NA,NA,NA))

for (i in 1:length(maps)){
  
  for (j in 1:length(periods2)){
    maps2 <- maps # To get back all the columns again in every loop
    column_period <- paste("colour",periods2[[j]]$period[1], sep = "_") 
    maps2[[i]]@data <- maps2[[i]]@data[ ,which(colnames(maps2[[i]]@data) %in% c("uso_mapa", column_period))] # Select the column "color period"
    
    for (c in 1:length(col)){
      area <- gArea(maps2[[i]][which(maps2[[i]]@data[,2] == col[c]), ])/10000 # Calculate area of a given color
      data[j,c] <- area # Store it
    }}
  data_allyears[[i]] <- data
}



#  ---- Plot by year and period ----

## -------------------------------------------------
##                    HORIZONTAL
## ------------------------------------------------- 

# All together


setwd("D:/PhD/Fourth chapter/Results/Figures")
pdf("maps17_noborder2.pdf",9, 2)

par(mfrow = c(1,3),
    mar = c(0,0,0,0),
    oma = c(0,0,0,0))

for (i in 1:length(periods)) {
  
  maps2 <- maps
  maps2[[1]]@data <- left_join(maps2[[1]]@data, periods[[i]], by = "uso_mapa")
  plot(maps2[[1]], col = maps2[[1]]$colour, border = NA, xlim = c(ext@xmin + 400, ext@xmax - 400), ylim = c(ext@ymin + 400 , ext@ymax - 400)) # I think is better with no border (dif tries with p2)
  box(which = "plot", lty = "solid")
} 

dev.off()


par(mfrow = c(1,1))

i = 2
maps2 <- maps
maps2[[1]]@data <- left_join(maps2[[1]]@data, periods[[i]], by = "uso_mapa")
plot(maps2[[1]], col = maps2[[1]]$colour, border = maps2[[1]]$colour, axes = TRUE, xlim = c(ext@xmin + 400, ext@xmax - 400), ylim = c(ext@ymin + 400 , ext@ymax - 400))




ext@xmin + 1000

## -------------------------------------------------
##                    VERTICAL
## ------------------------------------------------- 

# 2017

setwd("D:/PhD/Fourth chapter/Results/Figures/Fig1")
pdf("maps17_noborder2_A1_vert.pdf",4, 8)

par(mfrow = c(3,1),
    mar = c(0,0,0,0),
    oma = c(0,0,0,0))

ext <- extent(maps2[[1]])

for (i in 1:length(periods)) {
  
  maps2 <- maps
  maps2[[1]]@data <- left_join(maps2[[1]]@data, periods[[i]], by = "uso_mapa")
  plot(maps2[[1]], col = maps2[[1]]$colour, border = NA, xlim = c(ext@xmin + 400, ext@xmax - 400), ylim = c(ext@ymin + 400 , ext@ymax - 400)) # I think is better with no border (dif tries with p2)
  #box(which = "plot", lty = "solid")
} 

dev.off()

# 2018 (FOR SM)

setwd("D:/PhD/Fourth chapter/Results/Figures/Fig1")
pdf("maps18_noborder2_A1_vert.pdf",4, 8)

par(mfrow = c(3,1),
    mar = c(0,0,0,0),
    oma = c(0,0,0,0))

for (i in 1:length(periods)) {
  
  maps2 <- maps
  ext <- extent(maps2[[2]])
  maps2[[2]]@data <- left_join(maps2[[2]]@data, periods[[i]], by = "uso_mapa")
  plot(maps2[[2]], col = maps2[[2]]$colour, border = NA, xlim = c(ext@xmin + 400, ext@xmax - 400), ylim = c(ext@ymin + 400 , ext@ymax - 400)) # I think is better with no border (dif tries with p2)
  #box(which = "plot", lty = "solid")
} 

dev.off()

# 2019 (FOR SM)

setwd("D:/PhD/Fourth chapter/Results/Figures/Fig1")
pdf("maps19_noborder2_A1_vert.pdf",4, 8)

par(mfrow = c(3,1),
    mar = c(0,0,0,0),
    oma = c(0,0,0,0))

for (i in 1:length(periods)) {
  
  maps2 <- maps
  ext <- extent(maps2[[3]])
  maps2[[3]]@data <- left_join(maps2[[3]]@data, periods[[i]], by = "uso_mapa")
  plot(maps2[[3]], col = maps2[[3]]$colour, border = NA, xlim = c(ext@xmin + 400, ext@xmax - 400), ylim = c(ext@ymin + 400 , ext@ymax - 400)) # I think is better with no border (dif tries with p2)
  #box(which = "plot", lty = "solid")
} 

dev.off()



## -------------------------------------------------
###           TRY BORDER/NO BORDER...
## ------------------------------------------------- 

# One by one (try p2)

setwd("D:/PhD/Fourth chapter/Results/Figures")
pdf("maps17_p2_normalborder.pdf")

par(mfrow = c(1,1))

i = 2
maps2 <- maps
maps2[[1]]@data <- left_join(maps2[[1]]@data, periods[[i]], by = "uso_mapa")
plot(maps2[[1]], col = maps2[[1]]$colour, border = maps2[[1]]$colour)

dev.off()


setwd("D:/PhD/Fourth chapter/Results/Figures")
pdf("maps17_p2_noborder.pdf")

par(mfrow = c(1,1))

i = 2
maps2 <- maps
maps2[[1]]@data <- left_join(maps2[[1]]@data, periods[[i]], by = "uso_mapa")
plot(maps2[[1]], col = maps2[[1]]$colour, border = NA)

dev.off() 


setwd("D:/PhD/Fourth chapter/Results/Figures")
pdf("maps17_p2_smallborder.pdf")

par(mfrow = c(1,1))

i = 2
maps2 <- maps
maps2[[1]]@data <- left_join(maps2[[1]]@data, periods[[i]], by = "uso_mapa")
plot(maps2[[1]], col = maps2[[1]]$colour, border = maps2[[1]]$colour, lwd = 0.3)

dev.off() 


setwd("D:/PhD/Fourth chapter/Results/Figures")
pdf("maps17_p2_blackborder.pdf")

par(mfrow = c(1,1))

i = 2
maps2 <- maps
maps2[[1]]@data <- left_join(maps2[[1]]@data, periods[[i]], by = "uso_mapa")
plot(maps2[[1]], col = maps2[[1]]$colour, border = "black", lwd = 0.3)

dev.off() 












