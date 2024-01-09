## -------------------------------------------------
##              Generate random points for 
##                HABITAT AVAILABILITY
## ------------------------------------------------- 

rm(list=ls())

library(rgdal)
library(dplyr)
library(adehabitatHR)
library(sp)
library(raster)
library(rgeos)

## ---- Load data ----

# Load GPS locations

data <- read.table("D:/PhD/Fourth chapter/Data/Revision/2_matrix_RSPF_uselocations.txt", header = T, dec = ",")

#REV: Data set sent by Rocio to specify the year of the location (Year) and the year that should join the observation with DUN
# This case only happens in december 2016 (year2 = 2017) and december 2018 (year2 = 2019)
# Identify the observations where the dun belongs to the next year

prevYear <- data[data$year2 != data$year, ] 

# Mirar a que corresponden estos puntos en el análisis previo

data2 <- read.table("D:/PhD/Fourth chapter/Data/2_matrix_RSPF.txt", header = T, dec = ",")
data2 <- data2[data2$STATUS == 1, c(2:20)]
data2[which(data2$x == 303007 & data2$y == 4602618), ] 
data2[which(data2$x == 304980 & data2$y == 4601607), ] 

#REV: Parece que cambié el nombre para las de 2016
# Pero no lo cambié para 2018, por lo que de estos datos está mal:
##  - El número de randompoints simulados en el periodo pre de 2018 (deben pertenecer a pre2019)
##  - La extracción del hábitat del periodo pre de 2018


# Load MCP

Logger_ID <- unique(data$Logger_ID)

MCP <- list()
for (i in 1:length(Logger_ID)){
  MCP[[i]] <- readOGR(dsn = "D:/PhD/Fourth chapter/Data/Full_analysis_Rocio", layer = paste("mcp", Logger_ID[i], sep = "_"))
}
names(MCP) <- Logger_ID 

# Load MAPS to restrict where random points are simulated

map17 <- readOGR("D:/PhD/Fourth chapter/Data/GIS/Capas_Rocío/usos", "clipMCP17_sigpacdun_WGS_1984_UTM_Zone_31N")
map18 <- readOGR("D:/PhD/Fourth chapter/Data/GIS/Capas_Rocío/usos", "clipMCP18_sigpacdun_WGS_1984_UTM_Zone_31N")
map19 <- readOGR("D:/PhD/Fourth chapter/Data/GIS/Capas_Rocío/usos", "clipMCP19_sigpacdun_WGS_1984_UTM_Zone_31N")

maps <- c(map17, map18, map19)

for (i in 1:length(maps)){ 
  maps[[i]]@proj4string <- MCP[[1]]@proj4string
}

for (i in 1:length(maps)){ 
  
  maps[[i]]@data$uso_mapa <- NA
  
  maps[[i]]@data$uso_mapa[which(maps[[i]]@data$pastos == "PASTOS")] <- "PASTOS"
  maps[[i]]@data$uso_mapa[which(maps[[i]]@data$pastos == "FORESTAL")] <- "FORESTAL"
  maps[[i]]@data$uso_mapa[which(maps[[i]]@data$us == "CA")] <- "CA"
  maps[[i]]@data$uso_mapa[which(maps[[i]]@data$uso == "HERBACEOS DE REGADIO")] <- "HERBACEOS DE REGADIO"
  maps[[i]]@data$uso_mapa[which(maps[[i]]@data$uso == "FRUTALES DE REGADIO")] <- "FRUTALES DE REGADIO"
  maps[[i]]@data$uso_mapa[which(maps[[i]]@data$uso == "ALMENDRO")] <- "ALMENDRO"
  maps[[i]]@data$uso_mapa[which(maps[[i]]@data$uso == "OLIVO")] <- "OLIVO"
  maps[[i]]@data$uso_mapa[which(maps[[i]]@data$uso == "CEREAL")] <- "CEREAL"
  maps[[i]]@data$uso_mapa[which(maps[[i]]@data$uso == "BARBECHO")] <- "BARBECHO"
  maps[[i]]@data$uso_mapa[which(maps[[i]]@data$uso == "OTROS HERBACEOS DE SECANO")] <- "OTROS HERBACEOS DE SECANO"
  
}

# Constrain to select polygons where I want the GPS positions to fall

constrain_random <- c("FRUTALES DE REGADIO", "CEREAL", "HERBACEOS DE REGADIO", "BARBECHO", "OTROS HERBACEOS DE SECANO",
                      "OLIVO", "ALMENDRO", "PASTOS", "FORESTAL") 

for (i in 1:length(maps)){ 
  maps[[i]] <- maps[[i]][which(maps[[i]]$uso_mapa %in% constrain_random), ]
}

## ----  Simulate random positions ----

# Determine number of random locations to simulate 

years <- c(2017,2018,2019)
periods <- c("Pre", "PreRep", "Rep")

loc_years <- list()

#REV: Here I have to take the variable year2 because it refers to the dun-year

for (t in 1:length(years)){
  data_year <- data[data$year2 %in% years[t], ]
  
  locations_ID <- data.frame(matrix(0, ncol = length(periods), nrow = length(unique(data$Logger_ID))))
  colnames(locations_ID) <- periods
  rownames(locations_ID) <- unique(data$Logger_ID)
  
  for (i in 1:length(periods)) {
    datos.rspf <- subset(data_year, periodo%in%periods[i])
    n.id <- table(datos.rspf["Logger_ID"])
    locations_ID[rownames(locations_ID) %in% names(n.id), colnames(locations_ID) %in% periods[i]] <- n.id
  }
  loc_years[[t]] <- locations_ID
}

#REV: The loc_years in pre2018 (that were from dec) are now in 2019, and sum to the ones of pre2019 (jan-feb) from before
## The number in this case is different from before because the used locations where also wrongly attributed,
## and when attributing them properly the number of used that fell in fallow changed

# Simulate 1 or 2 times number of available points than used points: First one time, will be called A1, two times the number of used points A2...
#REV: Now because the nlocs per year is modified in correct, I simulate the nlocs in the correct year with the correct dun


tmp.id.year <- list()
tmp.id <- list()

for (i in 1:length(Logger_ID)){
  
  mcpid <- MCP[names(MCP) == Logger_ID[i]][[1]]
  
  for (t in 1:length(years)){
    
    tmp.id.year.p <- data.frame( Logger_ID = NA, year = NA, periodo = NA, x = NA, y = NA )
    
    for (p in 1:length(periods)) {
      
      nlocs_total <- loc_years[[t]]
      nlocs_sim <- nlocs_total[rownames(nlocs_total) %in% Logger_ID[i], colnames(nlocs_total) %in% periods[p]]
      if(nlocs_sim == 0) next
      mcpid2 <- crop(maps[[t]], mcpid) # Restrict MCP to land uses where I want to generate random points
      
      # 2 modes to create random points: Proportional to number of used locations or proportional to MCP area (comment out rdm.sp depending on mode)
      
      # Mode 1: Multiplying number of used locations* 1, 2, 3....
      rdm.sp <- spsample(mcpid2, nlocs_sim, type = "random") # Simulate random points for 1 period, year and ID and year (A1, single available points)
      
      # Mode 2: Number of locations proportional to the size of the home range
      #area.mcp <- gArea(mcpid, byid = F)/1000000 # km2
      #n.puntos <- round(area.mcp *25 , 0) # 25/50/100/200 points per km2
      
      #rdm.sp <- spsample(mcpid2, n.puntos, type = "random")
      
      #setwd("D:/PhD/Fourth chapter/Results/RSF_Ana/Analisis 1/Availability/Aprop0.25") # Save to see distribution and density of points
      #pdf(paste(Logger_ID[i],"_",years[t], "_", periods[p],".pdf", sep = ""))
      #plot(mcpid2, main = paste(Logger_ID[i],"_",years[t], "_", periods[p], sep = ""), col = "lightgrey")
      #points(rdm.sp, pch = 18)
      #dev.off()
      
      tmp.id.year.p <- rbind(tmp.id.year.p, data.frame( Logger_ID = Logger_ID[i]
                                                        , year = years[t]
                                                        , periodo = periods[p] 
                                                        , x = coordinates(rdm.sp)[,1]
                                                        , y = coordinates(rdm.sp)[,2]) ) # Here I accumulate the three periods from 1 id and 1 year
    }
    
    tmp.id.year[[t]] <- tmp.id.year.p # dataframe with all random points from 1 id 1 year (the three periods)
  }
  tmp.id[[i]] <- do.call(rbind.data.frame, tmp.id.year) 
}

random_loc <- do.call(rbind.data.frame, tmp.id) 
random_loc <- random_loc[complete.cases(random_loc), ] # Same number of rows that data

# Check that it fits with number locations from loc_years
nrow(random_loc[which(random_loc$Logger_ID == "PIC17" & random_loc$year == 2019 & random_loc$periodo == "Pre"), ])


setwd("D:/PhD/Fourth chapter/Results/RSF_Ana/Analisis 1/random_loc/Revision")
write.csv(random_loc, "random_loc_A1.csv")

# Save also used locs without old random points
setwd("D:/PhD/Fourth chapter/Results/RSF_Ana/Analisis 1/Revision")
write.csv(data, "used_loc.csv")







