
## -------------------------------------------------
##              Generate random points for 
##                HABITAT AVAILABILITY
## ------------------------------------------------- 

library(rgdal)
library(dplyr)
library(adehabitatHR)
library(sp)

## ---- Load data ----

# Load GPS locations

data <- read.table("D:/PhD/Fourth chapter/Data/2_matrix_RSPF.txt", header = T, dec = ",")
data <- data[data$STATUS == 1, c(2:17)]

# Load MCP

Logger_ID <- unique(data$Logger_ID)

MCP <- list()
for (i in 1:length(Bird.ID)){
  MCP[[i]] <- readOGR(dsn = "D:/PhD/Fourth chapter/Data/Full_analysis_Rocio", layer = paste("mcp", Bird.ID[i], sep = "_"))
}
names(MCP) <- Logger_ID 

## ----  Simulate random positions ----

# Determine number of random locations to simulate 

years <- c(2017,2018,2019)
periods <- c("Pre", "PreRep", "Rep")

loc_years <- list()

for (t in 1:length(years)){
  data_year <- data[data$year %in% years[t], ]
  
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

# Simulate 1 or 2 times number of available points than used points: First one time, will be called A1


tmp.id.year <- list()


for (i in 1:length(Logger_ID)){
  
  mcpid <- MCP[names(MCP) == Logger_ID[i]][[1]]
  
for (t in 1:length(years)){
  
  tmp.id.year.p <- data.frame( Logger_ID = NA, year = NA, periodo = NA, x = NA, y = NA )
  
  for (p in 1:length(periods)) {
    
  nlocs_total <- loc_years[[t]]
  nlocs_sim <- nlocs_total[rownames(nlocs_total) %in% Logger_ID[i], colnames(nlocs_total) %in% periods[p]]
  if(nlocs_sim == 0) next
  rdm.sp <- spsample(mcpid, nlocs_sim, type = "random")
  # plot(mcpid)
  # points(rdm.sp)
  
  tmp.id.year.p <- rbind(tmp.id.year.p, data.frame( Logger_ID = Logger_ID[i]
                                         , year = years[t]
                                         , periodo = periods[p] 
                                         , x = coordinates(rdm.sp)[,1]
                                         , y = coordinates(rdm.sp)[,2]) )
  }
  
  tmp.id.year[[t]] <- tmp.id.year.p # dataframe with all random points from 1 id 1 year (the three periods)
  # Aquí: tengo que hacer do.call de tmp.id.year (dat)

}
  }









data <- read.table("D:/PhD/Fourth chapter/Data/Full_analysis_Rocio/D_XYgps_positions_no_regadio_ETRS89_tot.txt", 
                   header = T, dec = ",", sep = "\t")
data <- data[ ,c(1:5,13,14)]

data2 <- data[order(data$Year, data$Logger_ID), ]

coordinates(data) <- data[ ,c("X_25831", "Y_25831")]
data@proj4string <- MCP[[1]]@proj4string






ID2017 <- data.frame(Id = datos2017$Logger_ID)
xyID2017 <- datos2017[, c("X_25831", "Y_25831")]

MCP[[1]]@proj4string
