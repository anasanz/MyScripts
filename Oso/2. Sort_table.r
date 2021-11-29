
## -------------------------------------------------
##                 Sort out table
## ------------------------------------------------- 

rm(list = ls())

library(sp)
library(rgdal)
library(lubridate)

setwd("D:/Oso/Datos")
os <- read.csv("Seguiment_Ossos_Pirineus_1996_2020.csv", header = TRUE, row.names = NULL)
os <- os[,-c(1)] 

## ---- Differenciate cubs in critic period ----

# Date column to differenciate month

os$Date_register_formated <- os$Date_register 
os$Date_register_formated <- as.Date(os$Date_register_formated, format = "%d/%m/%Y")
os$month <- month(ymd(os$Date_register_formated)) 

# For female with cubs, re-classify the ones that have a cub of less than 6 month (critic period)

os$With_cubs_estimated_new <- os$With_cubs_estimated

for (i in 1:nrow(os)) {
  if(is.na(os$With_cubs_estimated[i])) next
  if(os$With_cubs_estimated[i] == 1) { # First year cubs
    if(is.na(os$month[i])) next
    if(os$month[i] < 7) { # Before July
      os$With_cubs_estimated_new[i] <- "<6month" # Place <6 month if it has cubs (estimated number) that are below 6 months
    }}}

os <- os[,c(1:22,36,23:35)] # Check variable is well calculated
os <- os[ ,-c(22,35)]# Delete extra-columns and sort table
colnames(os)[colnames(os) %in% c("With_cubs_estimated_new")] <- "With_cubs_estimated"


## ---- Age class ----

os$Age2 <- os$Age # To convert to numeric to do classification (will produce NA)
os$Age2 <- as.numeric(os$Age2)

for (i in 1:nrow(os)) { 
  if(is.na(os$Age2[i]) | is.na(os$month[i])) next
  if(os$Age2[i] > 4) {
    os$Age_class[i] <- "Adult" } else if (os$Age2[i] <= 4 & os$Age2[i] > 1.5) {
    os$Age_class[i] <- "Subadult" } else if (os$Age2[i] == 1.5 | os$Age2[i] == 1) {
    os$Age_class[i] <- "Cub2" } else if (os$Age2[i] == 0 & os$month[i] > 7) {
    os$Age_class[i] <- "Cub1" } else if (os$Age2[i] == 0 & os$month[i] < 7) {
    os$Age_class[i] <- "Cub0" }
}

os <- os[,-c(34,35)] # Delete extra-columns

## ---- Add region and country ----

os_na <- os[which(is.na(os$X)), ] # Extract NA to join later
os <- os[-which(is.na(os$X)), ] # Remove NA to convert coordinates

coordinates(os) <- os[,c("x_long","y_lat")] # Spatial object
os@proj4string <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

# Load layer with regions

os@data <- os@data[ ,-which(colnames(os@data) %in% c("Country", "Region"))]

map <- readOGR(dsn = "D:/Oso/Datos/GIS/Countries", layer = "clip_pyros2")
d <- over(os,map) # Overlay to see where each point fall

os@data <- cbind(os@data, d)
colnames(os@data)[which(colnames(os@data) %in% c("NAME_0", "NAME_1"))] <- c("Country", "Region")

os@data <- os@data[,c(1:17,32:33,18:31)]

# Export GIS layer with coordinates and good attribute table (last version)
# writeOGR(os, "D:/Oso/Datos/GIS/Seguiment_GIS_layer", "Seguiment_Ossos_Pirineus_1996_2020_coordinates_final", driver = "ESRI Shapefile")

os_data <- rbind(os@data, os_na)
os_data <- arrange(os_data, ID_obs) 

# Correct mistakes
unique(os_data$Confirmed_Individual)
os_data$Confirmed_Individual[os_data$Confirmed_Individual == "Hvala "] <- "Hvala"
os_data$Confirmed_Individual[os_data$Confirmed_Individual == "Hvala  "] <- "Hvala"
os_data$Confirmed_Individual[os_data$Confirmed_Individual == "Nere "] <- "Nere"

setwd("D:/Oso/Datos")
#write.csv(os_data, "Seguiment_Ossos_Pirineus_1996_2020_taula_final.csv")

## ---- Add columns to plot later ----

# Column of critical observations (only cubs or females with cubs of less than 6 month)

os_data$Female_cubs_critic <- ifelse(os_data$Age_class == "Cub0" | 
                                       os_data$With_cubs_estimated == "<6month",
                                     1,0)
os_data$Female_cubs_critic[is.na(os_data$Female_cubs_critic)] <- 0

# Column of observations of 1st year (cubs or females with cubs of 1 year, INCLUDING THE ONES OF < 6 MONTH)

os_data$Female_cubs_year1 <- ifelse(os_data$Age_class == "Cub0" | 
                                      os_data$Age_class == "Cub1" |
                                      os_data$With_cubs_estimated == "<6month" |
                                      os_data$With_cubs_estimated == "1"
                                      ,
                                     1,0)
os_data$Female_cubs_year1[is.na(os_data$Female_cubs_year1)] <- 0  

# Column of observations of 2nd year (cubs or females with cubs of 2 year or indetermined age, INCLUDING THE ONES OF < 6 MONTH AND 1 YEAR)

os_data$Female_cubs_year2 <- ifelse(os_data$Age_class == "Cub0" | 
                                      os_data$Age_class == "Cub1" |
                                      os_data$Age_class == "Cub2" |
                                      os_data$With_cubs_estimated == "<6month" |
                                      os_data$With_cubs_estimated == "1" |
                                      os_data$With_cubs_estimated == "2" |
                                      os_data$With_cubs_estimated == "3" 
                                    ,
                                    1,0)
os_data$Female_cubs_year2[is.na(os_data$Female_cubs_year2)] <- 0 

setwd("D:/Oso/Datos")
write.csv(os_data, "Seguiment_Ossos_Pirineus_1996_2020_PLOT.csv")
