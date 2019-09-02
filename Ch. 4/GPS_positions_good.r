
###### MANAGE GPS POSITIONS (ONLY REMOVE FLYING OBSERVATIONS) ######

rm(list = ls())

library(raster)
library(rgdal)

# Load GPS positions already divided in periods in arcgis

cip03_p1 <- readOGR("S:/PhD/Fourth chapter/GPS Cataluña/Ganga/17-18_rep/periods_all_positions", "cip03_p1")
cip03_p2 <- readOGR("S:/PhD/Fourth chapter/GPS Cataluña/Ganga/17-18_rep/periods_all_positions", "cip03_p2")
cip03_p3 <- readOGR("S:/PhD/Fourth chapter/GPS Cataluña/Ganga/17-18_rep/periods_all_positions", "cip03_p3")

cip04_p1 <- readOGR("S:/PhD/Fourth chapter/GPS Cataluña/Ganga/17-18_rep/periods_all_positions", "cip04_p1")
cip04_p2 <- readOGR("S:/PhD/Fourth chapter/GPS Cataluña/Ganga/17-18_rep/periods_all_positions", "cip04_p2")
cip04_p3 <- readOGR("S:/PhD/Fourth chapter/GPS Cataluña/Ganga/17-18_rep/periods_all_positions", "cip04_p3")

pic02_p2 <- readOGR("S:/PhD/Fourth chapter/GPS Cataluña/Ganga/17-18_rep/periods_all_positions", "pic02_p2")
pic02_p3 <- readOGR("S:/PhD/Fourth chapter/GPS Cataluña/Ganga/17-18_rep/periods_all_positions", "pic02_p3")

pic15_p2 <- readOGR("S:/PhD/Fourth chapter/GPS Cataluña/Ganga/17-18_rep/periods_all_positions", "pic15_p2")
pic15_p3 <- readOGR("S:/PhD/Fourth chapter/GPS Cataluña/Ganga/17-18_rep/periods_all_positions", "pic15_p3")

pic17_p2 <- readOGR("S:/PhD/Fourth chapter/GPS Cataluña/Ganga/17-18_rep/periods_all_positions", "pic17_p2")
pic17_p3 <- readOGR("S:/PhD/Fourth chapter/GPS Cataluña/Ganga/17-18_rep/periods_all_positions", "pic17_p3")


# Remove flying observations

cip03_p1 <- cip03_p1[which(cip03_p1$Speed < 1), ]
cip03_p2 <- cip03_p2[which(cip03_p2$Speed < 1), ]
cip03_p3 <- cip03_p3[which(cip03_p3$Speed < 1), ]

cip04_p1 <- cip04_p1[which(cip04_p1$Speed < 1), ]
cip04_p2 <- cip04_p2[which(cip04_p2$Speed < 1), ]
cip04_p3 <- cip04_p3[which(cip04_p3$Speed < 1), ]

pic02_p2 <- pic02_p2[which(pic02_p2$Speed < 1), ]
pic02_p3 <- pic02_p3[which(pic02_p3$Speed < 1), ]

pic15_p2 <- pic15_p2[which(pic15_p2$Speed < 1), ]
pic15_p3 <- pic15_p3[which(pic15_p3$Speed < 1), ]

pic17_p2 <- pic17_p2[which(pic17_p2$Speed < 1), ]
pic17_p3 <- pic17_p3[which(pic17_p3$Speed < 1), ]

# Remove missing observations

plot(pic02_p2)
df <- pic02_p2@data[which(pic02_p2@data$Latitude == 0), ] # Delete them (error GPS)
pic02_p2 <- pic02_p2[-which(pic02_p2$Latitude == 0), ]

plot(pic02_p3)
df <- pic02_p3@data[which(pic02_p3@data$Latitude == 0), ] # Delete them (error GPS)
pic02_p3 <- pic02_p3[-which(pic02_p3$Latitude == 0), ]

plot(pic15_p3)
df <- pic15_p3@data[which(pic15_p3@data$Latitude == 0), ] # Delete them (error GPS)
pic15_p3 <- pic15_p3[-which(pic15_p3$Latitude == 0), ]

plot(pic17_p3)
df <- pic17_p3@data[which(pic17_p3@data$Latitude == 0), ] # Delete them (error GPS)
pic17_p3 <- pic17_p3[-which(pic17_p3$Latitude == 0), ]

# Save

writeOGR(cip03_p1, 'S:/PhD/Fourth chapter/GPS Cataluña/Ganga/17-18_rep/periods_no_flying', "cip03_p1", driver="ESRI Shapefile")
writeOGR(cip03_p2, 'S:/PhD/Fourth chapter/GPS Cataluña/Ganga/17-18_rep/periods_no_flying', "cip03_p2", driver="ESRI Shapefile")
writeOGR(cip03_p3, 'S:/PhD/Fourth chapter/GPS Cataluña/Ganga/17-18_rep/periods_no_flying', "cip03_p3", driver="ESRI Shapefile")

writeOGR(cip04_p1, 'S:/PhD/Fourth chapter/GPS Cataluña/Ganga/17-18_rep/periods_no_flying', "cip04_p1", driver="ESRI Shapefile")
writeOGR(cip04_p2, 'S:/PhD/Fourth chapter/GPS Cataluña/Ganga/17-18_rep/periods_no_flying', "cip04_p2", driver="ESRI Shapefile")
writeOGR(cip04_p3, 'S:/PhD/Fourth chapter/GPS Cataluña/Ganga/17-18_rep/periods_no_flying', "cip04_p3", driver="ESRI Shapefile")

writeOGR(pic02_p2, 'S:/PhD/Fourth chapter/GPS Cataluña/Ganga/17-18_rep/periods_no_flying', "pic02_p2", driver="ESRI Shapefile")
writeOGR(pic02_p3, 'S:/PhD/Fourth chapter/GPS Cataluña/Ganga/17-18_rep/periods_no_flying', "pic02_p3", driver="ESRI Shapefile")

writeOGR(pic15_p2, 'S:/PhD/Fourth chapter/GPS Cataluña/Ganga/17-18_rep/periods_no_flying', "pic15_p2", driver="ESRI Shapefile")
writeOGR(pic15_p3, 'S:/PhD/Fourth chapter/GPS Cataluña/Ganga/17-18_rep/periods_no_flying', "pic15_p3", driver="ESRI Shapefile")

writeOGR(pic17_p2, 'S:/PhD/Fourth chapter/GPS Cataluña/Ganga/17-18_rep/periods_no_flying', "pic17_p2", driver="ESRI Shapefile")
writeOGR(pic17_p3, 'S:/PhD/Fourth chapter/GPS Cataluña/Ganga/17-18_rep/periods_no_flying', "pic17_p3", driver="ESRI Shapefile")


#### JOIN THEM IN A FULL DATASET ####

# Load GPS positions 

cip03_p1 <- readOGR("S:/PhD/Fourth chapter/GPS Cataluña/Ganga/17-18_rep/periods_no_flying", "cip03_p1")
cip03_p2 <- readOGR("S:/PhD/Fourth chapter/GPS Cataluña/Ganga/17-18_rep/periods_no_flying", "cip03_p2")
cip03_p3 <- readOGR("S:/PhD/Fourth chapter/GPS Cataluña/Ganga/17-18_rep/periods_no_flying", "cip03_p3")

cip04_p1 <- readOGR("S:/PhD/Fourth chapter/GPS Cataluña/Ganga/17-18_rep/periods_no_flying", "cip04_p1")
cip04_p2 <- readOGR("S:/PhD/Fourth chapter/GPS Cataluña/Ganga/17-18_rep/periods_no_flying", "cip04_p2")
cip04_p3 <- readOGR("S:/PhD/Fourth chapter/GPS Cataluña/Ganga/17-18_rep/periods_no_flying", "cip04_p3")

pic02_p2 <- readOGR("S:/PhD/Fourth chapter/GPS Cataluña/Ganga/17-18_rep/periods_no_flying", "pic02_p2")
pic02_p3 <- readOGR("S:/PhD/Fourth chapter/GPS Cataluña/Ganga/17-18_rep/periods_no_flying", "pic02_p3")

pic15_p2 <- readOGR("S:/PhD/Fourth chapter/GPS Cataluña/Ganga/17-18_rep/periods_no_flying", "pic15_p2")
pic15_p3 <- readOGR("S:/PhD/Fourth chapter/GPS Cataluña/Ganga/17-18_rep/periods_no_flying", "pic15_p3")

pic17_p2 <- readOGR("S:/PhD/Fourth chapter/GPS Cataluña/Ganga/17-18_rep/periods_no_flying", "pic17_p2")
pic17_p3 <- readOGR("S:/PhD/Fourth chapter/GPS Cataluña/Ganga/17-18_rep/periods_no_flying", "pic17_p3")
proj4string(pic17_p3)

# Create column to identify each period

cip03_p1$ID_p <- "cip03_p1"
cip03_p2$ID_p <- "cip03_p2"
cip03_p3$ID_p <- "cip03_p3"

cip04_p1$ID_p <- "cip04_p1"
cip04_p2$ID_p <- "cip04_p2"
cip04_p3$ID_p <- "cip04_p3"

pic02_p2$ID_p <- "pic02_p2"
pic02_p3$ID_p <- "pic02_p3"

pic15_p2$ID_p <- "pic15_p2"
pic15_p3$ID_p <- "pic15_p3"

pic17_p2$ID_p <- "pic17_p2"
pic17_p3$ID_p <- "pic17_p3"

# Select same columns

cip03_p1@data <- cip03_p1@data[ ,which(colnames(cip03_p1@data) %in% c("Logger_ID", "ID_p", "Latitude", "Longitude"))]
cip03_p2@data <- cip03_p2@data[ ,which(colnames(cip03_p2@data) %in% c("Logger_ID", "ID_p", "Latitude", "Longitude"))]
cip03_p3@data <- cip03_p3@data[ ,which(colnames(cip03_p3@data) %in% c("Logger_ID", "ID_p", "Latitude", "Longitude"))]

cip04_p1@data <- cip04_p1@data[ ,which(colnames(cip04_p1@data) %in% c("Logger_ID", "ID_p", "Latitude", "Longitude"))]
cip04_p2@data <- cip04_p2@data[ ,which(colnames(cip04_p2@data) %in% c("Logger_ID", "ID_p", "Latitude", "Longitude"))]
cip04_p3@data <- cip04_p3@data[ ,which(colnames(cip04_p3@data) %in% c("Logger_ID", "ID_p", "Latitude", "Longitude"))]

pic02_p2@data <- pic02_p2@data[ ,which(colnames(pic02_p2@data) %in% c("Logger_ID", "ID_p", "Latitude", "Longitude"))]
pic02_p3@data <- pic02_p3@data[ ,which(colnames(pic02_p3@data) %in% c("Logger_ID", "ID_p", "Latitude", "Longitude"))]

pic15_p2@data <- pic15_p2@data[ ,which(colnames(pic15_p2@data) %in% c("Logger_ID", "ID_p", "Latitude", "Longitude"))]
pic15_p3@data <- pic15_p3@data[ ,which(colnames(pic15_p3@data) %in% c("Logger_ID", "ID_p", "Latitude", "Longitude"))]

pic17_p2@data <- pic17_p2@data[ ,which(colnames(pic17_p2@data) %in% c("Logger_ID", "ID_p", "Latitude", "Longitude"))]
pic17_p3@data <- pic17_p3@data[ ,which(colnames(pic17_p3@data) %in% c("Logger_ID", "ID_p", "Latitude", "Longitude"))]

# Join all datasets

gps <- rbind(cip03_p1, cip03_p2, cip03_p3, cip04_p1, cip04_p2, cip04_p3,
             pic02_p2, pic02_p3, pic15_p2, pic15_p3, pic17_p2, pic17_p3)

writeOGR(gps, 'S:/PhD/Fourth chapter/GPS Cataluña/Ganga/17-18_rep/periods_no_flying', "gps", driver="ESRI Shapefile")
