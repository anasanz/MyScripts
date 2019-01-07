
library(rgdal)
library(dplyr)
rm(list=ls())

#############################################################################
###                  MAP SPATIAL CLUSTERING TREATMENTS                   ###
#############################################################################

# Spatial join with the analyzed data (with the treatments) and the layer
# to make the map and see if treatments were clustered

setwd("C:/Users/Ana/Documents/PhD/First chapter/Datos/Datos barbechos arrendados/Variables")

d <- read.csv("Data_path_submission.csv") # Analyzed data and treatments

dat <- d[which(d$Species == "SC"), which(colnames(d) %in% c("CF_A", "Codi_Finca", "Year", "Zone", "agri_practice"))]
dat$ap <- dat$agri_practice
# Layers
sg15 <- readOGR("C:/Users/Ana/Documents/PhD/Second chapter/Data/GIS/SG", layer = "SG_2015_EPSG23031")
sg16 <- readOGR("C:/Users/Ana/Documents/PhD/Second chapter/Data/GIS/SG", layer = "SG_2016_EPSG23031")
sg17 <- readOGR("C:/Users/Ana/Documents/PhD/Second chapter/Data/GIS/SG", layer = "SG_2017_EPSG23031")

# Join

#2015
year15 <- dat[which(dat$Year == 2015), ]
sg15@data <- left_join(sg15@data,year15, by = "Codi_Finca")
sg15_treat <- sg15[which(!is.na(sg15@data$agri_practice)),]
writeOGR(sg15_treat, dsn = "C:/Users/Ana/Documents/PhD/First chapter/Datos/Datos barbechos arrendados/GIS layers", layer = "SG_2015_ANALYZED", driver = "ESRI Shapefile")

#2016
year16 <- dat[which(dat$Year == 2016), ]
sg16@data <- left_join(sg16@data,year16, by = "Codi_Finca")
sg16_treat <- sg16[which(!is.na(sg16@data$agri_practice)),]
writeOGR(sg16_treat, dsn = "C:/Users/Ana/Documents/PhD/First chapter/Datos/Datos barbechos arrendados/GIS layers", layer = "SG_2016_ANALYZED", driver = "ESRI Shapefile")

#2017
year17 <- dat[which(dat$Year == 2017), ]
sg17@data <- left_join(sg17@data,year17, by = "Codi_Finca") #177? There is a mistake, there should be 175
sg17_treat <- sg17[which(!is.na(sg17@data$agri_practice)),]

writeOGR(sg17_treat, dsn = "C:/Users/Ana/Documents/PhD/First chapter/Datos/Datos barbechos arrendados/GIS layers", layer = "SG_2017_ANALYZED", driver = "ESRI Shapefile")

length(unique(year17$Codi_Finca))
length(unique(sg17_treat@data$Codi_Finca))
sg17_treat@data[which,]

x <- data.frame(sg17@data)
which(duplicated(sg17_treat@data$Codi_Finca))
sg17_treat@data[which(duplicated(sg17_treat@data$Codi_Finca)),] # BM31A and GR20A
length(which(!is.na(sg17@data$agri_practice)))
length(which(!is.na(x$agri_practice)))

sg15_treat@data$CF_A

head(d)
unique(d$Codi_Finca)

##########################################################################################################
# Add variables long_lat to check spatial autocorrelation
# Add it to the dataset of the fields that had mistakes (confused NA by 0 values)
setwd("C:/Users/Ana/Documents/PhD/First chapter/Datos/Datos barbechos arrendados/Variables")

d <- read.csv("Data_path_manuscript2.csv")
sg15 <- readOGR("C:/Users/Ana/Documents/PhD/First chapter/Datos/Datos barbechos arrendados/GIS layers", layer = "SG_2015_ANALYZED")

c15 <- data.frame(Lon_x = coordinates(sg15)[ ,1], Lat_y = coordinates(sg15)[ ,2])
sg15@data$Lon_x <- c15$Lon_x
sg15@data$Lat_y <- c15$Lat_y
writeOGR(sg15, dsn = "C:/Users/Ana/Documents/PhD/First chapter/Datos/Datos barbechos arrendados/GIS layers", layer = "SG_2015_ANALYZED_SP", driver = "ESRI Shapefile")


sg16 <- readOGR("C:/Users/Ana/Documents/PhD/First chapter/Datos/Datos barbechos arrendados/GIS layers", layer = "SG_2016_ANALYZED")

c16 <- data.frame(Lon_x = coordinates(sg16)[ ,1], Lat_y = coordinates(sg16)[ ,2])
sg16@data$Lon_x <- c16$Lon_x
sg16@data$Lat_y <- c16$Lat_y
writeOGR(sg16, dsn = "C:/Users/Ana/Documents/PhD/First chapter/Datos/Datos barbechos arrendados/GIS layers", layer = "SG_2016_ANALYZED_SP", driver = "ESRI Shapefile")

sg17 <- readOGR("C:/Users/Ana/Documents/PhD/First chapter/Datos/Datos barbechos arrendados/GIS layers", layer = "SG_2017_ANALYZED")

c17 <- data.frame(Lon_x = coordinates(sg17)[ ,1], Lat_y = coordinates(sg17)[ ,2])
sg17@data$Lon_x <- c17$Lon_x
sg17@data$Lat_y <- c17$Lat_y
writeOGR(sg17, dsn = "C:/Users/Ana/Documents/PhD/First chapter/Datos/Datos barbechos arrendados/GIS layers", layer = "SG_2017_ANALYZED_SP", driver = "ESRI Shapefile")

# Include coordinates in dataset for analysis (not only in layers)

# Unify column with codi finca
colnames(sg15@data)[which(colnames(sg15@data) %in% "Cod_Fnc")] <- "Codi_Finca"
colnames(sg16@data)[which(colnames(sg16@data) %in% "Cod_Fnc")] <- "Codi_Finca"
colnames(sg17@data)[which(colnames(sg17@data) %in% "Cod_Fnc")] <- "Codi_Finca"

# Remove duplicates from 2017
sg17 <- sg17[-which(sg17@data$OBJECTI == 269), ]
sg17 <- sg17[-which(sg17@data$OBJECTI == 445), ]

#Select columns to join to the data to be analyzed
sg15@data <- sg15@data[ ,which(colnames(sg15@data) %in% c("Codi_Finca", "CF_A", "Lon_x", "Lat_y"))]
sg16@data <- sg16@data[ ,which(colnames(sg16@data) %in% c("Codi_Finca", "CF_A", "Lon_x", "Lat_y"))]
sg17@data <- sg17@data[ ,which(colnames(sg17@data) %in% c("Codi_Finca", "CF_A", "Lon_x", "Lat_y"))]

sg <- rbind(sg15@data, sg16@data, sg17@data)


# Join with dataset for analysis
d_sp <- left_join(d,sg)

setwd("C:/Users/Ana/Documents/PhD/First chapter/Datos/Datos barbechos arrendados/Variables")
write.csv(d_sp, "Data_path_submission2_sp.csv") # The 2 is because 7 fields have been deleted from the previous version (Script Clean_data)


#############################################################################
###                      HISTOGRAM TREATMENTS                             ###
#############################################################################

# Frequency of each treatment per year and sector
dat$agri_practice <- as.numeric(dat$agri_practice)
unique(dat$agri_practice)
hist(dat$agri_practice, breaks = c(0,1,2,3,4,5), axes = TRUE) # All
year <- c(2015,2016,2017)

# Occidentales
dat_oc <- dat[which(dat$Zone == "West"), ]
dat_oc$agri_practice <- as.numeric(dat_oc$agri_practice)

par(mfrow = c(1,3))
for (i in 1:3){
  dat_oc_year <- dat_oc[which(dat_oc$Year == i),]
  hist(dat_oc_year$agri_practice, breaks = c(0,1,2,3,4,5))
}
