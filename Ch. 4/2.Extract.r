

rm(list = ls())

library(raster)
library(rgdal)
library(rgeos)

# 1. ---- Load layers ----

# Human

camin <- raster("S:/PhD/Fourth chapter/GIS/Capas_variables/EPSG_4326/clips/clip_camin_4326.tif")
carret <- raster("S:/PhD/Fourth chapter/GIS/Capas_variables/EPSG_4326/clips/clip_asp_4326.tif")
nuc <- raster("S:/PhD/Fourth chapter/GIS/Capas_variables/EPSG_4326/clips/clip_nuc_4326.tif")

stack_human <- stack(camin, carret, nuc)

# Terrain

slope <- raster("S:/PhD/Fourth chapter/GIS/Capas_variables/EPSG_4326/clips/clip_slope_4326.tif")

# Usos

cereal_17 <- raster("S:/PhD/Fourth chapter/GIS/Capas_variables/EPSG_4326/clips/usos/cereal_17.tif")
cereal_18 <- raster("S:/PhD/Fourth chapter/GIS/Capas_variables/EPSG_4326/clips/usos/cereal_18.tif")

barbecho_17 <- raster("S:/PhD/Fourth chapter/GIS/Capas_variables/EPSG_4326/clips/usos/barbecho_17.tif")
barbecho_18 <- raster("S:/PhD/Fourth chapter/GIS/Capas_variables/EPSG_4326/clips/usos/barbecho_18.tif")

herb_secano_17 <- raster("S:/PhD/Fourth chapter/GIS/Capas_variables/EPSG_4326/clips/usos/herb_secano_17.tif")
herb_secano_18 <- raster("S:/PhD/Fourth chapter/GIS/Capas_variables/EPSG_4326/clips/usos/herb_secano_18.tif")

herb_regadio_17 <- raster("S:/PhD/Fourth chapter/GIS/Capas_variables/EPSG_4326/clips/usos/herb_regadio_17.tif")
herb_regadio_18 <- raster("S:/PhD/Fourth chapter/GIS/Capas_variables/EPSG_4326/clips/usos/herb_regadio_18.tif")

frut_regadio_17 <- raster("S:/PhD/Fourth chapter/GIS/Capas_variables/EPSG_4326/clips/usos/frut_regadio_17.tif")
frut_regadio_18 <- raster("S:/PhD/Fourth chapter/GIS/Capas_variables/EPSG_4326/clips/usos/frut_regadio_18.tif")

olivo_17 <- raster("S:/PhD/Fourth chapter/GIS/Capas_variables/EPSG_4326/clips/usos/olivo_17.tif")
olivo_18 <- raster("S:/PhD/Fourth chapter/GIS/Capas_variables/EPSG_4326/clips/usos/olivo_18.tif")

almendro_17 <- raster("S:/PhD/Fourth chapter/GIS/Capas_variables/EPSG_4326/clips/usos/almendro_17.tif")
almendro_18 <- raster("S:/PhD/Fourth chapter/GIS/Capas_variables/EPSG_4326/clips/usos/almendro_18.tif")

frut_secano_17 <- raster("S:/PhD/Fourth chapter/GIS/Capas_variables/EPSG_4326/clips/usos/frut_secano_17.tif")
frut_secano_18 <- raster("S:/PhD/Fourth chapter/GIS/Capas_variables/EPSG_4326/clips/usos/frut_secano_18.tif")

pastos <- raster("S:/PhD/Fourth chapter/GIS/Capas_variables/EPSG_4326/clips/usos/pastos.tif")
forestal <- raster("S:/PhD/Fourth chapter/GIS/Capas_variables/EPSG_4326/clips/usos/forestal.tif")

stack_usos <- stack(cereal_17, cereal_18, barbecho_17, barbecho_18, herb_secano_17, herb_secano_18,
                herb_regadio_17, herb_regadio_18, frut_regadio_17, frut_regadio_18, olivo_17, olivo_18,
                almendro_17, almendro_18, frut_secano_17, frut_secano_18)

stack_pastos <- stack(pastos, forestal)

# 2. ---- Load coordinates ----

setwd("S:/PhD/Fourth chapter/Data")

d <- read.csv("random_used_points.csv", header = TRUE)

# Add year to extract the variables of each year
d$Year <- NA
d$Year[which(d$Logger_ID %in% c("CIP03", "CIP04"))] <- 2017
d$Year[which(d$Logger_ID %in% c("PIC02", "PIC15", "PIC17"))] <- 2018

coord <- d[ ,c("x","y")] # Coordinates used and random

# 3. ---- Extract values ----

cells <- cellFromXY(stack_human, coord) # 1. Tells the number of the cells where the coord. fall
cov_human <- stack_human[cells]           # 2. Returns the value of those cells in the stack

cells <- cellFromXY(slope, coord) 
cov_slope <- slope[cells]  

cells <- cellFromXY(stack_usos, coord) 
cov_usos <- stack_usos[cells] 
cov_usos <- as.data.frame(cov_usos)

cells <- cellFromXY(stack_pastos, coord) 
cov_pastos <- stack_pastos[cells] 

df <- data.frame(d, cov_human, cov_slope, cov_usos, cov_pastos) # Join coordinates with extracted values

# Arrange the column of usos (accordying to the year of the individual)

df$cereal <- NA
df$barbecho <- NA
df$herb_secano <- NA
df$herb_regadio <- NA
df$frut_regadio <- NA
df$olivo <- NA
df$almendro <- NA
df$frut_secano <- NA

for(i in 1:nrow(df)){
  df$cereal[i] <- ifelse(df$Year[i] == 2017,  df$cereal_17[i], df$cereal_18[i])
  df$barbecho[i] <- ifelse(df$Year[i] == 2017,  df$barbecho_17[i], df$barbecho_18[i])
  df$herb_secano[i] <- ifelse(df$Year[i] == 2017,  df$herb_secano_17[i], df$herb_secano_18[i])
  df$herb_regadio[i] <- ifelse(df$Year[i] == 2017,  df$herb_regadio_17[i], df$herb_regadio_18[i])
  df$frut_regadio[i] <- ifelse(df$Year[i] == 2017,  df$frut_regadio_17[i], df$frut_regadio_18[i])
  df$olivo[i] <- ifelse(df$Year[i] == 2017,  df$olivo_17[i], df$olivo_18[i])
  df$almendro[i] <- ifelse(df$Year[i] == 2017,  df$almendro_17[i], df$almendro_18[i])
  df$frut_secano[i] <- ifelse(df$Year[i] == 2017,  df$frut_secano_17[i], df$frut_secano_18[i])
}

# Check
check_cereal <- df[which(df$almendro_17 == 1 & df$almendro_18 == 0), ]

# Select columns
colnames(df)
df <- df[ ,which(colnames(df) %in% c("Logger_ID", "y", "x", "ID_p", "used", "Year", 
                                   "clip_camin_4326", "clip_asp_4326", "clip_nuc_4326", "cov_slope",
                                   "pastos", "forestal", "cereal", "barbecho", "herb_secano", "herb_regadio", "frut_regadio", "olivo", "almendro", "frut_secano"))]

colnames(df)[7]<- "dist_caminos"
colnames(df)[8]<- "dist_carreteras"
colnames(df)[9]<- "dist_nucleosurb"
colnames(df)[10]<- "pendiente"


setwd("S:/PhD/Fourth chapter/Data")
write.csv (df, "covariates.csv")


# Check the ones that fall outside
study_area <- readOGR("S:/PhD/Fourth chapter/GIS/Capas_variables/EPSG_4326", "studyarea_pteroclids_EPSG_4326")
mcp_99 <- readOGR("S:/PhD/Fourth chapter/GPS Cataluña", "mcp99")
coordinates(df) <- df[ ,c("x", "y")]
proj4string(df) <- proj4string(study_area) # As SpatialPointsDataFrame

plot(study_area)
plot(mcp_99, add = TRUE)
points(df)

writeOGR(df, 'S:/PhD/Fourth chapter/GIS', "covariates_gps", driver="ESRI Shapefile")


