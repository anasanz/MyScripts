
# Calculate proportion of fincas sg that has each transect

rm(list=ls())

library(rgdal)
library(raster)
library(rgeos)


#Load layers
sg15 <- readOGR("C:/Users/ana.sanz/OneDrive/PhD/Second chapter/Farmdindis/Maps", "sg_2015") # Fincas sg
t <- readOGR("C:/Users/ana.sanz/OneDrive/PhD/Second chapter/Farmdindis/Maps/transectes", "Trans_2017") # Transectos

#Rasterize fincas sg
sg15$field <- "1"
sg15$field <- as.factor(sg15$field) #Ready to rasterize (1/0)
r1 <- raster(resolution=25, extent(t), crs = "+proj=utm +zone=31 +ellps=intl +units=m +no_defs") # CRS of t15
ras_sg15 <- rasterize(sg15,r1, field = "field", background = 0)

#setwd("C:/Users/ana.sanz/OneDrive/PhD/Second chapter/Data/GIS")
#writeRaster(ras_sg15, "ras_sg15", overwrite = TRUE)

#Transects as spatial lines
t15 <- t[which(t$FETS2015 == 1), ]
l15 <- SpatialLines(t15@lines, proj4string = CRS("+proj=utm +zone=31 +ellps=intl +units=m +no_defs"))

# Extract values
# With extract function
v15_ext <- extract(ras_sg15, l15, buffer = 200, fun = sum, df = TRUE)
v15_ext@data
# With buffer: different results
buf <- gBuffer(l15, byid = TRUE, width = 200)
v15_ext_buf <- extract(ras_sg15, buf, fun = sum, df = TRUE)
plot(buf)

?extract
?gBuffer
