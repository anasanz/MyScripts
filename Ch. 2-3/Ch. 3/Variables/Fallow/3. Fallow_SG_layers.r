
rm(list=ls())

library(rgdal)
library(raster)

# ==================================================================================
#                 Create layer MANAGED (Join SG - Managed fields MAS DE MELONS)
# ==================================================================================

# LOAD SG

sg15 <- readOGR("D:/PhD/Third chapter/GIS/Fallow/SG", layer = "SG15_23031")
sg16 <- readOGR("D:/PhD/Third chapter/GIS/Fallow/SG", layer = "SG16_23031")
sg17 <- readOGR("D:/PhD/Third chapter/GIS/Fallow/SG", layer = "SG17_23031")
sg18 <- readOGR("D:/PhD/Third chapter/GIS/Fallow/SG", layer = "SG18_23031")
sg19 <- readOGR("D:/PhD/Third chapter/GIS/Fallow/SG", layer = "SG19_23031")

# LOAD GREENING (DUN but only fallow fields)

gre15 <- readOGR("D:/PhD/Third chapter/GIS/Fallow/Greening", layer = "gre_15")
gre16 <- readOGR("D:/PhD/Third chapter/GIS/Fallow/Greening", layer = "gre_16")
gre17 <- readOGR("D:/PhD/Third chapter/GIS/Fallow/Greening", layer = "gre_17")
gre18 <- readOGR("D:/PhD/Third chapter/GIS/Fallow/Greening", layer = "gre_18")
gre19 <- readOGR("D:/PhD/Third chapter/GIS/Fallow/Greening", layer = "gre_19")

# LOAD Finca más de melons (Fallow fields in there are also managed)
mdm <- readOGR("D:/PhD/Third chapter/GIS", layer = "finca_mdm_EPSG23031")

# Create layer with managed fallow fields from mdm

mdm15 <- crop(gre15, mdm)
mdm16 <- crop(gre16, mdm)
mdm17 <- crop(gre17, mdm)
mdm18 <- crop(gre18, mdm)
mdm19 <- crop(gre19, mdm)

# Join with sg to get layer MANAGED

man15 <- union(mdm15, sg15)
man16 <- union(mdm16, sg16)
man17 <- union(mdm17, sg17)
man18 <- union(mdm18, sg18)
man19 <- union(mdm19, sg19)

# Save 

writeOGR(man15, dsn = "D:/PhD/Third chapter/GIS/Fallow/SG", layer = "mdm_SG_2015_EPSG23031", driver = "ESRI Shapefile")
writeOGR(man16, dsn = "D:/PhD/Third chapter/GIS/Fallow/SG", layer = "mdm_SG_2016_EPSG23031", driver = "ESRI Shapefile")
writeOGR(man17, dsn = "D:/PhD/Third chapter/GIS/Fallow/SG", layer = "mdm_SG_2017_EPSG23031", driver = "ESRI Shapefile")
writeOGR(man18, dsn = "D:/PhD/Third chapter/GIS/Fallow/SG", layer = "mdm_SG_2018_EPSG23031", driver = "ESRI Shapefile")
writeOGR(man19, dsn = "D:/PhD/Third chapter/GIS/Fallow/SG", layer = "mdm_SG_2019_EPSG23031", driver = "ESRI Shapefile")



