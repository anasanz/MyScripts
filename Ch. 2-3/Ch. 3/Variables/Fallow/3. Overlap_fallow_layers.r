
rm(list=ls())

library(rgdal)
library(rgeos)
library(raster)
library(sf)

# Calculate GIS layers of fallow variables to REMOVE the overlap 
# 1. SG; 2. AES; 3. SIE
# From AES and Greening, the layer have been afterwards manually edited to remove the polygons that are resulting from an imperfect overlap 

# 1. ---- Load layers: The ones of SG stay in their original version ----

# SG

sg14 <- readOGR("C:/Users/ana.sanz/Documents/PhD_20_sept/Second chapter/Data/GIS/SG", layer = "SG_2014_EPSG23031")
sg15 <- readOGR("C:/Users/ana.sanz/Documents/PhD_20_sept/Second chapter/Data/GIS/SG", layer = "SG_2015_EPSG23031")
sg16 <- readOGR("C:/Users/ana.sanz/Documents/PhD_20_sept/Second chapter/Data/GIS/SG", layer = "SG_2016_EPSG23031")
sg17 <- readOGR("C:/Users/ana.sanz/Documents/PhD_20_sept/Second chapter/Data/GIS/SG", layer = "SG_2017_EPSG23031")
#sg18 <- readOGR("C:/Users/ana.sanz/Documents/PhD_20_sept/Second chapter/Data/GIS/SG", layer = "SG_2018_EPSG23031")
#sg19 <- readOGR("C:/Users/ana.sanz/Documents/PhD_20_sept/Second chapter/Data/GIS/SG", layer = "SG_2019_EPSG23031")


# AES

aes14 <- readOGR("C:/Users/ana.sanz/Documents/PhD_20_sept/Second chapter/Data/GIS/AES/Only AES/EPSG23031", layer = "AES_2014_EPSG23031")
aes15 <- readOGR("C:/Users/ana.sanz/Documents/PhD_20_sept/Second chapter/Data/GIS/AES/Only AES/EPSG23031", layer = "AES_2015_EPSG23031")
aes16 <- readOGR("C:/Users/ana.sanz/Documents/PhD_20_sept/Second chapter/Data/GIS/AES/Only AES/EPSG23031", layer = "AES_2016_EPSG23031_FALLOW_intersect") # This layer has the buffer alraeady done (because it was giving troubles)
aes17 <- readOGR("C:/Users/ana.sanz/Documents/PhD_20_sept/Second chapter/Data/GIS/AES/Only AES/EPSG23031", layer = "AES_2017_EPSG23031")
# aes18 <- FALTAN
# aes19 <- FALTAN

# GREEN

# gre14 <- NO HABÍA GREENING TODAVÍA (PERO METO LOS BARBECHOS NORMALES IGUALMENTE???)
gre15 <- readOGR("C:/Users/ana.sanz/Documents/PhD_20_sept/Third chapter/GIS/Fallow/Greening", layer = "gre_15")
gre16 <- readOGR("C:/Users/ana.sanz/Documents/PhD_20_sept/Third chapter/GIS/Fallow/Greening", layer = "gre_16")
gre17 <- readOGR("C:/Users/ana.sanz/Documents/PhD_20_sept/Third chapter/GIS/Fallow/Greening", layer = "gre_17")
#gre18 <- readOGR("C:/Users/ana.sanz/Documents/PhD_20_sept/Third chapter/GIS/Fallow/Greening", layer = "gre_18")
#gre19 <- readOGR("C:/Users/ana.sanz/Documents/PhD_20_sept/Third chapter/GIS/Fallow/Greening", layer = "gre_19")


# 2. ---- Layer AES without fields that are already SG ----

# Two options (print both type of layers to see what makes more sense): 

  #  ---- A. Determine the polygons that intersect and remove them fully <- NO, YOU REMOVE IMPORTANT POLYGONS ----
# Polygons that intersect:
int_aes14 <- intersect(aes14, sg14)
int_aes15 <- intersect(aes15, sg15)
int_aes16 <- intersect(aes16, sg16)
int_aes17 <- intersect(aes17, sg17)

# Example:
pl1 <- sg14[which(sg14$ID == "01450400169-00"), ]
plot(pl1)
pl2 <- int_aes14[which(int_aes14$ID_REC == "25014:0:0:3:78:1"), ]
plot(pl2, col = "red", add = TRUE)

# REMOVE them (remove the IDs)
new_aes14 <- aes14[-which(aes14$ID_REC %in% int_aes14$ID_REC), ]
new_aes15 <- aes15[-which(aes15$ID_REC %in% int_aes15$ID_REC), ]
new_aes16 <- aes16[-which(aes16$ID_REC %in% int_aes16$ID_REC), ]
new_aes17 <- aes17[-which(aes17$ID_REC %in% int_aes17$ID_REC), ]

# I have deleted it because I don't want to have it in the computer
#writeOGR(new_aes14, dsn = "C:/Users/ana.sanz/Documents/PhD_20_sept/Third chapter/GIS/Fallow/AES", layer = "new_aes14", driver = "ESRI Shapefile")
#writeOGR(new_aes15, dsn = "C:/Users/ana.sanz/Documents/PhD_20_sept/Third chapter/GIS/Fallow/AES", layer = "new_aes15", driver = "ESRI Shapefile")
#writeOGR(new_aes16, dsn = "C:/Users/ana.sanz/Documents/PhD_20_sept/Third chapter/GIS/Fallow/AES", layer = "new_aes16", driver = "ESRI Shapefile")
#writeOGR(new_aes17, dsn = "C:/Users/ana.sanz/Documents/PhD_20_sept/Third chapter/GIS/Fallow/AES", layer = "new_aes17", driver = "ESRI Shapefile")


  # ---- B. Remove only the parts that overlap <- ESTO, MEJOR CORTAR PORQUE SI NO SE ELIMINAN MUCHAS FINCAS QUE SOLAPAN POR ERROR O MUY POCO ----

e14 <- erase(aes14,sg14)
e15 <- erase(aes15,sg15)
e16 <- erase(aes16,sg16)
e17 <- erase(aes17,sg17)
# e18 <- erase(aes18,sg18) # Cuando tenga las AES del 18
# e19 <- erase(aes19,sg19) # Cuando tenga las AES del 19


# Example:
pl1 <- sg14[which(sg14$ID == "01450400169-00"), ]
plot(pl1)
pl2 <- e14[which(e14$ID_REC == "25014:0:0:3:78:1"), ]
plot(pl2, col = "red", add = TRUE)

writeOGR(e14, dsn = "C:/Users/ana.sanz/Documents/PhD_20_sept/Third chapter/GIS/Fallow/AES", layer = "AEScutted_2014_EPSG23031", driver = "ESRI Shapefile")
writeOGR(e15, dsn = "C:/Users/ana.sanz/Documents/PhD_20_sept/Third chapter/GIS/Fallow/AES", layer = "AEScutted_2015_EPSG23031", driver = "ESRI Shapefile")
writeOGR(e16, dsn = "C:/Users/ana.sanz/Documents/PhD_20_sept/Third chapter/GIS/Fallow/AES", layer = "AEScutted_2016_EPSG23031", driver = "ESRI Shapefile")
writeOGR(e17, dsn = "C:/Users/ana.sanz/Documents/PhD_20_sept/Third chapter/GIS/Fallow/AES", layer = "AEScutted_2017_EPSG23031", driver = "ESRI Shapefile")
#writeOGR(e18, dsn = "C:/Users/ana.sanz/Documents/PhD_20_sept/Third chapter/GIS/Fallow/AES", layer = "AEScutted_2018_EPSG23031", driver = "ESRI Shapefile")
#writeOGR(e19, dsn = "C:/Users/ana.sanz/Documents/PhD_20_sept/Third chapter/GIS/Fallow/AES", layer = "AEScutted_2019_EPSG23031", driver = "ESRI Shapefile")

# THIS IS DONE, NEXT DAY START WITH POINT 3
# 3. ---- Layer GREENING without fields that are already in SG and in AES ----

# 3.1. Fallow greening that are not SG

# g14 <- NO HAY (los meto igualmente?????)
g15 <- erase(gre15,sg15)
g16 <- erase(gre16,sg16)
g17 <- erase(gre17,sg17)
# g18 <- erase(gre18,sg18) # Cuando tenga las AES del 18
# g19 <- erase(gre19,sg19) # Cuando tenga las AES del 19

# Write the layer to modify the mistakes that are produced in the layer and that doesnt allow to cut it later on
writeOGR(g15, dsn = "C:/Users/ana.sanz/Documents/PhD_20_sept/Third chapter/GIS/Fallow/Greening", layer = "GREEN_noSG_2015_EPSG23031", driver = "ESRI Shapefile")
writeOGR(g16, dsn = "C:/Users/ana.sanz/Documents/PhD_20_sept/Third chapter/GIS/Fallow/Greening", layer = "GREEN_noSG_2016_EPSG23031", driver = "ESRI Shapefile")
writeOGR(g17, dsn = "C:/Users/ana.sanz/Documents/PhD_20_sept/Third chapter/GIS/Fallow/Greening", layer = "GREEN_noSG_2017_EPSG23031", driver = "ESRI Shapefile")
#
g15_2 <- readOGR("C:/Users/ana.sanz/Documents/PhD_20_sept/Third chapter/GIS/Fallow/Greening/GREEN_cutted", layer = "GREEN_noSG_2015_EPSG23031")
g17_2<- readOGR("C:/Users/ana.sanz/Documents/PhD_20_sept/Third chapter/GIS/Fallow/Greening/GREEN_cutted", layer = "GREEN_noSG_2017_EPSG23031")

# 3.2. Fallow greening that are not SG and are not AES 

# g14_only <- NO HAY (los meto igualmente?????)
g15_only <- erase(g15_2,aes15)
g16_only <- erase(g16,aes16)
g17_only <- erase(g17,aes17)
# g18_only <- erase(gre18,sg18) # Cuando tenga las AES del 18
# g19_only <- erase(gre19,sg19) # Cuando tenga las AES del 19


#???writeOGR(g14_only, dsn = "C:/Users/ana.sanz/Documents/PhD_20_sept/Third chapter/GIS/Fallow/Greening", layer = "GREENcutted_2014_EPSG23031", driver = "ESRI Shapefile")
writeOGR(g15_only, dsn = "C:/Users/ana.sanz/Documents/PhD_20_sept/Third chapter/GIS/Fallow/Greening", layer = "GREENcutted_2015_EPSG23031", driver = "ESRI Shapefile")
writeOGR(g16_only, dsn = "C:/Users/ana.sanz/Documents/PhD_20_sept/Third chapter/GIS/Fallow/Greening", layer = "GREENcutted_2016_EPSG23031", driver = "ESRI Shapefile")
writeOGR(g17_only, dsn = "C:/Users/ana.sanz/Documents/PhD_20_sept/Third chapter/GIS/Fallow/Greening", layer = "GREENcutted_2017_EPSG23031", driver = "ESRI Shapefile")
#writeOGR(g18_only, dsn = "C:/Users/ana.sanz/Documents/PhD_20_sept/Third chapter/GIS/Fallow/Greening", layer = "GREENcutted_2018_EPSG23031", driver = "ESRI Shapefile")
#writeOGR(g19_only, dsn = "C:/Users/ana.sanz/Documents/PhD_20_sept/Third chapter/GIS/Fallow/Greening", layer = "GREENcutted_2019_EPSG23031", driver = "ESRI Shapefile")

