rm(list=ls())

library(rgdal)
library(rgeos)
library(raster)
library(sf)

# Calculate GIS layers of fallow variables to REMOVE the overlap 
# 1. SG; 2. AES; 3. SIE
# From AES and Greening, the layer have been afterwards manually edited to remove the polygons that are resulting from an imperfect overlap 

# 1. ---- Load layers: The ones of SG stay in their original version ----
# I am not going to analyze the 2014 by now

# SG: sg + fincas gestionadas mas de melons

#sg14 <- readOGR("D:/PhD/Second chapter/Data/GIS/SG", layer = "SG_2014_EPSG23031")
sg15 <- readOGR("D:/PhD/Third chapter/GIS/Fallow_FIX/SG", layer = "mdm_SG_2015_EPSG23031")
sg16 <- readOGR("D:/PhD/Third chapter/GIS/Fallow_FIX/SG", layer = "mdm_SG_2016_EPSG23031")
sg17 <- readOGR("D:/PhD/Third chapter/GIS/Fallow_FIX/SG", layer = "mdm_SG_2017_EPSG23031")
sg18 <- readOGR("D:/PhD/Third chapter/GIS/Fallow_FIX/SG", layer = "mdm_SG_2018_EPSG23031")
sg19 <- readOGR("D:/PhD/Third chapter/GIS/Fallow_FIX/SG", layer = "mdm_SG_2019_EPSG23031")

# In sg18 and sg19, they rent PR and PS because there are not enough fallow fields. 
# I need to remove the fields that are General = "altres" because are pr and ps

#sg18 <- sg18[-which(sg18$General_ == "Altres"), ]
#sg19 <- sg19[-which(sg19$General_ == "Altres"), ]

# Write the layers again
#writeOGR(sg18, dsn = "D:/PhD/Second chapter/Data/GIS/SG", layer = "SG_2018_EPSG23031", driver = "ESRI Shapefile", overwrite_layer = TRUE)
#writeOGR(sg19, dsn = "D:/PhD/Second chapter/Data/GIS/SG", layer = "SG_2019_EPSG23031", driver = "ESRI Shapefile", overwrite_layer = TRUE)



# AES

#aes14 <- readOGR("D:/PhD/Second chapter/Data/GIS/AES/Only AES/EPSG23031", layer = "AES_2014_EPSG23031")
aes15 <- readOGR("D:/PhD/Second chapter/Data/GIS/AES/Only AES/EPSG23031", layer = "AES_2015_EPSG23031")
aes16 <- readOGR("D:/PhD/Second chapter/Data/GIS/AES/Only AES/EPSG23031", layer = "AES_2016_EPSG23031_FALLOW_intersect") # This layer has the buffer alraeady done (because it was giving troubles)
aes17 <- readOGR("D:/PhD/Second chapter/Data/GIS/AES/Only AES/EPSG23031", layer = "AES_2017_EPSG23031")
aes18 <- readOGR("D:/PhD/Second chapter/Data/GIS/AES/Only AES/EPSG23031", layer = "AES_2018_EPSG23031")
aes19 <- readOGR("D:/PhD/Second chapter/Data/GIS/AES/Only AES/EPSG23031", layer = "AES_2019_EPSG23031_FALLOW")

# GREEN

# gre14 <- NO HABÍA GREENING TODAVÍA (PERO PODRIA METER LOS BARBECHOS NORMALES IGUALMENTE???)
gre15 <- readOGR("D:/PhD/Third chapter/GIS/Fallow/Greening", layer = "gre_15")
gre16 <- readOGR("D:/PhD/Third chapter/GIS/Fallow/Greening", layer = "gre_16")
gre17 <- readOGR("D:/PhD/Third chapter/GIS/Fallow/Greening", layer = "gre_17")
gre18 <- readOGR("D:/PhD/Third chapter/GIS/Fallow/Greening", layer = "gre_18")
gre19 <- readOGR("D:/PhD/Third chapter/GIS/Fallow/Greening", layer = "gre_19")


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
#writeOGR(new_aes14, dsn = "D:/PhD/Third chapter/GIS/Fallow/AES", layer = "new_aes14", driver = "ESRI Shapefile")
#writeOGR(new_aes15, dsn = "D:/PhD/Third chapter/GIS/Fallow/AES", layer = "new_aes15", driver = "ESRI Shapefile")
#writeOGR(new_aes16, dsn = "D:/PhD/Third chapter/GIS/Fallow/AES", layer = "new_aes16", driver = "ESRI Shapefile")
#writeOGR(new_aes17, dsn = "D:/PhD/Third chapter/GIS/Fallow/AES", layer = "new_aes17", driver = "ESRI Shapefile")


# ---- B. Remove only the parts that overlap <- ESTO, MEJOR CORTAR PORQUE SI NO SE ELIMINAN MUCHAS FINCAS QUE SOLAPAN POR ERROR O MUY POCO ----

# Prueba

plot(sg17[706,])
sg17@data[which(sg17$ID == "13100200298-00"),]

aes17@data[which(aes17$ID_REC == "25131:0:0:2:298:1"), ]
plot(aes17[2492,])

prueba_sg <- sg17[706,]
prueba_aes <- aes17[2492,]

plot(prueba_sg)
plot(prueba_aes, add = TRUE, col = "red")

eprueba <- erase(prueba_aes, prueba_sg)
plot(eprueba)

#e14 <- erase(aes14,sg14)
e15 <- erase(aes15,sg15)
e16 <- erase(aes16,sg16)
e17 <- erase(aes17,sg17)
e18 <- erase(aes18,sg18) 
e19 <- erase(aes19,sg19) 

?erase
# Example:
pl1 <- sg14[which(sg14$ID == "01450400169-00"), ]
plot(pl1)
pl2 <- e14[which(e14$ID_REC == "25014:0:0:3:78:1"), ]
plot(pl2, col = "red", add = TRUE)

#writeOGR(e14, dsn = "D:/PhD/Third chapter/GIS/Fallow/AES", layer = "AEScutted_2014_EPSG23031", driver = "ESRI Shapefile")
writeOGR(e15, dsn = "D:/PhD/Third chapter/GIS/Fallow_FIX/AES", layer = "AEScutted_2015_EPSG23031", driver = "ESRI Shapefile")
writeOGR(e16, dsn = "D:/PhD/Third chapter/GIS/Fallow_FIX/AES", layer = "AEScutted_2016_EPSG23031", driver = "ESRI Shapefile")
writeOGR(e17, dsn = "D:/PhD/Third chapter/GIS/Fallow_FIX/AES", layer = "AEScutted_2017_EPSG23031_FIXED", driver = "ESRI Shapefile")
writeOGR(e18, dsn = "D:/PhD/Third chapter/GIS/Fallow_FIX/AES", layer = "AEScutted_2018_EPSG23031", driver = "ESRI Shapefile")
writeOGR(e19, dsn = "D:/PhD/Third chapter/GIS/Fallow_FIX/AES", layer = "AEScutted_2019_EPSG23031", driver = "ESRI Shapefile")

# 3. ---- Layer GREENING without fields that are already in SG and in AES ----

# 3.1. Fallow greening that are not SG

# g14 <- NO HAY 
g15 <- erase(gre15,sg15)
g16 <- erase(gre16,sg16)
g17 <- erase(gre17,sg17)
g18 <- erase(gre18,sg18) 
g19 <- erase(gre19,sg19) 

# Write the layer to modify the mistakes that are produced in the layer and that doesnt allow to cut it later on
writeOGR(g15, dsn = "D:/PhD/Third chapter/GIS/Fallow/Greening", layer = "GREEN_noSG_2015_EPSG23031", driver = "ESRI Shapefile")
writeOGR(g16, dsn = "D:/PhD/Third chapter/GIS/Fallow/Greening", layer = "GREEN_noSG_2016_EPSG23031", driver = "ESRI Shapefile")
writeOGR(g17, dsn = "D:/PhD/Third chapter/GIS/Fallow/Greening", layer = "GREEN_noSG_2017_EPSG23031", driver = "ESRI Shapefile")
writeOGR(g18, dsn = "D:/PhD/Third chapter/GIS/Fallow/Greening", layer = "GREEN_noSG_2018_EPSG23031", driver = "ESRI Shapefile")
writeOGR(g19, dsn = "D:/PhD/Third chapter/GIS/Fallow/Greening", layer = "GREEN_noSG_2019_EPSG23031", driver = "ESRI Shapefile")

#
#g15_2 <- readOGR("D:/PhD/Third chapter/GIS/Fallow/Greening/GREEN_cutted", layer = "GREEN_noSG_2015_EPSG23031")
#g17_2<- readOGR("D:/PhD/Third chapter/GIS/Fallow/Greening/GREEN_cutted", layer = "GREEN_noSG_2017_EPSG23031")

# 3.2. Fallow greening that are not SG and are not AES 

g15 <- readOGR("D:/PhD/Third chapter/GIS/Fallow/Greening", layer = "GREEN_noSG_2015_EPSG23031")
g16 <- readOGR("D:/PhD/Third chapter/GIS/Fallow/Greening", layer = "GREEN_noSG_2016_EPSG23031")
g17 <- readOGR("D:/PhD/Third chapter/GIS/Fallow/Greening", layer = "GREEN_noSG_2017_EPSG23031")
g18 <- readOGR("D:/PhD/Third chapter/GIS/Fallow/Greening", layer = "GREEN_noSG_2018_EPSG23031")
g19 <- readOGR("D:/PhD/Third chapter/GIS/Fallow/Greening", layer = "GREEN_noSG_2019_EPSG23031")

# g14_only <- NO HAY 
g15_only <- erase(g15,aes15)
g16_only <- erase(g16,aes16)
g17_only <- erase(g17,aes17)
g18_only <- erase(g18,aes18) # ESTO ESTABA MAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAL ponia sg18!!!!
g19_only <- erase(g19,aes19)  # ESTO ESTABA MAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAL ponia sg19!!!!

writeOGR(g15_only, dsn = "D:/PhD/Third chapter/GIS/Fallow/Greening", layer = "GREENcutted_2015_EPSG23031", driver = "ESRI Shapefile")
writeOGR(g16_only, dsn = "D:/PhD/Third chapter/GIS/Fallow/Greening", layer = "GREENcutted_2016_EPSG23031", driver = "ESRI Shapefile")
writeOGR(g17_only, dsn = "D:/PhD/Third chapter/GIS/Fallow/Greening", layer = "GREENcutted_2017_EPSG23031", driver = "ESRI Shapefile")
writeOGR(g18_only, dsn = "D:/PhD/Third chapter/GIS/Fallow_FIX/Greening", layer = "GREENcutted_2018_EPSG23031", driver = "ESRI Shapefile")
writeOGR(g19_only, dsn = "D:/PhD/Third chapter/GIS/Fallow_FIX/Greening", layer = "GREENcutted_2019_EPSG23031", driver = "ESRI Shapefile")

