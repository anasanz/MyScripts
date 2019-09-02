

# Sigpac and DUN variables

rm(list = ls())

library(raster)
library(rgdal)
library(rgeos)


#### DUN ####

#### Load clips DUN ####
dun17 <- readOGR("S:/PhD/Fourth chapter/GIS/SIGPAC_DUN", "clip_dun17_4326")
dun18 <- readOGR("S:/PhD/Fourth chapter/GIS/SIGPAC_DUN", "clip_dun18_4326")

# Crop with different MCP to see uses
mcp99 <- readOGR("S:/PhD/Fourth chapter/GPS Cataluña", "mcp99")

# 2017

dun17_mcp <- crop(dun17, extent(mcp99), snap="out")

df <- dun17_mcp@data
unique(dun17_mcp$Cultiu)

dun17_mcp$uso <- NA
dun17_mcp$uso[which(dun17_mcp$Cultiu == "ORDI" & dun17_mcp$Seca_Regad == "S")] <- "CEREAL"
dun17_mcp$uso[which(dun17_mcp$Cultiu == "ORDI" & dun17_mcp$Seca_Regad == "R")] <- "HERBACEOS DE REGADIO"

dun17_mcp$uso[which(dun17_mcp$Cultiu == "GUARET NO SIE/ SUP. LLIURE SE*")] <- "BARBECHO"
dun17_mcp$uso[which(dun17_mcp$Cultiu == "GUARET SIE/ SUP. LLIURE SEMBRA")] <- "BARBECHO"

dun17_mcp$uso[which(dun17_mcp$Cultiu == "OLIVERES" & dun17_mcp$Seca_Regad == "R")] <- "FRUTALES DE REGADIO"
dun17_mcp$uso[which(dun17_mcp$Cultiu == "OLIVERES" & dun17_mcp$Seca_Regad == "S")] <- "OLIVO"

dun17_mcp$uso[which(dun17_mcp$Cultiu == "AMETLLERS" & dun17_mcp$Seca_Regad == "R")] <- "FRUTALES DE REGADIO"
dun17_mcp$uso[which(dun17_mcp$Cultiu == "AMETLLERS" & dun17_mcp$Seca_Regad == "S")] <- "ALMENDRO"

dun17_mcp$uso[which(dun17_mcp$Cultiu == "NECTARINS")] <- "FRUTALES DE REGADIO"

dun17_mcp$uso[which(dun17_mcp$Cultiu == "CAQUI")] <- "FRUTALES DE REGADIO"

dun17_mcp$uso[which(dun17_mcp$Cultiu == "BLAT TOU" & dun17_mcp$Seca_Regad == "S")] <- "CEREAL"
dun17_mcp$uso[which(dun17_mcp$Cultiu == "BLAT TOU" & dun17_mcp$Seca_Regad == "R")] <- "HERBACEOS DE REGADIO"

dun17_mcp$uso[which(dun17_mcp$Cultiu == "CIVADA" & dun17_mcp$Seca_Regad == "S")] <- "CEREAL"
dun17_mcp$uso[which(dun17_mcp$Cultiu == "CIVADA" & dun17_mcp$Seca_Regad == "R")] <- "HERBACEOS DE REGADIO"

dun17_mcp$uso[which(dun17_mcp$Cultiu == "TRITICALE" & dun17_mcp$Seca_Regad == "S")] <- "CEREAL"

dun17_mcp$uso[which(dun17_mcp$Cultiu == "PESOLS" & dun17_mcp$Seca_Regad == "S")] <- "OTROS HERBACEOS DE SECANO"
dun17_mcp$uso[which(dun17_mcp$Cultiu == "PESOLS" & dun17_mcp$Seca_Regad == "R")] <- "HERBACEOS DE REGADIO"

dun17_mcp$uso[which(dun17_mcp$Cultiu == "FRUITERS VARIS")] <- "FRUTALES DE REGADIO"
dun17_mcp$uso[which(dun17_mcp$Cultiu == "PERERES/POMERES")] <- "FRUTALES DE REGADIO"
dun17_mcp$uso[which(dun17_mcp$Cultiu == "PRESSEGUERS/NECTARINS")] <- "FRUTALES DE REGADIO"

dun17_mcp$uso[which(dun17_mcp$Cultiu == "BLAT DUR" & dun17_mcp$Seca_Regad == "R")] <- "HERBACEOS DE REGADIO"

dun17_mcp$uso[which(dun17_mcp$Cultiu == "BLAT DE MORO")] <- "HERBACEOS DE REGADIO"
dun17_mcp$uso[which(dun17_mcp$Cultiu == "GIRA-SOL")] <- "HERBACEOS DE REGADIO"
dun17_mcp$uso[which(dun17_mcp$Cultiu == "VECES")] <- "HERBACEOS DE REGADIO"
dun17_mcp$uso[which(dun17_mcp$Cultiu == "ALFALS")] <- "HERBACEOS DE REGADIO"
dun17_mcp$uso[which(dun17_mcp$Cultiu == "FESTUCA")] <- "HERBACEOS DE REGADIO"
dun17_mcp$uso[which(dun17_mcp$Cultiu == "RAY-GRASS")] <- "HERBACEOS DE REGADIO"

dun17_mcp$uso[which(dun17_mcp$Cultiu == "PRESSEGUERS")] <- "FRUTALES DE REGADIO"
dun17_mcp$uso[which(dun17_mcp$Cultiu == "ALBERCOQUERS")] <- "FRUTALES DE REGADIO"
dun17_mcp$uso[which(dun17_mcp$Cultiu == "PERERES")] <- "FRUTALES DE REGADIO"
dun17_mcp$uso[which(dun17_mcp$Cultiu == "POMERES")] <- "FRUTALES DE REGADIO"
dun17_mcp$uso[which(dun17_mcp$Cultiu == "CIRERERS")] <- "FRUTALES DE REGADIO"
dun17_mcp$uso[which(dun17_mcp$Cultiu == "PRUNERES")] <- "FRUTALES DE REGADIO"
dun17_mcp$uso[which(dun17_mcp$Cultiu == "NOGUERES")] <- "FRUTALES DE REGADIO"
dun17_mcp$uso[which(dun17_mcp$Cultiu == "VIVER ARBRE I ARBUST")] <- "FRUTALES DE REGADIO"

dun17_mcp$uso[which(dun17_mcp$Cultiu == "HORTA")] <- "HERBACEOS DE REGADIO"

dun17_mcp$uso[which(dun17_mcp$Cultiu == "MAGRANER")] <- "FRUTALES DE REGADIO"

dun17_mcp$uso[which(dun17_mcp$Cultiu == "SORGO")] <- "HERBACEOS DE REGADIO"

dun17_mcp$uso[which(dun17_mcp$Cultiu == "COLZA" & dun17_mcp$Seca_Regad == "S")] <- "OTROS HERBACEOS DE SECANO"
dun17_mcp$uso[which(dun17_mcp$Cultiu == "COLZA" & dun17_mcp$Seca_Regad == "R")] <- "HERBACEOS DE REGADIO"

dun17_mcp$uso[which(dun17_mcp$Cultiu == "NAP I COL XINESA" & dun17_mcp$Seca_Regad == "R")] <- "HERBACEOS DE REGADIO"

dun17_mcp$uso[which(dun17_mcp$Cultiu == "FIGUERA")] <- "FRUTALES DE REGADIO"
dun17_mcp$uso[which(dun17_mcp$Cultiu == "KIWI")] <- "FRUTALES DE REGADIO"

unique(dun17_mcp$uso)
dun17_mcp[which(is.na(dun17_mcp$uso)), ] # Only safra and vinyes (very few, so dont assign to category)

writeOGR(dun17_mcp, 'S:/PhD/Fourth chapter/GIS/SIGPAC_DUN', "clip_dun17_usos_4326", driver="ESRI Shapefile")


# 2018

dun18_mcp <- crop(dun18, extent(mcp99), snap="out")

df <- dun18_mcp@data
unique(dun18_mcp$Cultiu)

dun18_mcp$uso <- NA
dun18_mcp$uso[which(dun18_mcp$Cultiu == "TRITICALE" & dun18_mcp$Seca_Regad == "S")] <- "CEREAL"
dun18_mcp$uso[which(dun18_mcp$Cultiu == "TRITICALE" & dun18_mcp$Seca_Regad == "R")] <- "HERBACEOS DE REGADIO"

dun18_mcp$uso[which(dun18_mcp$Cultiu == "GUARET NO SIE/ SUP. LLIURE SE*")] <- "BARBECHO"
dun18_mcp$uso[which(dun18_mcp$Cultiu == "GUARET SIE/ SUP. LLIURE SEMBRA")] <- "BARBECHO"

dun18_mcp$uso[which(dun18_mcp$Cultiu == "NECTARINS")] <- "FRUTALES DE REGADIO"

dun18_mcp$uso[which(dun18_mcp$Cultiu == "OLIVERES" & dun18_mcp$Seca_Regad == "R")] <- "FRUTALES DE REGADIO"
dun18_mcp$uso[which(dun18_mcp$Cultiu == "OLIVERES" & dun18_mcp$Seca_Regad == "S")] <- "OLIVO"

dun18_mcp$uso[which(dun18_mcp$Cultiu == "ORDI" & dun18_mcp$Seca_Regad == "S")] <- "CEREAL"
dun18_mcp$uso[which(dun18_mcp$Cultiu == "ORDI" & dun18_mcp$Seca_Regad == "R")] <- "HERBACEOS DE REGADIO"

dun18_mcp$uso[which(dun18_mcp$Cultiu == "VEÃƒâ€¡A I CIVADA NO SIE" & dun18_mcp$Seca_Regad == "S")] <- "OTROS HERBACEOS DE SECANO"
dun18_mcp$uso[which(dun18_mcp$Cultiu == "VEÃƒâ€¡A I CIVADA NO SIE" & dun18_mcp$Seca_Regad == "R")] <- "HERBACEOS DE REGADIO"

dun18_mcp$uso[which(dun18_mcp$Cultiu == "AMETLLERS" & dun18_mcp$Seca_Regad == "R")] <- "FRUTALES DE REGADIO"
dun18_mcp$uso[which(dun18_mcp$Cultiu == "AMETLLERS" & dun18_mcp$Seca_Regad == "S")] <- "ALMENDRO"

dun18_mcp$uso[which(dun18_mcp$Cultiu == "BLAT TOU" & dun18_mcp$Seca_Regad == "S")] <- "CEREAL"
dun18_mcp$uso[which(dun18_mcp$Cultiu == "BLAT TOU" & dun18_mcp$Seca_Regad == "R")] <- "HERBACEOS DE REGADIO"

dun18_mcp$uso[which(dun18_mcp$Cultiu == "PÃƒË†SOLS NO SIE" & dun18_mcp$Seca_Regad == "S")] <- "OTROS HERBACEOS DE SECANO"
dun18_mcp$uso[which(dun18_mcp$Cultiu == "PÃƒË†SOLS NO SIE" & dun18_mcp$Seca_Regad == "R")] <- "HERBACEOS DE REGADIO"

dun18_mcp$uso[which(dun18_mcp$Cultiu == "PERERES")] <- "FRUTALES DE REGADIO"

dun18_mcp$uso[which(dun18_mcp$Cultiu == "ALFALS SIE")] <- "HERBACEOS DE REGADIO"
dun18_mcp$uso[which(dun18_mcp$Cultiu == "ALFALS NO SIE")] <- "HERBACEOS DE REGADIO"
dun18_mcp$uso[which(dun18_mcp$Cultiu == "GIRA-SOL")] <- "HERBACEOS DE REGADIO"
dun18_mcp$uso[which(dun18_mcp$Cultiu == "BLAT DE MORO")] <- "HERBACEOS DE REGADIO"

dun18_mcp$uso[which(dun18_mcp$Cultiu == "POMERES")] <- "FRUTALES DE REGADIO"

dun18_mcp$uso[which(dun18_mcp$Cultiu == "CIVADA" & dun18_mcp$Seca_Regad == "S")] <- "CEREAL"
dun18_mcp$uso[which(dun18_mcp$Cultiu == "CIVADA" & dun18_mcp$Seca_Regad == "R")] <- "HERBACEOS DE REGADIO"

dun18_mcp$uso[which(dun18_mcp$Cultiu == "GARLANDA NO SIE")] <- "HERBACEOS DE REGADIO"
dun18_mcp$uso[which(dun18_mcp$Cultiu == "PERERES/POMERES")] <- "FRUTALES DE REGADIO"
dun18_mcp$uso[which(dun18_mcp$Cultiu == "PRESSEGUERS/NECTARINS")] <- "FRUTALES DE REGADIO"
dun18_mcp$uso[which(dun18_mcp$Cultiu == "FRUITERS VARIS")] <- "FRUTALES DE REGADIO"

dun18_mcp$uso[which(dun18_mcp$Cultiu == "VEÃƒâ€¡A I CIVADA SIE" & dun18_mcp$Seca_Regad == "S")] <- "OTROS HERBACEOS DE SECANO"
dun18_mcp$uso[which(dun18_mcp$Cultiu == "VEÃƒâ€¡A I CIVADA SIE" & dun18_mcp$Seca_Regad == "R")] <- "HERBACEOS DE REGADIO"

dun18_mcp$uso[which(dun18_mcp$Cultiu == "FESTUCA")] <- "HERBACEOS DE REGADIO"

dun18_mcp$uso[which(dun18_mcp$Cultiu == "PÃƒË†SOLS SIE" & dun18_mcp$Seca_Regad == "S")] <- "OTROS HERBACEOS DE SECANO"
dun18_mcp$uso[which(dun18_mcp$Cultiu == "PÃƒË†SOLS SIE" & dun18_mcp$Seca_Regad == "R")] <- "HERBACEOS DE REGADIO"

dun18_mcp$uso[which(dun18_mcp$Cultiu == "VECES SIE")] <- "HERBACEOS DE REGADIO"
dun18_mcp$uso[which(dun18_mcp$Cultiu == "VECES NO SIE")] <- "HERBACEOS DE REGADIO"

dun18_mcp$uso[which(dun18_mcp$Cultiu == "PRESSEGUERS")] <- "FRUTALES DE REGADIO"

dun18_mcp$uso[which(dun18_mcp$Cultiu == "COLZA" & dun18_mcp$Seca_Regad == "S")] <- "OTROS HERBACEOS DE SECANO"
dun18_mcp$uso[which(dun18_mcp$Cultiu == "COLZA" & dun18_mcp$Seca_Regad == "R")] <- "HERBACEOS DE REGADIO"

dun18_mcp$uso[which(dun18_mcp$Cultiu == "ALBERCOQUERS")] <- "FRUTALES DE REGADIO"

dun18_mcp$uso[which(dun18_mcp$Cultiu == "RAY-GRASS")] <- "HERBACEOS DE REGADIO"

dun18_mcp$uso[which(dun18_mcp$Cultiu == "PRUNERES")] <- "FRUTALES DE REGADIO"
dun18_mcp$uso[which(dun18_mcp$Cultiu == "VIVER ARBRE I ARBUST")] <- "FRUTALES DE REGADIO"

dun18_mcp$uso[which(dun18_mcp$Cultiu == "HORTA")] <- "HERBACEOS DE REGADIO"

dun18_mcp$uso[which(dun18_mcp$Cultiu == "MAGRANER")] <- "FRUTALES DE REGADIO"
dun18_mcp$uso[which(dun18_mcp$Cultiu == "NOGUERES")] <- "FRUTALES DE REGADIO"

dun18_mcp$uso[which(dun18_mcp$Cultiu == "NAP I COL XINESA" & dun18_mcp$Seca_Regad == "R")] <- "HERBACEOS DE REGADIO"

dun18_mcp$uso[which(dun18_mcp$Cultiu == "CIRERERS")] <- "FRUTALES DE REGADIO"
dun18_mcp$uso[which(dun18_mcp$Cultiu == "FIGUERA")] <- "FRUTALES DE REGADIO"
dun18_mcp$uso[which(dun18_mcp$Cultiu == "KIWI")] <- "FRUTALES DE REGADIO"
dun18_mcp$uso[which(dun18_mcp$Cultiu == "CAQUI")] <- "FRUTALES DE REGADIO"
dun18_mcp$uso[which(dun18_mcp$Cultiu == "SORGO")] <- "HERBACEOS DE REGADIO"

unique(dun18_mcp$uso)
dun18_mcp[which(is.na(dun18_mcp$uso)), ] # Only safra and vinyes (very few, so dont assign to category)

writeOGR(dun18_mcp, 'S:/PhD/Fourth chapter/GIS/SIGPAC_DUN', "clip_dun18_usos_4326", driver="ESRI Shapefile")



#### SIGPAC ####

# For variable USOS:

sigpac <- readOGR("S:/PhD/Fourth chapter/GIS/SIGPAC_DUN", "clip_sigpac_EPSG_4326")
sigpac_mcp <- crop(sigpac, extent(mcp99), snap="out")

sigpac_mcp$pastos <- NA
sigpac_mcp$pastos[which(sigpac_mcp$us == "PR" | sigpac_mcp$us == "PS" | sigpac_mcp$us == "PA")] <- "PASTOS"
sigpac_mcp$pastos[which(sigpac_mcp$us == "FO")] <- "FORESTAL"

sigpac_mcp[which(sigpac_mcp$pastos == "PASTOS"), ]

writeOGR(sigpac_mcp, 'S:/PhD/Fourth chapter/GIS/SIGPAC_DUN', "clip_sigpac_usos_4326", driver="ESRI Shapefile")

### RASTERIZE ###

# Rasterize function doesn't work. I have created one raster layer for: usos dun 17 and 18, pastos sigpac
# Make one layer for each class
# Load rasters

# 2017

dun17 <- raster("S:/PhD/Fourth chapter/GIS/SIGPAC_DUN/dun17_usos")

d17 <- layerize(dun17, classes = NULL, bylayer = TRUE, suffix = 'numbers')

dun17@data # To check order layers

setwd("S:/PhD/Fourth chapter/GIS/Capas_variables/EPSG_4326/clips/usos")

cereal <- raster(d17, layer = 1)
cereal[is.na(cereal)] <- 0 # Change NA to 0
writeRaster(cereal, filename = 'cereal_17', format = 'GTiff')

barbecho <- raster(d17, layer = 2)
barbecho[is.na(barbecho)] <- 0 # Change NA to 0
writeRaster(barbecho, filename ='barbecho_17', format = 'GTiff')

herb_secano <- raster(d17, layer = 3)
herb_secano[is.na(herb_secano)] <- 0 # Change NA to 0
writeRaster(herb_secano, filename ='herb_secano_17', format = 'GTiff')

frut_regadio <- raster(d17, layer = 4)
frut_regadio[is.na(frut_regadio)] <- 0 # Change NA to 0
writeRaster(frut_regadio, filename = 'frut_regadio_17', format='GTiff')

herb_regadio <- raster(d17, layer = 5)
herb_regadio[is.na(herb_regadio)] <- 0 # Change NA to 0
writeRaster(herb_regadio, filename = 'herb_regadio_17', format='GTiff')

olivo <- raster(d17, layer = 6)
olivo[is.na(olivo)] <- 0 # Change NA to 0
writeRaster(olivo, filename = 'olivo_17', format='GTiff')

almendro <- raster(d17, layer = 7)
almendro[is.na(almendro)] <- 0 # Change NA to 0
writeRaster(almendro, filename = 'almendro_17', format='GTiff')

# For variable FIELD SIZE

# Create raster 10*10 from field area

# Load layer with the relevant polygons (which(sigpac$us %in% c("TA", "OV", "PR", "PS", "PA", "FS", "FL")))
# I have done this layer in Arcgis and calculated the area here as well
# I have rasterized it in arcgis as well because it didnt work in r (clip_field_area_4326.shp)

# 2018

dun18 <- raster("S:/PhD/Fourth chapter/GIS/SIGPAC_DUN/dun18_usos")

d18 <- layerize(dun18, classes = NULL, bylayer = TRUE, suffix = 'numbers')

dun18@data # To check order layers

setwd("S:/PhD/Fourth chapter/GIS/Capas_variables/EPSG_4326/clips/usos")

cereal <- raster(d18, layer = 1)
cereal[is.na(cereal)] <- 0 # Change NA to 0
writeRaster(cereal, filename = 'cereal_18', format = 'GTiff')

barbecho <- raster(d18, layer = 2)
barbecho[is.na(barbecho)] <- 0 # Change NA to 0
writeRaster(barbecho, filename ='barbecho_18', format = 'GTiff')

herb_secano <- raster(d18, layer = 3)
herb_secano[is.na(herb_secano)] <- 0 # Change NA to 0
writeRaster(herb_secano, filename ='herb_secano_18', format = 'GTiff')

herb_regadio <- raster(d18, layer = 4)
herb_regadio[is.na(herb_regadio)] <- 0 # Change NA to 0
writeRaster(herb_regadio, filename = 'herb_regadio_18', format='GTiff')

frut_regadio <- raster(d18, layer = 5)
frut_regadio[is.na(frut_regadio)] <- 0 # Change NA to 0
writeRaster(frut_regadio, filename = 'frut_regadio_18', format='GTiff')

olivo <- raster(d18, layer = 6)
olivo[is.na(olivo)] <- 0 # Change NA to 0
writeRaster(olivo, filename = 'olivo_18', format='GTiff')

almendro <- raster(d18, layer = 7)
almendro[is.na(almendro)] <- 0 # Change NA to 0
writeRaster(almendro, filename = 'almendro_18', format='GTiff')

# SIGPAC

sig <- raster("S:/PhD/Fourth chapter/GIS/SIGPAC_DUN/sigpac_pastos")

s <- layerize(sig, classes = NULL, bylayer = TRUE, suffix = 'numbers')

s@data # To check order layers

setwd("S:/PhD/Fourth chapter/GIS/Capas_variables/EPSG_4326/clips/usos")

pastos <- raster(s, layer = 2)
pastos[is.na(pastos)] <- 0 # Change NA to 0
writeRaster(cereal, filename = 'pastos', format = 'GTiff')

forestal <- raster(s, layer = 3)
forestal[is.na(forestal)] <- 0 # Change NA to 0
writeRaster(forestal, filename ='forestal', format = 'GTiff')