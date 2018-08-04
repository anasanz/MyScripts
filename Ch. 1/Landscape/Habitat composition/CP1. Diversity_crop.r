
library(rgdal)
library(sp)
library(raster)
library(rgeos)


#######################################################
#Try to rasterize faster but it doesnt work: XYfromraster <-extract(points,buffer)
#xy<-xyFromCell(r1,cell = 1:150)
#spdf <- SpatialPoints(coords = xy,
#             proj4string = CRS("+proj=utm +zone=31 +ellps=GRS80 +units=m +no_defs"))
#d15subset<- d15
#d15subset@data <- data.frame(d15subset@data$cultiu)
#c<-extract(d15subset,spdf)
#c<-over(d15subset,spdf)
#r1[]<-c
############################################################

setwd("C:/Users/ana.sanz/Documents/GIS Ana/DUN")

#RASTERIZE FULL DUN
#2015#
d15<-readOGR("C:/Users/ana.sanz/Documents/GIS Ana/DUN", "clip_dun2015") #Land uses 2015
r1 <- raster(resolution=50, extent(d15), crs = "+proj=utm +zone=31 +ellps=GRS80 +units=m +no_defs")
ras_d15 <- rasterize(d15,r1,field="cultiu")
#writeRaster(ras_d15,filename='ras_d15',format='GTiff')
#Layerize the raster to extract the % of each land category
levels(d15@data$cultiu) #The levels are the numbers, I need to assign them to the class

#2016#
d16<-readOGR("C:/Users/ana.sanz/Documents/GIS Ana/DUN", "clip_dun2016") #Land uses 2015
r2 <- raster(resolution=50, extent(d16), crs = "+proj=utm +zone=31 +ellps=GRS80 +units=m +no_defs")
ras_d16 <- rasterize(d16,r2,field="cultiu")
#writeRaster(ras_d16,filename='ras_d16',format='GTiff')

#2017#
d17<-readOGR("C:/Users/ana.sanz/Documents/GIS Ana/DUN", "clip_dun2017") #Land uses 2015
r3 <- raster(resolution=50, extent(d17), crs = "+proj=utm +zone=31 +ellps=GRS80 +units=m +no_defs")
ras_d17 <- rasterize(d17,r3,field="cultiu")
#writeRaster(ras_d17,filename='ras_d17',format='GTiff')
##########################

#2015!!!!!!!!!!!!!!!!!!!!

#Clip buffer DUN
fin15<-readOGR("C:/Users/ana.sanz/Documents/GIS Ana/Fincas", "FINQUES_2015_SELECCION_BUENA")
cf15<-gCentroid(fin15,byid = TRUE)
b15_200<-gBuffer(cf15,byid = TRUE, width = 200)
b15_500<-gBuffer(cf15,byid = TRUE, width = 500)
c15_5<-crop(d15,b15_500,snap = "out")
#writeOGR(c15_5,"C:/Users/ana.sanz/Documents/GIS Ana/DUN",layer = "clip_b15_5",driver="ESRI Shapefile")

#Mirar usos para ver c?mo reclasificar
library(dplyr)
library(tidyr)
c15_5<-readOGR("C:/Users/ana.sanz/Documents/GIS Ana/DUN", "clip_b15_5") 
f<-as.data.frame(xtabs(~ cultiu + seca_regad,c15_5@data)) #2015, frecuencia usos
colnames(f)[1]<-"cultiu" #mismo nombre para luego unir
j<-c15_5@data[which(c15_5@data$cultiu %in% f$cultiu), c(6,10)] #
t<-j[which(!duplicated(j)),]
k<-left_join(f,t)
w<-spread(k,seca_regad,Freq)
#write.csv(w,file = "usos_2015") #Reclasificaci?n en excel Usos_buffer_dun_dg


#VARIABLE % BARBECHO
#1. Reclassify
c15_5<-readOGR("C:/Users/ana.sanz/Documents/GIS Ana/DUN", "clip_b15_5")
c15_5$fallow<-as.character(c15_5$cultiu)
c15_5$fallow[c15_5$fallow == "GUARET NO SIE/ SUP. LLIURE SE*"]<-"FALLOW"
c15_5$fallow[c15_5$fallow == "GUARET SIE/ SUP. LLIURE SEMBRA"]<-"FALLOW"
c15_5$fallow[c15_5$fallow != "FALLOW"]<-"OTHERS"
c15_5$fallow<-as.factor(c15_5$fallow) #Factor because otherwise rasterize doesnt work
#2. Rasterize
r1 <- raster(resolution=50, extent(c15_5), crs = "+proj=utm +zone=31 +ellps=GRS80 +units=m +no_defs") #Empty raster
ras15_fallow <- rasterize(c15_5,r1,c15_5$fallow) #Create raster of fallow % in buffer (LAYER 1)
#writeRaster(ras15_fallow,filename='ras15_fallow',format='GTiff',overwrite = TRUE)
#3. Layerize
fal<-layerize(ras15_fallow,classes=NULL,bylayer=TRUE,suffix='numbers')
fallow<-raster(fal,layer=1)
#writeRaster(fallow,filename='fallow_15',format='GTiff') #Layer with only % fallow in buffers 2015


#VARIABLE % CULTIVOS LE?OSOS
#1. Reclassify
c15_5<-readOGR("C:/Users/ana.sanz/Documents/GIS Ana/DUN", "clip_b15_5")
c15_5$tree<-as.character(c15_5$cultiu)
c15_5$tree[c15_5$tree == "OLIVERES"]<-"tree"
c15_5$tree[c15_5$tree == "AMETLLERS"]<-"tree"
c15_5$tree[c15_5$tree == "PERERES"]<-"tree"
c15_5$tree[c15_5$tree == "NECTARINS"]<-"tree"
c15_5$tree[c15_5$tree == "PRESSEGUERS"]<-"tree"
c15_5$tree[c15_5$tree == "POMERES"]<-"tree"
c15_5$tree[c15_5$tree == "VINYES"]<-"tree"
c15_5$tree[c15_5$tree == "FRUITERS VARIS"]<-"tree"
c15_5$tree[c15_5$tree == "PRESSEGUERS/NECTARINS"]<-"tree"
c15_5$tree[c15_5$tree == "CIRERERS"]<-"tree"
c15_5$tree[c15_5$tree == "NOGUERES"]<-"tree"
c15_5$tree[c15_5$tree == "ALBERCOQUERS"]<-"tree"
c15_5$tree[c15_5$tree == "PERERES/POMERES"]<-"tree"
c15_5$tree[c15_5$tree == "CAQUI"]<-"tree"
c15_5$tree[c15_5$tree == "KIWI"]<-"tree"
c15_5$tree[c15_5$tree == "PRUNERES"]<-"tree"
c15_5$tree[c15_5$tree == "VIVER ARBRE I ARBUST"]<-"tree"
c15_5$tree[c15_5$tree != "tree"]<-"others"
c15_5$tree<-as.factor(c15_5$tree) #Factor because otherwise rasterize doesnt work
#2. Rasterize
r1 <- raster(resolution=50, extent(c15_5), crs = "+proj=utm +zone=31 +ellps=GRS80 +units=m +no_defs") #Empty raster
ras15_tree <- rasterize(c15_5,r1,c15_5$tree) #Create raster of fallow % in buffer (LAYER 1)
#writeRaster(ras15_tree,filename='ras15_tree',format='GTiff')
#3. Layerize
tre<-layerize(ras15_tree,classes=NULL,bylayer=TRUE,suffix='numbers')
tree<-raster(tre,layer=2)
#writeRaster(tree,filename='tree_15',format='GTiff') #Layer with only % tree in buffers 2015


#VARIABLE % CULTIVOS EN REGAD?O INTENSIVO
#1. Reclassify
c15_5<-readOGR("C:/Users/ana.sanz/Documents/GIS Ana/DUN", "clip_b15_5")
c15_5$irri<-as.character(c15_5$cultiu)
c15_5$irri[c15_5$irri == "BLAT DE MORO"]<-"irri"
c15_5$irri[c15_5$irri == "ALFALS"]<-"irri"
c15_5$irri[c15_5$irri == "PERERES"]<-"irri"
c15_5$irri[c15_5$irri == "NECTARINS"]<-"irri"
c15_5$irri[c15_5$irri == "PRESSEGUERS"]<-"irri"
c15_5$irri[c15_5$irri == "POMERES"]<-"irri"
c15_5$irri[c15_5$irri == "HORTA"]<-"irri"
c15_5$irri[c15_5$irri == "FRUITERS VARIS"]<-"irri"
c15_5$irri[c15_5$irri == "PRESSEGUERS/NECTARINS"]<-"irri"
c15_5$irri[c15_5$irri == "CIRERERS"]<-"irri"
c15_5$irri[c15_5$irri == "ALBERCOQUERS"]<-"irri"
c15_5$irri[c15_5$irri == "PERERES/POMERES"]<-"irri"
c15_5$irri[c15_5$irri == "CAQUI"]<-"irri"
c15_5$irri[c15_5$irri == "KIWI"]<-"irri"
c15_5$irri[c15_5$irri == "PRUNERES"]<-"irri"
c15_5$irri[c15_5$irri != "irri"]<-"others"
c15_5$irri<-as.factor(c15_5$irri) #Factor because otherwise rasterize doesnt work
#2. Rasterize
r1 <- raster(resolution=50, extent(c15_5), crs = "+proj=utm +zone=31 +ellps=GRS80 +units=m +no_defs") #Empty raster
ras15_irri <- rasterize(c15_5,r1,c15_5$irri) #Create raster of fallow % in buffer (LAYER 1)
#writeRaster(ras15_irri,filename='ras15_irri',format='GTiff')
#3. Layerize
ir<-layerize(ras15_irri,classes=NULL,bylayer=TRUE,suffix='numbers')
irri<-raster(ir,layer=1)
writeRaster(irri,filename='irri_15',format='GTiff') #Layer with only % irri in buffers 2015

#VARIABLE CROP DIVERSITY
#1. Reclassify
c15_5<-readOGR("C:/Users/ana.sanz/Documents/GIS Ana/DUN", "clip_b15_5")
c15_5$cropdiv<-as.character(c15_5$grup)
c15_5$cropdiv[c15_5$cultiu == "AMETLLERS"]<-"AMETLLERS"
c15_5$cropdiv[c15_5$cultiu == "ALFALS"]<-"ALFALS"
c15_5$cropdiv[c15_5$cultiu == "COLZA"]<-"COLZA"
c15_5$cropdiv[c15_5$cultiu == "PESOLS"]<-"PESOLS"
c15_5$cropdiv[c15_5$cultiu == "VECA I CIVADA"]<-"CEREALS"
c15_5$cropdiv[c15_5$cropdiv == "LLEGUMINOSES"]<-"ALTRES"
c15_5$cropdiv[c15_5$cropdiv == "HORTICOLES"]<-"ALTRES"
c15_5$cropdiv[c15_5$cropdiv == "FARRATGERES"]<-"ALTRES"
c15_5$cropdiv[c15_5$cultiu == "NOGUERES"]<-"ALTRES"
c15_5$cropdiv[c15_5$cultiu == "CAMELINA"]<-"ALTRES"
c15_5$cropdiv[c15_5$cultiu == "VIVER ARBRE I ARBUST"]<-"ALTRES"
c15_5$cropdiv[c15_5$cultiu == "MILL"]<-"ALTRES"


c15_5$cropdiv<-as.factor(c15_5$cropdiv) #Factor because otherwise rasterize doesnt work
#2. Rasterize
r1 <- raster(resolution=50, extent(c15_5), crs = "+proj=utm +zone=31 +ellps=GRS80 +units=m +no_defs") #Empty raster
ras15_cropdiv <- rasterize(c15_5,r1,c15_5$cropdiv) #Create raster of cropdiv % in buffer (LAYER 1)
#writeRaster(ras15_cropdiv,filename='ras15_cropdiv',format='GTiff',overwrite = TRUE)
#3. Layerize
crop<-layerize(ras15_cropdiv,classes=NULL,bylayer=TRUE,suffix='numbers')
c1<-raster(crop,layer=1)
c2<-raster(crop,layer=2)
c3<-raster(crop,layer=3)
c4<-raster(crop,layer=4)
c5<-raster(crop,layer=5)
c6<-raster(crop,layer=6)
c7<-raster(crop,layer=7)
c8<-raster(crop,layer=8)
c9<-raster(crop,layer=9)
c10<-raster(crop,layer=10)
c11<-raster(crop,layer=11)

writeRaster(c1,filename='c1_15',format='GTiff') #Layer with only % of each land category 
writeRaster(c2,filename='c2_15',format='GTiff') #in buffers 2015
writeRaster(c3,filename='c3_15',format='GTiff')
writeRaster(c4,filename='c4_15',format='GTiff')
writeRaster(c5,filename='c5_15',format='GTiff')
writeRaster(c6,filename='c6_15',format='GTiff')
writeRaster(c7,filename='c7_15',format='GTiff')
writeRaster(c8,filename='c8_15',format='GTiff')
writeRaster(c9,filename='c9_15',format='GTiff')
writeRaster(c10,filename='c10_15',format='GTiff')
writeRaster(c11,filename='c11_15',format='GTiff')

#2016 !!!!!!!!!!!!!!!!!!!!!!

#Clip buffer DUN
fin16<-readOGR("C:/Users/ana.sanz/Documents/GIS Ana/Fincas", "FINQUES_2016_SELECCION_BUENA")
cf16<-gCentroid(fin16,byid = TRUE)
b16_200<-gBuffer(cf16,byid = TRUE, width = 200)
b16_500<-gBuffer(cf16,byid = TRUE, width = 500)
c16_5<-crop(d16,b16_500,snap = "out")
#writeOGR(c16_5,"C:/Users/ana.sanz/Documents/GIS Ana/DUN",layer = "clip_b16_5",driver="ESRI Shapefile")

#Mirar usos para ver c?mo reclasificar
c16_5<-readOGR("C:/Users/ana.sanz/Documents/GIS Ana/DUN", "clip_b16_5") 
f<-as.data.frame(xtabs(~ cultiu + seca_regad,c16_5@data)) #2015, frecuencia usos
colnames(f)[1]<-"cultiu" #mismo nombre para luego unir
j<-c16_5@data[which(c16_5@data$cultiu %in% f$cultiu), c(2,7)] #
t<-j[which(!duplicated(j)),]
k<-left_join(f,t)
w<-spread(k,seca_regad,Freq)
#write.csv(w,file = "usos_2016") #Reclasificaci?n en excel Usos_buffer_dun_dg

#VARIABLE % BARBECHO
#1. Reclassify
c16_5<-readOGR("C:/Users/ana.sanz/Documents/GIS Ana/DUN", "clip_b16_5")
c16_5$fallow<-as.character(c16_5$cultiu)
c16_5$fallow[c16_5$fallow == "GUARET NO SIE/ SUP. LLIURE SE*"]<-"FALLOW"
c16_5$fallow[c16_5$fallow == "GUARET SIE/ SUP. LLIURE SEMBRA"]<-"FALLOW"
c16_5$fallow[c16_5$fallow != "FALLOW"]<-"OTHERS"
c16_5$fallow<-as.factor(c16_5$fallow) #Factor because otherwise rasterize doesnt work
#2. Rasterize
r1 <- raster(resolution=50, extent(c16_5), crs = "+proj=utm +zone=31 +ellps=GRS80 +units=m +no_defs") #Empty raster
ras16_fallow <- rasterize(c16_5,r1,c16_5$fallow) #Create raster of fallow % in buffer (LAYER 1)
#3. Layerize
fal<-layerize(ras16_fallow,classes=NULL,bylayer=TRUE,suffix='numbers')
fallow<-raster(fal,layer=1)
writeRaster(fallow,filename='fallow_16',format='GTiff') #Layer with only % fallow in buffers 2016

#VARIABLE % CULTIVOS LE?OSOS
#1. Reclassify
c16_5<-readOGR("C:/Users/ana.sanz/Documents/GIS Ana/DUN", "clip_b16_5")
c16_5$tree<-as.character(c16_5$cultiu)
c16_5$tree[c16_5$tree == "OLIVERES"]<-"tree"
c16_5$tree[c16_5$tree == "AMETLLERS"]<-"tree"
c16_5$tree[c16_5$tree == "PERERES"]<-"tree"
c16_5$tree[c16_5$tree == "NECTARINS"]<-"tree"
c16_5$tree[c16_5$tree == "PRESSEGUERS"]<-"tree"
c16_5$tree[c16_5$tree == "POMERES"]<-"tree"
c16_5$tree[c16_5$tree == "VINYES"]<-"tree"
c16_5$tree[c16_5$tree == "ALTRES FRUITERS"]<-"tree"
c16_5$tree[c16_5$tree == "PRESSEGUERS/NECTARINS"]<-"tree"
c16_5$tree[c16_5$tree == "CIRERERS"]<-"tree"
c16_5$tree[c16_5$tree == "NOGUERES"]<-"tree"
c16_5$tree[c16_5$tree == "ALBERCOQUERS"]<-"tree"
c16_5$tree[c16_5$tree == "PERERES/POMERES"]<-"tree"
c16_5$tree[c16_5$tree == "CAQUI"]<-"tree"
c16_5$tree[c16_5$tree == "KIWI"]<-"tree"
c16_5$tree[c16_5$tree == "PRUNERES"]<-"tree"
c16_5$tree[c16_5$tree == "VIVER ARBRE I ARBUST"]<-"tree"
c16_5$tree[c16_5$tree == "AVELLANER"]<-"tree"
c16_5$tree[c16_5$tree == "FIGUERA"]<-"tree"
c16_5$tree[c16_5$tree == "MAGRANER"]<-"tree"
c16_5$tree[c16_5$tree == "PISTATXER O FESTUC"]<-"tree"

c16_5$tree[c16_5$tree != "tree"]<-"others"
c16_5$tree<-as.factor(c16_5$tree) #Factor because otherwise rasterize doesnt work
#2. Rasterize
r1 <- raster(resolution=50, extent(c16_5), crs = "+proj=utm +zone=31 +ellps=GRS80 +units=m +no_defs") #Empty raster
ras16_tree <- rasterize(c16_5,r1,c16_5$tree) #Create raster of fallow % in buffer (LAYER 1)
#writeRaster(ras16_tree,filename='ras16_tree',format='GTiff')
#3. Layerize
tre<-layerize(ras16_tree,classes=NULL,bylayer=TRUE,suffix='numbers')
tree<-raster(tre,layer=2)
#writeRaster(tree,filename='tree_16',format='GTiff') #Layer with only % tree in buffers 2016


#VARIABLE % CULTIVOS EN REGAD?O INTENSIVO
#1. Reclassify
c16_5<-readOGR("C:/Users/ana.sanz/Documents/GIS Ana/DUN", "clip_b16_5")
c16_5$irri<-as.character(c16_5$cultiu)
c16_5$irri[c16_5$irri == "BLAT DE MORO"]<-"irri"
c16_5$irri[c16_5$irri == "ALFALS"]<-"irri"
c16_5$irri[c16_5$irri == "PERERES"]<-"irri"
c16_5$irri[c16_5$irri == "NECTARINS"]<-"irri"
c16_5$irri[c16_5$irri == "PRESSEGUERS"]<-"irri"
c16_5$irri[c16_5$irri == "POMERES"]<-"irri"
c16_5$irri[c16_5$irri == "HORTA"]<-"irri"
c16_5$irri[c16_5$irri == "FRUITERS VARIS"]<-"irri"
c16_5$irri[c16_5$irri == "PRESSEGUERS/NECTARINS"]<-"irri"
c16_5$irri[c16_5$irri == "CIRERERS"]<-"irri"
c16_5$irri[c16_5$irri == "ALBERCOQUERS"]<-"irri"
c16_5$irri[c16_5$irri == "PERERES/POMERES"]<-"irri"
c16_5$irri[c16_5$irri == "CAQUI"]<-"irri"
c16_5$irri[c16_5$irri == "KIWI"]<-"irri"
c16_5$irri[c16_5$irri == "PRUNERES"]<-"irri"
c16_5$irri[c16_5$irri == "CARABASSA"]<-"irri"
c16_5$irri[c16_5$irri == "ALTRES FRUITERS"]<-"irri"
c16_5$irri[c16_5$irri == "CEBES, CAL?OTS, PORROS I ALLS"]<-"irri"
c16_5$irri[c16_5$irri == "FIGUERA"]<-"irri"
c16_5$irri[c16_5$irri == "MAGRANER"]<-"irri"
c16_5$irri[c16_5$irri == "MEL?"]<-"irri"
c16_5$irri[c16_5$irri != "irri"]<-"others"
c16_5$irri<-as.factor(c16_5$irri) #Factor because otherwise rasterize doesnt work
#2. Rasterize
r1 <- raster(resolution=50, extent(c16_5), crs = "+proj=utm +zone=31 +ellps=GRS80 +units=m +no_defs") #Empty raster
ras16_irri <- rasterize(c16_5,r1,c16_5$irri) #Create raster of fallow % in buffer (LAYER 1)
#writeRaster(ras16_irri,filename='ras16_irri',format='GTiff')
#3. Layerize
ir<-layerize(ras16_irri,classes=NULL,bylayer=TRUE,suffix='numbers')
irri<-raster(ir,layer=1)
#writeRaster(irri,filename='irri_16',format='GTiff') #Layer with only % irri in buffers 2016

#VARIABLE CROP DIVERSITY
#1. Reclassify
c16_5<-readOGR("C:/Users/ana.sanz/Documents/GIS Ana/DUN", "clip_b16_5")
c16_5$cropdiv<-as.character(c16_5$grup)
c16_5$cropdiv[c16_5$cultiu == "AMETLLERS"]<-"AMETLLERS"
c16_5$cropdiv[c16_5$cropdiv == "CEREALS D'ESTIU"]<-"BLAT DE MORO"
c16_5$cropdiv[c16_5$cultiu == "ALFALS"]<-"ALFALS"
c16_5$cropdiv[c16_5$cropdiv == "PROTEAGINOSES"]<-"PÈSOLS"
c16_5$cropdiv[c16_5$cultiu == "VE?A I CIVADA"]<-"CEREALS"
c16_5$cropdiv[c16_5$cropdiv == "LLEGUMINOSES"]<-"ALTRES"
c16_5$cropdiv[c16_5$cropdiv == "HORT?COLES"]<-"ALTRES"
c16_5$cropdiv[c16_5$cultiu == "COLZA"]<-"COLZA"
c16_5$cropdiv[c16_5$cultiu == "FESTUCA"]<-"ALTRES"
c16_5$cropdiv[c16_5$cultiu == "TREPADELLA"]<-"ALTRES"
c16_5$cropdiv[c16_5$cultiu == "NOGUERES"]<-"ALTRES"
c16_5$cropdiv[c16_5$cropdiv == "ALTRES PRODUCTES"]<-"ALTRES"
c16_5$cropdiv[c16_5$cultiu == "GIRA-SOL"]<-"ALTRES"
c16_5$cropdiv[c16_5$cultiu == "RAY-GRASS"]<-"ALTRES"
c16_5$cropdiv[c16_5$cultiu == "AVELLANER"]<-"AMETLLERS"
c16_5$cropdiv[c16_5$cultiu == "PISTATXER O FESTUC"]<-"AMETLLERS"


c16_5$cropdiv<-as.factor(c16_5$cropdiv) #Factor because otherwise rasterize doesnt work
#2. Rasterize
r1 <- raster(resolution=50, extent(c16_5), crs = "+proj=utm +zone=31 +ellps=GRS80 +units=m +no_defs") #Empty raster
ras16_cropdiv <- rasterize(c16_5,r1,c16_5$cropdiv) #Create raster of cropdiv % in buffer (LAYER 1)
#writeRaster(ras16_cropdiv,filename='ras16_cropdiv',format='GTiff',overwrite = TRUE)
#3. Layerize
crop<-layerize(ras16_cropdiv,classes=NULL,bylayer=TRUE,suffix='numbers')
c1<-raster(crop,layer=1)
c2<-raster(crop,layer=2)
c3<-raster(crop,layer=3)
c4<-raster(crop,layer=4)
c5<-raster(crop,layer=5)
c6<-raster(crop,layer=6)
c7<-raster(crop,layer=7)
c8<-raster(crop,layer=8)
c9<-raster(crop,layer=9)
c10<-raster(crop,layer=10)
c11<-raster(crop,layer=11)

#2017 !!!!!!!!!!!!!!!!!!!!!!

#Clip buffer DUN
fin17<-readOGR("C:/Users/ana.sanz/Documents/GIS Ana/Fincas", "FINQUES_2017_SELECCION_BUENA_OK")
cf17<-gCentroid(fin17,byid = TRUE)
b17_200<-gBuffer(cf17,byid = TRUE, width = 200)
b17_500<-gBuffer(cf17,byid = TRUE, width = 500)
c17_5<-crop(d17,b17_500,snap = "out")
#writeOGR(c17_5,"C:/Users/ana.sanz/Documents/GIS Ana/DUN",layer = "clip_b17_5",driver="ESRI Shapefile")

#Mirar usos para ver c?mo reclasificar
c17_5<-readOGR("C:/Users/ana.sanz/Documents/GIS Ana/DUN", "clip_b17_5") 
f<-as.data.frame(xtabs(~ cultiu + seca_regad,c17_5@data)) #2015, frecuencia usos
colnames(f)[1]<-"cultiu" #mismo nombre para luego unir
j<-c17_5@data[which(c17_5@data$cultiu %in% f$cultiu), c(4,9)] #
t<-j[which(!duplicated(j)),]
k<-left_join(f,t)
w<-spread(k,seca_regad,Freq)
write.csv(w,file = "usos_2017.csv") #Reclasificaci?n en excel Usos_buffer_dun_dg

#VARIABLE % BARBECHO
#1. Reclassify
c17_5<-readOGR("C:/Users/ana.sanz/Documents/GIS Ana/DUN", "clip_b17_5")
c17_5$fallow<-as.character(c17_5$cultiu)
c17_5$fallow[c17_5$fallow == "GUARET NO SIE/ SUP. LLIURE SE*"]<-"FALLOW"
c17_5$fallow[c17_5$fallow == "GUARET SIE/ SUP. LLIURE SEMBRA"]<-"FALLOW"
c17_5$fallow[c17_5$fallow != "FALLOW"]<-"OTHERS"
c17_5$fallow<-as.factor(c17_5$fallow) #Factor because otherwise rasterize doesnt work
#2. Rasterize
r1 <- raster(resolution=50, extent(c17_5), crs = "+proj=utm +zone=31 +ellps=GRS80 +units=m +no_defs") #Empty raster
ras17_fallow <- rasterize(c17_5,r1,c17_5$fallow) #Create raster of fallow % in buffer (LAYER 1)
#3. Layerize
fal<-layerize(ras17_fallow,classes=NULL,bylayer=TRUE,suffix='numbers')
fallow<-raster(fal,layer=1)
writeRaster(fallow,filename='fallow_17',format='GTiff') #Layer with only % fallow in buffers 2017

#VARIABLE % CULTIVOS LE?OSOS
#1. Reclassify
c17_5<-readOGR("C:/Users/ana.sanz/Documents/GIS Ana/DUN", "clip_b17_5")
c17_5$tree<-as.character(c17_5$cultiu)
c17_5$tree[c17_5$tree == "OLIVERES"]<-"tree"
c17_5$tree[c17_5$tree == "AMETLLERS"]<-"tree"
c17_5$tree[c17_5$tree == "PERERES"]<-"tree"
c17_5$tree[c17_5$tree == "NECTARINS"]<-"tree"
c17_5$tree[c17_5$tree == "PRESSEGUERS"]<-"tree"
c17_5$tree[c17_5$tree == "POMERES"]<-"tree"
c17_5$tree[c17_5$tree == "VINYES"]<-"tree"
c17_5$tree[c17_5$tree == "ALTRES FRUITERS"]<-"tree"
c17_5$tree[c17_5$tree == "PRESSEGUERS/NECTARINS"]<-"tree"
c17_5$tree[c17_5$tree == "CIRERERS"]<-"tree"
c17_5$tree[c17_5$tree == "NOGUERES"]<-"tree"
c17_5$tree[c17_5$tree == "ALBERCOQUERS"]<-"tree"
c17_5$tree[c17_5$tree == "PERERES/POMERES"]<-"tree"
c17_5$tree[c17_5$tree == "CAQUI"]<-"tree"
c17_5$tree[c17_5$tree == "PRUNERES"]<-"tree"
c17_5$tree[c17_5$tree == "VIVER ARBRE I ARBUST"]<-"tree"
c17_5$tree[c17_5$tree == "AVELLANER"]<-"tree"
c17_5$tree[c17_5$tree == "FIGUERA"]<-"tree"
c17_5$tree[c17_5$tree == "MAGRANER"]<-"tree"


c17_5$tree[c17_5$tree != "tree"]<-"others"
c17_5$tree<-as.factor(c17_5$tree) #Factor because otherwise rasterize doesnt work
#2. Rasterize
r1 <- raster(resolution=50, extent(c17_5), crs = "+proj=utm +zone=31 +ellps=GRS80 +units=m +no_defs") #Empty raster
ras17_tree <- rasterize(c17_5,r1,c17_5$tree) #Create raster of tree % in buffer (LAYER 1)
#writeRaster(ras17_tree,filename='ras17_tree',format='GTiff')
#3. Layerize
tre<-layerize(ras17_tree,classes=NULL,bylayer=TRUE,suffix='numbers')
tree<-raster(tre,layer=2)
#writeRaster(tree,filename='tree_17',format='GTiff') #Layer with only % tree in buffers 2017


#VARIABLE % CULTIVOS EN REGAD?O INTENSIVO
#1. Reclassify
c17_5<-readOGR("C:/Users/ana.sanz/Documents/GIS Ana/DUN", "clip_b17_5")
c17_5$irri<-as.character(c17_5$cultiu)
c17_5$irri[c17_5$irri == "BLAT DE MORO"]<-"irri"
c17_5$irri[c17_5$irri == "ALFALS"]<-"irri"
c17_5$irri[c17_5$irri == "PERERES"]<-"irri"
c17_5$irri[c17_5$irri == "NECTARINS"]<-"irri"
c17_5$irri[c17_5$irri == "PRESSEGUERS"]<-"irri"
c17_5$irri[c17_5$irri == "POMERES"]<-"irri"
c17_5$irri[c17_5$irri == "HORTA"]<-"irri"
c17_5$irri[c17_5$irri == "HORTICOLES"]<-"irri"
c17_5$irri[c17_5$irri == "FRUITERS VARIS"]<-"irri"
c17_5$irri[c17_5$irri == "PRESSEGUERS/NECTARINS"]<-"irri"
c17_5$irri[c17_5$irri == "CIRERERS"]<-"irri"
c17_5$irri[c17_5$irri == "ALBERCOQUERS"]<-"irri"
c17_5$irri[c17_5$irri == "PERERES/POMERES"]<-"irri"
c17_5$irri[c17_5$irri == "CAQUI"]<-"irri"
c17_5$irri[c17_5$irri == "PRUNERES"]<-"irri"
c17_5$irri[c17_5$irri == "CARABASSA"]<-"irri"
c17_5$irri[c17_5$irri == "ALTRES FRUITERS"]<-"irri"
c17_5$irri[c17_5$irri == "FIGUERA"]<-"irri"
c17_5$irri[c17_5$irri == "MAGRANER"]<-"irri"

c17_5$irri[c17_5$irri != "irri"]<-"others"
c17_5$irri<-as.factor(c17_5$irri) #Factor because otherwise rasterize doesnt work
#2. Rasterize
r1 <- raster(resolution=50, extent(c17_5), crs = "+proj=utm +zone=31 +ellps=GRS80 +units=m +no_defs") #Empty raster
ras17_irri <- rasterize(c17_5,r1,c17_5$irri) #Create raster of fallow % in buffer (LAYER 1)
#writeRaster(ras17_irri,filename='ras17_irri',format='GTiff')
#3. Layerize
ir<-layerize(ras17_irri,classes=NULL,bylayer=TRUE,suffix='numbers')
irri<-raster(ir,layer=1)
#writeRaster(irri,filename='irri_17',format='GTiff') #Layer with only % irri in buffers 2016


#VARIABLE CROP DIVERSITY
#1. Reclassify
c17_5<-readOGR("C:/Users/ana.sanz/Documents/GIS Ana/DUN", "clip_b17_5")
c17_5$cropdiv<-as.character(c17_5$grup)
c17_5$cropdiv[c17_5$cultiu == "ALFALS"]<-"ALFALS"
c17_5$cropdiv[c17_5$cultiu == "AMETLLERS"]<-"AMETLLERS"
c17_5$cropdiv[c17_5$cultiu == "AVELLANER"]<-"AMETLLERS"
c17_5$cropdiv[c17_5$cropdiv == "CEREALS D'ESTIU"]<-"BLAT DE MORO"
c17_5$cropdiv[c17_5$cropdiv == "HORTICOLES"]<-"ALTRES"
c17_5$cropdiv[c17_5$cultiu == "CAMELINA"]<-"ALTRES"
c17_5$cropdiv[c17_5$cultiu == "COLZA"]<-"COLZA"
c17_5$cropdiv[c17_5$cropdiv == "ALTRES PRODUCTES"]<-"ALTRES"
c17_5$cropdiv[c17_5$cultiu == "FESTUCA"]<-"ALTRES"
c17_5$cropdiv[c17_5$cultiu == "GIRA-SOL"]<-"ALTRES"
c17_5$cropdiv[c17_5$cultiu == "NOGUERES"]<-"ALTRES"
c17_5$cropdiv[c17_5$cultiu == "RAY-GRASS"]<-"ALTRES"
c17_5$cropdiv[c17_5$cultiu == "TREPADELLA"]<-"ALTRES"
c17_5$cropdiv[c17_5$cultiu == "PESOLS"]<-"PESOLS"
c17_5$cropdiv[c17_5$cropdiv == "LLEGUMINOSES"]<-"ALTRES"
c17_5$cropdiv[c17_5$cultiu == "VERA I CIVADA"]<-"CEREALS"


c17_5$cropdiv<-as.factor(c17_5$cropdiv) #Factor because otherwise rasterize doesnt work
#2. Rasterize
r1 <- raster(resolution=50, extent(c17_5), crs = "+proj=utm +zone=31 +ellps=GRS80 +units=m +no_defs") #Empty raster
ras17_cropdiv <- rasterize(c17_5,r1,c17_5$cropdiv) #Create raster of cropdiv % in buffer (LAYER 1)
#writeRaster(ras17_cropdiv,filename='ras17_cropdiv',format='GTiff',overwrite = TRUE)
#3. Layerize
crop<-layerize(ras17_cropdiv,classes=NULL,bylayer=TRUE,suffix='numbers')
c1<-raster(crop,layer=1)
c2<-raster(crop,layer=2)
c3<-raster(crop,layer=3)
c4<-raster(crop,layer=4)
c5<-raster(crop,layer=5)
c6<-raster(crop,layer=6)
c7<-raster(crop,layer=7)
c8<-raster(crop,layer=8)
c9<-raster(crop,layer=9)
c10<-raster(crop,layer=10)
c11<-raster(crop,layer=11)

###############################################################################################
#All variables created, now extract values from variables with the different buffer sizes

#1. RASTER STACK

setwd("C:/Users/ana.sanz/Documents/GIS Ana/DUN")

fallow15<-raster('fallow_15.tif')  
fallow16<-raster('fallow_16.tif') 
fallow17<-raster('fallow_17.tif') 

tree15<-raster('tree_15.tif')  
tree16<-raster('tree_16.tif')
tree17<-raster('tree_17.tif')

irri15<-raster('irri_15.tif')  
irri16<-raster('irri_16.tif') 
irri17<-raster('irri_17.tif')

# Stack including crop diversity rasters calculated before

stack15<-stack(fallow15, tree15, irri15,
               c1, c2, c3, c4, c5, c6, c7, 
               c8, c9, c10, c11, RAT=TRUE) # One stack per year because the extents are different
stack16<-stack(fallow16,tree16,irri16,
               c1, c2, c3, c4, c5, c6, c7, 
               c8, c9, c10, c11, RAT=TRUE)
stack17<-stack(fallow17,tree17,irri17,
               c1, c2, c3, c4, c5, c6, c7, 
               c8, c9, c10, c11, RAT=TRUE)

#2. EXTRACT
library(dplyr)
setwd("C:/Users/ana.sanz/Documents/Datos/Datos barbechos arrendados/Variables")

#2015
fin15<-as.data.frame(fin15@data[,c(colnames(fin15@data) %in% c("ID","Codi_Finca"))])
fin15$Any<-"2015"

b15_200<-as(b15_200,"SpatialPolygonsDataFrame")
comp15_200<-extract(stack15,b15_200,fun = mean,na.rm = TRUE,df = TRUE,factors = TRUE,sp = TRUE)
comp15_200<-bind_cols (fin15,as.data.frame(comp15_200))
#write.csv(comp15_200,"comp15_200.csv")

b15_500<-as(b15_500,"SpatialPolygonsDataFrame")
comp15_500<-extract(stack15,b15_500,fun = mean,na.rm = TRUE,df = TRUE,factors = TRUE,sp = TRUE)
comp15_500<-bind_cols (fin15,as.data.frame(comp15_500))
#write.csv(comp15_500,"comp15_500.csv")



#2016
fin16<-as.data.frame(fin16@data[,c(colnames(fin16@data) %in% c("ID","Codi_Finca"))])
fin16$Any<-"2016"

b16_200<-as(b16_200,"SpatialPolygonsDataFrame")
comp16_200<-extract(stack16,b16_200,fun = mean,na.rm = TRUE,df = TRUE,factors = TRUE,sp = TRUE)
comp16_200<-bind_cols (fin16,as.data.frame(comp16_200))
#write.csv(comp16_200,"comp16_200.csv")

b16_500<-as(b16_500,"SpatialPolygonsDataFrame")
comp16_500<-extract(stack16,b16_500,fun = mean,na.rm = TRUE,df = TRUE,factors = TRUE,sp = TRUE)
comp16_500<-bind_cols (fin16,as.data.frame(comp16_500))
#write.csv(comp16_500,"comp16_500.csv")



#2017
fin17<-as.data.frame(fin17@data[,c(colnames(fin17@data) %in% c("ID","Codi_Finca"))])
fin17$Any<-"2017"

b17_200<-as(b17_200,"SpatialPolygonsDataFrame")
comp17_200<-extract(stack17,b17_200,fun = mean,na.rm = TRUE,df = TRUE,factors = TRUE,sp = TRUE)
comp17_200<-bind_cols (fin17,as.data.frame(comp17_200))
#write.csv(comp17_200,"comp17_200.csv")

b17_500<-as(b17_500,"SpatialPolygonsDataFrame")
comp17_500<-extract(stack17,b17_500,fun = mean,na.rm = TRUE,df = TRUE,factors = TRUE,sp = TRUE)
comp17_500<-bind_cols (fin17,as.data.frame(comp17_500))
#write.csv(comp17_500,"comp17_500.csv")

#############################################################
#SHANNON DIVERSITY INDEX 

#2015
library(vegan)
shan_15_200<-as.data.frame(diversity(comp15_200[ ,c(8:18)],index = "shannon"))
colnames(shan_15_200)[1] <- "shan_15_200"
shan_15_500<-as.data.frame(diversity(comp15_500[ ,c(8:18)],index = "shannon"))
colnames(shan_15_500)[1] <- "shan_15_500"

crop_div_15 <- data.frame(shan_15_200,shan_15_500)
#write.csv(crop_div_15,"crop_div_15.csv")

shan_16_200<-as.data.frame(diversity(comp16_200[ ,c(8:18)],index = "shannon"))
colnames(shan_16_200)[1] <- "shan_16_200"
shan_16_500<-as.data.frame(diversity(comp16_500[ ,c(8:18)],index = "shannon"))
colnames(shan_16_500)[1] <- "shan_16_500"

crop_div_16 <- data.frame(shan_16_200,shan_16_500)
#write.csv(crop_div_16,"crop_div_16.csv")

shan_17_200<-as.data.frame(diversity(comp17_200[ ,c(7:17)],index = "shannon"))
colnames(shan_17_200)[1] <- "shan_17_200"
shan_17_500<-as.data.frame(diversity(comp17_500[ ,c(7:17)],index = "shannon"))
colnames(shan_17_500)[1] <- "shan_17_500"

crop_div_17 <- data.frame(shan_17_200,shan_17_500)
#write.csv(crop_div_17,"crop_div_17.csv")

# Join index with Codi_Finca and Any
#2015
a<-read.csv("crop_div_15.csv",sep = ",",header=TRUE,fill = TRUE)
b<-read.csv("comp15_500.csv",sep = ",",header=TRUE,fill = TRUE) #Only to get Codi_Finca

a$Codi_Finca <- b$Codi_Finca
a$Any <- b$Any
#write.csv(a,"crop_div_15.csv")

#2016
a<-read.csv("crop_div_16.csv",sep = ",",header=TRUE,fill = TRUE)
b<-read.csv("comp16_500.csv",sep = ",",header=TRUE,fill = TRUE) #Only to get Codi_Finca

a$Codi_Finca <- b$Codi_Finca
a$Any <- b$Any
#write.csv(a,"crop_div_16.csv")

#2017
a<-read.csv("crop_div_17.csv",sep = ",",header=TRUE,fill = TRUE)
b<-read.csv("comp17_500.csv",sep = ",",header=TRUE,fill = TRUE) #Only to get Codi_Finca

a$Codi_Finca <- b$fin17.data...c.colnames.fin17.data...in..c..ID....Codi_Finca....
a$Any <- b$Any
#write.csv(a,"crop_div_17.csv")





