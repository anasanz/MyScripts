
#TOTAL BORDER LENGTH: Length of borders that fall inside the buffer, removing non relevant land uses

library(rgdal)
library(sp)
library(raster)
library(rgeos)

setwd("C:/Users/ana.sanz/Documents/GIS Ana/SIGPAC")

#1. Get smallest clip of SIGPAC possible
#2015
s15<-readOGR("C:/Users/ana.sanz/Documents/GIS Ana/Capes GIS/SIGPAC_2015", "SIGPAC_secans_2015_v2") 

fin15<-readOGR("C:/Users/ana.sanz/Documents/GIS Ana/Fincas", "FINQUES_2015_SELECCION_BUENA")
fin15<-fin15[-c(183),] #Delete 183 because I don't have landscape data
cf15<-gCentroid(fin15,byid = TRUE) #Centroid fincas

b15_200<-gBuffer(cf15,byid = TRUE, width = 200)
b15_500<-gBuffer(cf15,byid = TRUE, width = 500)

c15_5<-crop(s15,b15_500,snap = "out") #This will be used as the base SIGPAC LAYER layer
#writeOGR(c15_5,"C:/Users/ana.sanz/Documents/GIS Ana/DUN",layer = "clipSIG_b15_5",driver="ESRI Shapefile")

#2016
s16<-readOGR("C:/Users/ana.sanz/Documents/GIS Ana/Capes GIS/SIGPAC_2016/ETRS", "SIGPAC_Secans")

fin16<-readOGR("C:/Users/ana.sanz/Documents/GIS Ana/Fincas", "FINQUES_2016_SELECCION_BUENA")
cf16<-gCentroid(fin16,byid = TRUE)

b16_200<-gBuffer(cf16,byid = TRUE, width = 200)
b16_500<-gBuffer(cf16,byid = TRUE, width = 500)
b16_500<-as(b16_500,"SpatialPolygonsDataFrame")
writeOGR(b16_500,"C:/Users/ana.sanz/Documents/GIS Ana/Fincas",layer = "buffer16_500",driver="ESRI Shapefile")

c16_5<-crop(s16,b16_500,snap = "out")
#writeOGR(c16_5,"C:/Users/ana.sanz/Documents/GIS Ana/DUN",layer = "clipSIG_b16_5",driver="ESRI Shapefile")

#2017
s17<-readOGR("C:/Users/ana.sanz/Documents/GIS Ana/Capes GIS/SIGPAC_2017", "SIGPAC17_fullp")

fin17<-readOGR("C:/Users/ana.sanz/Documents/GIS Ana/Fincas", "FINQUES_2017_SELECCION_BUENA_OK")
cf17<-gCentroid(fin17,byid = TRUE)

b17_200<-gBuffer(cf17,byid = TRUE, width = 200)
b17_500<-gBuffer(cf17,byid = TRUE, width = 500)
b17_500<-as(b17_500,"SpatialPolygonsDataFrame")
#writeOGR(b17_500,"C:/Users/ana.sanz/Documents/GIS Ana/Fincas",layer = "buffer17_500",driver="ESRI Shapefile")

c17_5<-crop(s17,b17_500,snap = "out")
#writeOGR(c17_5,"C:/Users/ana.sanz/Documents/GIS Ana/DUN",layer = "clipSIG_b17_5",driver="ESRI Shapefile")

#2. Calculate line length for each buffer and year

#Length buffer
l<-as(b15_200,"SpatialLines") 
length_buffer<-LineLength(l[3,]@lines[[1]]@Lines[[1]]@coords,longlat = FALSE,sum=TRUE) #1251.476 (Maybe I rest it)

############
#2015

s15<-readOGR("C:/Users/ana.sanz/Documents/GIS Ana/SIGPAC", "clipSIG_b15_5") #CLIP SIGPAC2015
#Exclusion of land uses that are not relevant to remove extra borders
v<-c("ZV","AG","CA","ZU","ED","FO","PA","IV","IM" )
s15<-s15[-which(s15@data$US %in% v),]

#200m BUFFER

h<-list()
L<-list()
for(i in 1:341) { 
  c<-crop(s15,b15_200[i,]) # Clip sigpac with buffer of each field
  lin<-as(c,"SpatialLines")
  for (j in 1:length(lin)) { 
  len<-LineLength(lin@lines[[j]]@Lines[[1]]@coords,longlat = FALSE ,sum = TRUE) #Length of each segment of the buffer
  L[j]<-len 
  }
  s<-do.call(sum,L) #Sum all segments. 
            #The lines of the buffer are also taken into account but in all the fields is the same, so not important
  h[[i]]<-s
}

TBL_2015_b200<-data.frame(Any = "2015", Codi_Finca = fin15@data$Codi_Finca, TBL = do.call(rbind,h))
write.csv(TBL_2015_b200,file = "TBL_2015_b200.csv")

#500m BUFFER

h<-list()
for(i in 1:341) { 
  c<-crop(s15,b15_500[i,]) # Clip sigpac with buffer of each field
  lin<-as(c,"SpatialLines")
  for (j in 1:length(lin)) { 
    len<-LineLength(lin@lines[[j]]@Lines[[1]]@coords,longlat = FALSE ,sum = TRUE) #Length of each segment of the buffer
    L[j]<-len 
  }
  s<-do.call(sum,L) #Sum all segments. 
  #The lines of the buffer are also taken into account but in all the fields is the same, so not important
  h[[i]]<-s
}

TBL_2015_b500<-data.frame(Any = "2015", Codi_Finca = fin15@data$Codi_Finca, TBL = do.call(rbind,h))
write.csv(TBL_2015_b500,file = "TBL_2015_b500.csv")

#######################???

#2016

s16<-readOGR("C:/Users/ana.sanz/Documents/GIS Ana/SIGPAC", "clipSIG_b16_5")
v<-c("ZV","AG","CA","ZU","ED","FO","PA","IV","IM" )
s16<-s16[-which(s16@data$US %in% v),]

#200m BUFFER
h<-list()
for(i in 1:349) { 
  c<-crop(s16,b16_200[i,]) # Clip sigpac with buffer of each field
  lin<-as(c,"SpatialLines")
  for (j in 1:length(lin)) { 
    len<-LineLength(lin@lines[[j]]@Lines[[1]]@coords,longlat = FALSE ,sum = TRUE) #Length of each segment of the buffer
    L[j]<-len 
  }
  s<-do.call(sum,L) #Sum all segments. 
  #The lines of the buffer are also taken into account but in all the fields is the same, so not important
  h[[i]]<-s
}

TBL_2016_b200<-data.frame(Any = "2016", Codi_Finca = fin16@data$Codi_Finca, TBL = do.call(rbind,h))
write.csv(TBL_2016_b200,file = "TBL_2016_b200.csv")


#500m BUFFER
s16<-readOGR("C:/Users/ana.sanz/Documents/GIS Ana/SIGPAC", "clipSIG_b16_5")

h<-list()
for(i in 1:349) { 
  c<-crop(s16,b16_500[i,]) # Clip sigpac with buffer of each field
  lin<-as(c,"SpatialLines")
  for (j in 1:length(lin)) { 
    len<-LineLength(lin@lines[[j]]@Lines[[1]]@coords,longlat = FALSE ,sum = TRUE) #Length of each segment of the buffer
    L[j]<-len 
  }
  s<-do.call(sum,L) #Sum all segments. 
  #The lines of the buffer are also taken into account but in all the fields is the same, so not important
  h[[i]]<-s
}

TBL_2016_b500<-data.frame(Any = "2016", Codi_Finca = fin16@data$Codi_Finca, TBL = do.call(rbind,h))
write.csv(TBL_2016_b500,file = "TBL_2016_5200.csv")


#2017

s17<-readOGR("C:/Users/ana.sanz/Documents/GIS Ana/SIGPAC", "clipSIG_b17_5_")
v<-c("ZV","AG","CA","ZU","ED","FO","PA","IV","IM" )
s17<-s17[-which(s17@data$us %in% v),]

#200m BUFFER
h<-list()
L<-list()
for(i in 1:446) { 
  c<-crop(s17,b17_200[i,]) # Clip sigpac with buffer of each field
  lin<-as(c,"SpatialLines")
  for (j in 1:length(lin)) { 
    len<-LineLength(lin@lines[[j]]@Lines[[1]]@coords,longlat = FALSE ,sum = TRUE) #Length of each segment of the buffer
    L[j]<-len 
  }
  s<-do.call(sum,L) #Sum all segments. 
  #The lines of the buffer are also taken into account but in all the fields is the same, so not important
  h[[i]]<-s
}

TBL_2017_b200<-data.frame(Any = "2017", Codi_Finca = fin17@data$Codi_Finca, TBL = do.call(rbind,h))
write.csv(TBL_2017_b200,file = "TBL_2017_b200.csv")


#500m BUFFER
s17<-readOGR("C:/Users/ana.sanz/Documents/GIS Ana/SIGPAC", "clipSIG_b17_5")

h<-list()
for(i in 1:446) { 
  c<-crop(s17,b17_500[i,]) # Clip sigpac with buffer of each field
  lin<-as(c,"SpatialLines")
  for (j in 1:length(lin)) { 
    len<-LineLength(lin@lines[[j]]@Lines[[1]]@coords,longlat = FALSE ,sum = TRUE) #Length of each segment of the buffer
    L[j]<-len 
  }
  s<-do.call(sum,L) #Sum all segments. 
  #The lines of the buffer are also taken into account but in all the fields is the same, so not important
  h[[i]]<-s
}

TBL_2017_b500<-data.frame(Any = "2017", Codi_Finca = fin17@data$Codi_Finca, TBL = do.call(rbind,h))
write.csv(TBL_2017_b500,file = "TBL_2017_5200.csv")
