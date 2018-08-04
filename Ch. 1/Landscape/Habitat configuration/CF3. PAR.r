

#MEAN OF THE PERIMETER AREA (PAR). Relation perimeter-area (shape) as a mean of all the shapes that fall inside the buffer

setwd("C:/Users/ana.sanz/Documents/GIS Ana/SIGPAC")
library(rgdal)
library(rgeos)
library(dplyr)

mean_shape<-function(perimeter,area){
  
  shape<-perimeter/(2*pi*(sqrt(area/pi)))
  
  return(shape)
}
s<-mean_shape(p,a)

############
#2015

fin15<-readOGR("C:/Users/ana.sanz/Documents/GIS Ana/Fincas", "FINQUES_2015_SELECCION_BUENA") ###FINCAS
fin15<-fin15[-c(183),] #Delete 183 because I don't have landscape data
cf15<-gCentroid(fin15,byid = TRUE) #Centroid fincas
b15_200<-gBuffer(cf15,byid = TRUE, width = 200)
b15_500<-gBuffer(cf15,byid = TRUE, width = 500)

s15<-readOGR("C:/Users/ana.sanz/Documents/GIS Ana/SIGPAC", "clipSIG_b15_5") ####CLIP SIGPAC2015
#Exclusion of land uses that are not relevant to remove extra shapes
v<-c("ZV","AG","CA","ZU","ED","FO","PA","IV","IM" )
s15<-s15[-which(s15@data$US %in% v),]

#200m BUFFER
h<-list()
for(i in 1:341) { 
  c<-crop(s15,b15_200[i,])# Clip sigpac with buffer of each field
  a<-gArea(c,byid = TRUE) 
  p<-gLength(c,byid = TRUE) 
  s<-mean_shape(p,a) # Perimeter-area for all polygons inside buffer
  m<-mean(s)
  h[[i]]<-m
}

PAR_2015_b200<-data.frame(Any = "2015", Codi_Finca = fin15@data$Codi_Finca, PAR = do.call(rbind,h))
write.csv(PAR_2015_b200,file = "PAR_2015_b200.csv")

#500m BUFFER
h<-list()
for(i in 1:341) { 
  c<-crop(s15,b15_500[i,])# Clip sigpac with buffer of each field
  a<-gArea(c,byid = TRUE) 
  p<-gLength(c,byid = TRUE) 
  s<-mean_shape(p,a) # Perimeter-area for all polygons inside buffer
  m<-mean(s)
  h[[i]]<-m
}

PAR_2015_b500<-data.frame(Any = "2015", Codi_Finca = fin15@data$Codi_Finca, PAR = do.call(rbind,h))
write.csv(PAR_2015_b500,file = "PAR_2015_b500.csv")

#################
#2016
fin16<-readOGR("C:/Users/ana.sanz/Documents/GIS Ana/Fincas", "FINQUES_2016_SELECCION_BUENA")
cf16<-gCentroid(fin16,byid = TRUE)
b16_200<-gBuffer(cf16,byid = TRUE, width = 200)
b16_500<-gBuffer(cf16,byid = TRUE, width = 500)

s16<-readOGR("C:/Users/ana.sanz/Documents/GIS Ana/SIGPAC", "clipSIG_b16_5")
v<-c("ZV","AG","CA","ZU","ED","FO","PA","IV","IM" )
s16<-s16[-which(s16@data$US %in% v),]

#200m BUFFER
h<-list()
for(i in 1:349) { 
  c<-crop(s16,b16_200[i,])# Clip sigpac with buffer of each field
  a<-gArea(c,byid = TRUE) 
  p<-gLength(c,byid = TRUE) 
  s<-mean_shape(p,a) # Perimeter-area for all polygons inside buffer
  m<-mean(s)
  h[[i]]<-m
}

PAR_2016_b200<-data.frame(Any = "2016", Codi_Finca = fin16@data$Codi_Finca, PAR = do.call(rbind,h))
write.csv(PAR_2016_b200,file = "PAR_2016_b200.csv")

#500m BUFFER
h<-list()
for(i in 1:349) { 
  c<-crop(s16,b16_500[i,])# Clip sigpac with buffer of each field
  a<-gArea(c,byid = TRUE) 
  p<-gLength(c,byid = TRUE) 
  s<-mean_shape(p,a) # Perimeter-area for all polygons inside buffer
  m<-mean(s)
  h[[i]]<-m
}

PAR_2016_b500<-data.frame(Any = "2016", Codi_Finca = fin16@data$Codi_Finca, PAR = do.call(rbind,h))
write.csv(PAR_2016_b500,file = "PAR_2016_b500.csv")


##################################################
#2017
fin17<-readOGR("C:/Users/ana.sanz/Documents/GIS Ana/Fincas", "FINQUES_2017_SELECCION_BUENA_OK")
cf17<-gCentroid(fin17,byid = TRUE)
b17_200<-gBuffer(cf17,byid = TRUE, width = 200)
b17_500<-gBuffer(cf17,byid = TRUE, width = 500)

s17<-readOGR("C:/Users/ana.sanz/Documents/GIS Ana/SIGPAC", "clipSIG_b17_5_")
v<-c("ZV","AG","CA","ZU","ED","FO","PA","IV","IM" )
s17<-s17[-which(s17@data$us %in% v),]

#200m BUFFER
h<-list()
for(i in 1:446) { 
  c<-crop(s17,b17_200[i,])# Clip sigpac with buffer of each field
  a<-gArea(c,byid = TRUE) 
  p<-gLength(c,byid = TRUE) 
  s<-mean_shape(p,a) # Perimeter-area for all polygons inside buffer
  m<-mean(s)
  h[[i]]<-m
}

PAR_2017_b200<-data.frame(Any = "2017", Codi_Finca = fin17@data$Codi_Finca, PAR = do.call(rbind,h))
write.csv(PAR_2017_b200,file = "PAR_2017_b200.csv")

#500m BUFFER
h<-list()
for(i in 1:446) { 
  c<-crop(s17,b17_500[i,])# Clip sigpac with buffer of each field
  a<-gArea(c,byid = TRUE) 
  p<-gLength(c,byid = TRUE) 
  s<-mean_shape(p,a) # Perimeter-area for all polygons inside buffer
  m<-mean(s)
  h[[i]]<-m
}

PAR_2017_b500<-data.frame(Any = "2017", Codi_Finca = fin17@data$Codi_Finca, PAR = do.call(rbind,h))
write.csv(PAR_2017_b500,file = "PAR_2017_b500.csv")

