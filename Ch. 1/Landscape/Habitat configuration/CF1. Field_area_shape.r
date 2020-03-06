


library(rgdal)
library(rgeos)
library(dplyr)

#Area of the study area
sa<- readOGR("D:/PhD/GIS Ana_14_Mayo/Fincas", "Study_area")
proj4string(sa)
a<-gArea(sa,byid = TRUE) #Area = 3580111544 m2 = 3580.111544 km2 = 358011.1544 ha

###############  2014  ##########################
##################################################

fin<- readOGR("C:/Users/ana.sanz/Documents/GIS Ana/Fincas", "FINQUES_FINAL_2014_ETRS89_BUENA")
a<-gArea(fin,byid = TRUE) #Area
p<-gLength(fin,byid = TRUE) #Perimetro
simple_shape<-p/a

#Calculate field shape index with formula donald

mean_shape<-function(perimeter,area){
  
  shape<-perimeter/(2*pi*(sqrt(area/pi)))
  
  return(shape)
}
s<-mean_shape(p,a)

#Join attributes
fin@data<-fin@data[,c(1,4,10,25,26,27,30)]
fin@data$area<-a
fin@data$perimeter<-p
fin@data$shape<-s #Shape: Cuánto de más perimetro tiene la finca si se compara con 
                  #una finca circular de la misma area. El mínimo de ratio es 1 porque el 
                  #círculo es la figura más regular. A partir de 1 si s = 1.26, quiere
                  #♠decir que una finca tiene 0.26 veces más perímetro que un círculo de la
                  #misma area
fin@data$simple_shape<-simple_shape #Only p/a
fin_2014<-fin
fin_2014@data<-fin_2014@data[,c(2,7,8,10)]
fin_2014@data$Any<-2014
colnames(fin_2014@data)[2]<-"Codi_Finca"


###############  2015  ##########################
##################################################

fin<- readOGR("C:/Users/ana.sanz/Documents/GIS Ana/Fincas", "FINQUES_2015_SELECCION_BUENA")

a<-gArea(fin,byid = TRUE) 
p<-gLength(fin,byid = TRUE) 
simple_shape<-p/a
s<-mean_shape(p,a)

#Join attributes
fin@data<-fin@data[,c(3,17,23,26)]
fin@data$area<-a
fin@data$perimeter<-p
fin@data$shape<-s #Shape: Cuánto de más perimetro tiene la finca si se compara con 
                  #una finca circular de la misma area
fin@data$simple_shape<-simple_shape #Only p/
fin_2015<-fin

fin_2015@data<-fin_2015@data[,c(1,3,5,7)]
fin_2015@data$Any<-2015

###############  2016  ##########################
##################################################

fin<- readOGR("C:/Users/ana.sanz/Documents/GIS Ana/Fincas", "FINQUES_2016_SELECCION_BUENA")

a<-gArea(fin,byid = TRUE) 
p<-gLength(fin,byid = TRUE) 
simple_shape<-p/a
s<-mean_shape(p,a)

#Join attributes
fin@data<-fin@data[,c(1,7,9,20)]
fin@data$area<-a
fin@data$perimeter<-p
fin@data$shape<-s #Shape: Cuánto de más perimetro tiene la finca si se compara con 
                  #una finca circular de la misma area
fin@data$simple_shape<-simple_shape #Only p/A
fin_2016<-fin

fin_2016@data<-fin_2016@data[,c(1,3,5,7)]
fin_2016@data$Any<-2016

###############  2017  ##########################
##################################################

fin<- readOGR("C:/Users/ana.sanz/Documents/GIS Ana/Finques_ASG_2017 SELECCIÓN", "FINQUES_2017_SELECCION_BUENA_OK")

a<-gArea(fin,byid = TRUE) 
p<-gLength(fin,byid = TRUE) 
simple_shape<-p/a
s<-mean_shape(p,a)

#Join attributes
fin@data<-fin@data[,c(1,2,8,9,12)]
fin@data$area<-a
fin@data$perimeter<-p
fin@data$shape<-s #Shape: Cuánto de más perimetro tiene la finca si se compara con 
#una finca circular de la misma area
fin@data$simple_shape<-simple_shape #Only p/A
fin_2017<-fin

fin_2017@data<-fin_2017@data[,c(2,5,6,8)]
fin_2017@data$Any<-2017
colnames(fin_2017@data)[1]<- "ID"

#####################ALL TOGETHER######################
f<-bind_rows(fin_2014@data,fin_2015@data,fin_2016@data,fin_2017@data)

setwd("C:/Users/ana.sanz/Documents/Datos/Datos barbechos arrendados")
#write.csv(f,"Allfincas_area_shape.csv")
