
### MANAGE GPS POSITIONS TRY 1: IT DOESNT WORK, R DOES WEIRD THINGS WITH THE DATA WHEN DIVIDING IN PERIODS ####

rm(list = ls())

library(raster)
library(rgdal)


cip03 <- readOGR("S:/PhD/Fourth chapter/GPS Cataluña/Ganga/17-18_rep", "XYCIP03_20170810")
cip04 <- readOGR("S:/PhD/Fourth chapter/GPS Cataluña/Ganga/17-18_rep", "XYCIP04_20171231")

pic02 <- readOGR("S:/PhD/Fourth chapter/GPS Cataluña/Ganga/17-18_rep", "XYPIC02_20190729")
pic15 <- readOGR("S:/PhD/Fourth chapter/GPS Cataluña/Ganga/17-18_rep", "XYPIC15_20180804")
pic17 <- readOGR("S:/PhD/Fourth chapter/GPS Cataluña/Ganga/17-18_rep", "XYPIC17_20190131")



# 1. Remove flying observations

cip03_nf <- cip03[which(cip03$Speed < 1), ]
cip04_nf <- cip04[which(cip04$Speed < 1), ]

pic02_nf <- pic02[which(pic02$Speed < 1), ]
pic15_nf <- pic15[which(pic15$Speed < 1), ]
pic17_nf <- pic17[which(pic17$Speed < 1), ]

# 2. Separate in seasons

#### P1 : PRE-REPRODUCTOR & CEREAL BAJO del 2/3 (when all in breeding area) al 7/4 (cereal starts being high)  ####

# cip03
cip03_nf$Day <- as.numeric(cip03_nf$Day)
cip03_nf@data[which(cip03_nf@data$Year == 2017 & cip03_nf@data$Month == 3 & cip03_nf@data$Day == 2 ), ]
cip03_nf@data[which(cip03_nf@data$Year == 2017 & cip03_nf@data$Month == 3 & cip03_nf@data$Day == 12 ), ]

cip03_p1_1 <- cip03_nf[which(cip03_nf$Year == 2017 & cip03_nf$Month == 3 & cip03_nf$Day > 2), ] 
cip03_p1_2 <- cip03_nf[which(cip03_nf$Year == 2017 & cip03_nf$Month == 4 & cip03_nf$Day < 8), ] 

cip03_p1 <-  bind(cip03_p1_1, cip03_p1_2)

plot(cip03_p1) #???


# cip04
cip04_nf$Day <- as.integer(cip04_nf$Day)
cip04_p1_1 <- cip04_nf[which(cip04_nf$Year == 2017 & cip04_nf$Month == 3 & cip04_nf$Day > 2), ] 
cip04_p1_2 <- cip04_nf[which(cip04_nf$Year == 2017 & cip04_nf$Month == 4 & cip04_nf$Day < 8), ] 

cip04_p1 <-  bind(cip04_p1_1, cip04_p1_2)

plot(cip04_p1) #???

setwd("S:/PhD/Fourth chapter/GPS Cataluña/Ganga/17-18_rep")
writeOGR(cip03_p1, 'S:/PhD/Fourth chapter/GPS Cataluña/Ganga/17-18_rep/p1', "cip03_p1", driver="ESRI Shapefile")
writeOGR(cip04_p1, 'S:/PhD/Fourth chapter/GPS Cataluña/Ganga/17-18_rep/p1', "cip04_p1", driver="ESRI Shapefile")
  
#### P2 : PRE-REPRODUCTOR/REPRODUCTOR & CEREAL ALTO del 8/4 (cereal high) al 7/6 (siega) ####

# cip03
cip03_nf$Day <- as.integer(cip03_nf$Day)
cip03_p2_1 <- cip03_nf[which(cip03_nf$Year == 2017 & cip03_nf$Month == 4 & cip03_nf$Day > 7), ] 
cip03_p2_2 <- cip03_nf[which(cip03_nf$Year == 2017 & cip03_nf$Month == 6 & cip03_nf$Day < 8), ] 

cip03_p2 <-  bind(cip03_p2_1, cip03_p2_2)

plot(cip03_p2) 


# cip04
cip04_nf$Day <- as.integer(cip04_nf$Day)
cip04_p2_1 <- cip04_nf[which(cip04_nf$Year == 2017 & cip04_nf$Month == 4 & cip04_nf$Day > 7), ] 
cip04_p2_2 <- cip04_nf[which(cip04_nf$Year == 2017 & cip04_nf$Month == 6 & cip04_nf$Day < 8), ] 

cip04_p2 <-  bind(cip04_p2_1, cip04_p2_2)

plot(cip04_p2) 

# pic02
pic02_nf$Day <- as.integer(pic02_nf$Day)
pic02_p2_1 <- pic02_nf[which(pic02_nf$Year == 2018 & pic02_nf$Month == 4 & pic02_nf$Day > 7), ] 
pic02_p2_2 <- pic02_nf[which(pic02_nf$Year == 2018 & pic02_nf$Month == 6 & pic02_nf$Day < 8), ] 

pic02_p2 <-  bind(pic02_p2_1, pic02_p2_2)

plot(pic02_p2) 

# pic15
pic15_nf$Day <- as.integer(pic15_nf$Day)
pic15_p2_1 <- pic15_nf[which(pic15_nf$Year == 2018 & pic15_nf$Month == 4 & pic15_nf$Day > 7), ] 
pic15_p2_2 <- pic15_nf[which(pic15_nf$Year == 2018 & pic15_nf$Month == 6 & pic15_nf$Day < 8), ] 

pic15_p2 <-  bind(pic15_p2_1, pic15_p2_2)

plot(pic15_p2) 

# pic17
pic17_nf$Day <- as.integer(pic17_nf$Day)
pic17_p2_1 <- pic17_nf[which(pic17_nf$Year == 2018 & pic17_nf$Month == 4 & pic17_nf$Day > 7), ] 
pic17_p2_2 <- pic17_nf[which(pic17_nf$Year == 2018 & pic17_nf$Month == 6 & pic17_nf$Day < 8), ] 

pic17_p2 <-  bind(pic17_p2_1, pic17_p2_2)

plot(pic17_p2) 

# Save

setwd("S:/PhD/Fourth chapter/GPS Cataluña/Ganga/17-18_rep")
writeOGR(cip03_p2, 'S:/PhD/Fourth chapter/GPS Cataluña/Ganga/17-18_rep/p2', "cip03_p2", driver="ESRI Shapefile")
writeOGR(cip04_p2, 'S:/PhD/Fourth chapter/GPS Cataluña/Ganga/17-18_rep/p2', "cip04_p2", driver="ESRI Shapefile")
writeOGR(pic02_p2, 'S:/PhD/Fourth chapter/GPS Cataluña/Ganga/17-18_rep/p2', "pic02_p2", driver="ESRI Shapefile")
writeOGR(pic15_p2, 'S:/PhD/Fourth chapter/GPS Cataluña/Ganga/17-18_rep/p2', "pic15_p2", driver="ESRI Shapefile")
writeOGR(pic17_p2, 'S:/PhD/Fourth chapter/GPS Cataluña/Ganga/17-18_rep/p2', "pic17_p2", driver="ESRI Shapefile")


#### P3: REPRODUCTOR & SEGADO ####

# cip03
cip03_nf$Day <- as.integer(cip03_nf$Day)
cip03_p3_1 <- cip03_nf[which(cip03_nf$Year == 2017 & cip03_nf$Month == 6 & cip03_nf$Day > 7), ] 
cip03_p3_2 <- cip03_nf[which(cip03_nf$Year == 2017 & cip03_nf$Month %in% c(7,8)), ] 

cip03_p3 <-  bind(cip03_p3_1, cip03_p3_2)

plot(cip03_p3) 

# cip04
cip04_nf$Day <- as.integer(cip04_nf$Day)
cip04_p3_1 <- cip04_nf[which(cip04_nf$Year == 2017 & cip04_nf$Month == 6 & cip04_nf$Day > 7), ] 
cip04_p3_2 <- cip04_nf[which(cip04_nf$Year == 2017 & cip04_nf$Month %in% c(7)), ] 
cip04_p3_3 <- cip04_nf[which(cip04_nf$Year == 2017 & cip04_nf$Month == 8 & cip04_nf$Day < 12), ] 


cip04_p3 <-  bind(cip04_p3_1, cip04_p3_2, cip04_p3_3)

plot(cip04_p3) 


# pic02
pic02_nf$Day <- as.integer(pic02_nf$Day)
pic02_p3_1 <- pic02_nf[which(pic02_nf$Year == 2018 & pic02_nf$Month == 6 & pic02_nf$Day > 7), ] 
pic02_p3_2 <- pic02_nf[which(pic02_nf$Year == 2018 & pic02_nf$Month %in% c(7)), ] 
pic02_p3_3 <- pic02_nf[which(pic02_nf$Year == 2018 & pic02_nf$Month == 8 & pic02_nf$Day < 8), ] 

pic02_p3 <-  bind(pic02_p3_1, pic02_p3_2, pic02_p3_3)

plot(pic02_p3) 

df <- pic02_p3@data[which(pic02_p3@data$Latitude == 0), ] # Delete them (error GPS)
pic02_p3 <- pic02_p3[-which(pic02_p3$Latitude == 0), ]

plot(pic02_p3) 

# pic15
pic15_nf$Day <- as.integer(pic15_nf$Day)
pic15_p3_1 <- pic15_nf[which(pic15_nf$Year == 2018 & pic15_nf$Month == 6 & pic15_nf$Day > 7), ] 
pic15_p3_2 <- pic15_nf[which(pic15_nf$Year == 2018 & pic15_nf$Month %in% c(7)), ] 

pic15_p3 <-  bind(pic15_p3_1, pic15_p3_2)

plot(pic15_p3) 
df <- pic15_p3@data[which(pic15_p3@data$Latitude == 0), ] 
pic15_p3 <- pic15_p3[-which(pic15_p3$Latitude == 0), ]

plot(pic15_p3) 

# pic17
pic17_nf$Day <- as.integer(pic17_nf$Day)
pic17_p3_1 <- pic17_nf[which(pic17_nf$Year == 2018 & pic17_nf$Month == 6 & pic17_nf$Day > 7), ] 
pic17_p3_2 <- pic17_nf[which(pic17_nf$Year == 2018 & pic17_nf$Month %in% c(7,8)), ] 

pic17_p3 <-  bind(pic17_p3_1, pic17_p3_2)

plot(pic17_p3) 
pic17_p3 <- pic17_p3[-which(pic17_p3$Latitude == 0), ]
plot(pic17_p3) 

# Save

setwd("S:/PhD/Fourth chapter/GPS Cataluña/Ganga/17-18_rep")
writeOGR(cip03_p3, 'S:/PhD/Fourth chapter/GPS Cataluña/Ganga/17-18_rep/p3', "cip03_p3", driver="ESRI Shapefile")
writeOGR(cip04_p3, 'S:/PhD/Fourth chapter/GPS Cataluña/Ganga/17-18_rep/p3', "cip04_p3", driver="ESRI Shapefile")
writeOGR(pic02_p3, 'S:/PhD/Fourth chapter/GPS Cataluña/Ganga/17-18_rep/p3', "pic02_p3", driver="ESRI Shapefile")
writeOGR(pic15_p3, 'S:/PhD/Fourth chapter/GPS Cataluña/Ganga/17-18_rep/p3', "pic15_p3", driver="ESRI Shapefile")
writeOGR(pic17_p3, 'S:/PhD/Fourth chapter/GPS Cataluña/Ganga/17-18_rep/p3', "pic17_p3", driver="ESRI Shapefile")


