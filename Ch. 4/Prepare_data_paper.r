

## SUMMARY POSITIONS ##

rm(list = ls())

library(raster)
library(rgdal)
library(ggplot2)

#  ---- Load updated csv (24/02/2020) and join ----

setwd("D:/PhD/Fourth chapter/GPS Cataluña/Ganga/Updated_24-2-2020")

pic17 <- read.csv("D:/PhD/Fourth chapter/GPS Cataluña/Ganga/Updated_24-2-2020/PIC17_20190131.csv", sep = ";")
pic15 <- read.csv("D:/PhD/Fourth chapter/GPS Cataluña/Ganga/Updated_24-2-2020/PIC15_20180804.csv", sep = ";")
pic02 <- read.csv("D:/PhD/Fourth chapter/GPS Cataluña/Ganga/Updated_24-2-2020/PIC02_20190805.csv", sep = ";")
gue05 <- read.csv("D:/PhD/Fourth chapter/GPS Cataluña/Ganga/Updated_24-2-2020/GUE05_20190804.csv", sep = ";")
gue04 <- read.csv("D:/PhD/Fourth chapter/GPS Cataluña/Ganga/Updated_24-2-2020/GUE04_20190609.csv", sep = ";")
gue03 <- read.csv("D:/PhD/Fourth chapter/GPS Cataluña/Ganga/Updated_24-2-2020/GUE03_20190825.csv", sep = ";")
gue02 <- read.csv("D:/PhD/Fourth chapter/GPS Cataluña/Ganga/Updated_24-2-2020/GUE02_20190421.csv", sep = ";")
gue01 <- read.csv("D:/PhD/Fourth chapter/GPS Cataluña/Ganga/Updated_24-2-2020/GUE01_20190825.csv", sep = ";")
cip05 <- read.csv("D:/PhD/Fourth chapter/GPS Cataluña/Ganga/Updated_24-2-2020/CIP05_20170118.csv", sep = ";")
cip04 <- read.csv("D:/PhD/Fourth chapter/GPS Cataluña/Ganga/Updated_24-2-2020/CIP04_20171231.csv", sep = ";")
cip03 <- read.csv("D:/PhD/Fourth chapter/GPS Cataluña/Ganga/Updated_24-2-2020/CIP03_20170810.csv", sep = ";")
cip02 <- read.csv("D:/PhD/Fourth chapter/GPS Cataluña/Ganga/Updated_24-2-2020/CIP02 20170214.csv", sep = ";")

# Select column names
id <- list(pic17, pic15, pic02, gue05, gue04, gue03, gue02, gue01, cip05, cip04, cip03, cip02)
for (i in 1:length(id)){
  id[[i]] <- id[[i]][ ,which(colnames(id[[i]]) %in% c("Logger.ID", "Year", "Month", "Day", "Hour", "Minute", "Second", "Latitude", "Longitude", "Speed", "Raw.latitude", "Raw.Longitude" ))]
}

all <- do.call(rbind.data.frame, id)

#  ---- Clean data set ----

all <- all[-which(is.na(all$Latitude)), ] # Remove gps errors 

all <- all[-which(all$Speed > 1.5), ] # Remove flying positions (speed > 1.5 knots)

all <- all[which(all$Longitude > 0.59), ] # Select positions from Cataluña

# ---- Incubation ----

# Detect which id and periods where in incubation 
# ID that have attempted to reproduce: cip03, pic02, pic15, pic17

all$incu <- 0
all$Day <- as.integer(all$Day)

# cip03: 19/5/18 a 7/6/18...???? ASK
all$incu[all$Logger.ID == "CIP03" & all$Year == 2018 & all$Month == 5 & all$Day > 18] <- 1
all$incu[all$Logger.ID == "CIP03" & all$Year == 2018 & all$Month == 6 & all$Day < 8] <- 1

# pic02: 8/6/18 a 26/6/18 & 23/7/18 a 01/08/18
all$incu[all$Logger.ID == "PIC02" & all$Year == 2018 & all$Month == 6 & all$Day > 7 & all$Day < 27] <- 1

all$incu[all$Logger.ID == "PIC02" & all$Year == 2018 & all$Month == 7 & all$Day > 22] <- 1
all$incu[all$Logger.ID == "PIC02" & all$Year == 2018 & all$Month == 8 & all$Day < 2] <- 1

# pic15: 17/6/18 a 22/6/18 & 4/7/18 a 11/7/18
all$incu[all$Logger.ID == "PIC15" & all$Year == 2018 & all$Month == 6 & all$Day > 16 & all$Day < 23] <- 1
all$incu[all$Logger.ID == "PIC15" & all$Year == 2018 & all$Month == 7 & all$Day > 3 & all$Day < 12] <- 1

# pic17: 11/7/18 a 31/7/18
all$incu[all$Logger.ID == "PIC17" & all$Year == 2018 & all$Month == 7 & all$Day > 10] <- 1 

# From the period of incubation, remove the 2 first and 2 last hours
all$rem <- 0 

for ( i in 1:nrow(all)){
  if (all$incu[i] == 1){
    ifelse(all$Hour[i] == 5 | all$Hour[i] == 6 | all$Hour[i] == 18 | all$Hour[i] == 19, all$rem[i] <- 1, all$rem[i] <- 0)
  }
}

all <- all[-which(all$rem == 1), ]

# Remove positions from the first day of capture
all <- all[-which(all$Logger.ID == "PIC17" & all$Month == 4 & all$Day == 17 & all$Year == 2018), ] # 17/04/18 

all <- all[-which(all$Logger.ID == "PIC15" & all$Month == 4 & all$Day == 15 & all$Year == 2018), ] # 16/04/2018 a las 1:34 
all <- all[-which(all$Logger.ID == "PIC15" & all$Month == 4 & all$Day == 16 & all$Year == 2018), ] 

all <- all[-which(all$Logger.ID == "PIC02" & all$Month == 4 & all$Day == 16 & all$Year == 2018), ] # 16/04/2018 a las 1:34 (quitar dia 15)

all <- all[-which(all$Logger.ID == "GUE05" & all$Month == 3 & all$Day == 6 & all$Year == 2019), ] 
all <- all[-which(all$Logger.ID == "GUE04" & all$Month == 3 & all$Day == 7 & all$Year == 2019), ] 
all <- all[-which(all$Logger.ID == "GUE03" & all$Month == 3 & all$Day == 5 & all$Year == 2019), ] 
all <- all[-which(all$Logger.ID == "GUE02" & all$Month == 3 & all$Day == 6 & all$Year == 2019), ] 
all <- all[-which(all$Logger.ID == "GUE01" & all$Month == 3 & all$Day == 4 & all$Year == 2019), ] 


all <- all[-which(all$Logger.ID == "CIP05" & all$Month == 11 & all$Day == 29 & all$Year == 2016), ] 
#○all <- all[-which(all$Logger.ID == "CIP04" & all$Month == 9 & all$Day == 28 & all$Year == 2016), ] # There is none this day
#all <- all[-which(all$Logger.ID == "CIP03" & all$Month == 9 & all$Day == 30 & all$Year == 2016), ] # 
all <- all[-which(all$Logger.ID == "CIP02" & all$Month == 11 & all$Day == 30 & all$Year == 2016), ] 

all[which(all$Logger.ID == "GUE01" & all$Month == 3 & all$Day == 4 & all$Year == 2019), ] 
# Save (de aquí faltaría sólo quitar los puntos en regadío, manualmente en la capa)

setwd("D:/PhD/Fourth chapter/GPS Cataluña/Ganga/Updated_24-2-2020")
write.csv(all, "gps_positions.csv")



