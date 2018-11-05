
# ==================================================================================
#                       REMOVE IRRIGATED TRANSECTS FROM LAYER
# ==================================================================================

library(rgdal)

# ---- Load data ----

#Transects
tr <- readOGR("C:/Users/Ana/Documents/PhD/Second chapter/Farmdindis/Maps/transectes", "Trans_2017") # Contains transects sampled each year (1/0)
tr@data$FETS2017 <- as.numeric(tr@data$FETS2017)
tr@data$FETS2017[which(is.na(tr@data$FETS2017))] <- 0

# Irrigated transects
setwd("C:/Users/Ana/Documents/PhD/Second chapter/Data")
irri <- read.csv("TransecteAnyReg.csv", sep = ";")
colnames(irri)[2] <- "Num_transecte"
colnames(irri)[1] <- "Region.Label"

# CREATE TRANSECT ID VARIABLE
# Add a 0 before the transect number
for (i in 1:nrow(irri)){ 
  irri$Num_transecte[i] <- paste(0,irri$Num_transecte[i], sep = "")}
# Keep only the last 2 digits 
library(stringr)
for (i in 1:nrow(irri)){ 
  irri$Num_transecte[i] <- str_sub(irri$Num_transecte[i], start = -2)}
# Create variable by pasting it
for (i in 1:nrow(irri)){ 
  irri$transectID[i] <- paste(irri$Region.Label[i],irri$Num_transecte[i], sep = "")}


# ---- Remove irrigated all years ----
irri_all <- irri$transectID[which(irri$Regadio == 1)]
tr <- tr[-which(tr@data$Codi %in% irri_all), ]

# ---- Remove the ones irrigated the year it changed -> BY HAND ----

irri_change <- irri[which(!is.na(irri$X1er.año.cambio)), ]
irri_change_ID <- irri_change$transectID
irri_change_year <- irri_change$X1er.año.cambio

tr@data[which(tr@data$Codi %in% irri_change_ID[1]), c(12,13,14) ] <- 0 
tr@data[which(tr@data$Codi %in% irri_change_ID[2]), c(10:14) ] <- 0
tr@data[which(tr@data$Codi %in% irri_change_ID[3]), c(8:14) ] <- 0
tr@data[which(tr@data$Codi %in% irri_change_ID[4]), c(11:14) ] <- 0
tr@data[which(tr@data$Codi %in% irri_change_ID[5]), c(8:14) ] <- 0
tr@data[which(tr@data$Codi %in% irri_change_ID[6]), c(10:14) ] <- 0
tr@data[which(tr@data$Codi %in% irri_change_ID[7]), c(9:14) ] <- 0
tr@data[which(tr@data$Codi %in% irri_change_ID[8]), c(13,14) ] <- 0 
tr@data[which(tr@data$Codi %in% irri_change_ID[9]), c(13,14) ] <- 0 
# tr@data[which(tr@data$Codi %in% irri_change_ID[10]), ] <- 0 # Change when 2018 comes!
tr@data[which(tr@data$Codi %in% irri_change_ID[11]), c(12,13,14) ] <- 0 
tr@data[which(tr@data$Codi %in% irri_change_ID[12]), c(12,13,14) ] <- 0 

# Save layer

writeOGR(tr,
         dsn = "C:/Users/Ana/Documents/PhD/Second chapter/Farmdindis/Maps/transectes", 
         layer = "Transects_2010_2017", driver = "ESRI Shapefile")


