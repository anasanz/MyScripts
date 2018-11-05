
# ==================================================================================
#                             REMOVE IRRIGATED TRANSECTS
# ==================================================================================

library(rgdal)

# Load data

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

