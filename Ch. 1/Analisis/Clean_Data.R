

library(dplyr)
library(tidyr)

rm(list=ls())

# Data for submission and cleaned
setwd("C:/Users/ana.sanz/Documents/First chapter/Datos/Datos barbechos arrendados/Variables")
#setwd("~/PhD/Datos/Datos barbechos arrendados/Variables")

f <- read.csv("Variables.csv", sep = ",", header=TRUE, fill = TRUE)
colnames(f)[6] <- "EspecieObj"
f[f == 99.00] <- NA
f <- f[-which(duplicated(f[ , 2:15])), ]
f <- f[which(complete.cases(f$Contatge)), ]



f <- f[ , which(colnames(f) %in% c("CF_A", "EspecieObj", "Contatge", "Recob_plotViu", "Recob_plotMort","lev_ind", "Simpson", # All variables
                                   "PromigAltura1Plot", "biom", "SAI_sd","LAI_sd", "TBL_500", "PAR_500","Fallow_500", 
                                   "shan_500", "Zone", "Tractament", "Any", "Codi_Finca", "area"))]

f <- na.omit(f)
f <- f[-which(f$Tractament == "Pastoreada"), ] # Remove grazing because is only present in 2017
f <- f[-which(duplicated(f[ , 3:9])), ] # Remove duplicates

length(which(f$EspecieObj == "TERAX_m")) #619 FF per species. f is the raw data in which all analyses are based
f$Tractament[which(f$Tractament == "Curronar")] <- "Llaurar" 

f$Contatge[f$Contatge > 1] <- 1 # Binomial response

#write.csv(f, "Data_path_manuscript.csv") #DATA FOR ANALYSES

# Put it nice for submission:

#Change names
colnames(f)[2] <- "Year"
colnames(f)[3] <- "Species"
colnames(f)[4] <- "Presence"
colnames(f)[5] <- "Cover"
colnames(f)[6] <- "Cover_dead"
colnames(f)[7] <- "Height"
colnames(f)[8] <- "Diversity"
colnames(f)[10] <- "Heterogeneity"
colnames(f)[11] <- "area"
colnames(f)[12] <- "tbl"
colnames(f)[13] <- "mpar"
colnames(f)[14] <- "agri_practice"
colnames(f)[15] <- "fallow"
colnames(f)[20] <- "crop_diversity"

f$Zone <- as.character(f$Zone)
#f$Zone[which(f$Zone == "OCCIDENTAL")] <- "East"
#f$Zone[which(f$Zone == "ORIENTAL")] <- "West"

f$agri_practice <- as.character(f$agri_practice)
f$agri_practice[which(f$agri_practice == "Picar i herbicidar")] <- "S+H"
f$agri_practice[which(f$agri_practice == "Picar")] <- "S"
f$agri_practice[which(f$agri_practice == "Llaurar")] <- "T"
f$agri_practice[which(f$agri_practice == "Alfals")] <- "A"
f$agri_practice[which(f$agri_practice == "Control")] <- "C"

f$Species <- as.character(f$Species)
f$Species[which(f$Species == "BUOED")] <- "SC"
f$Species[which(f$Species == "MECAL")] <- "CL"
f$Species[which(f$Species == "TERAX_m")] <- "LB"

#Change order
f<- f[ ,c(9,1,2,16,14,3:8,10:13,15,17:20)]

#write.csv(f, "Data_path_submission.csv") #DATA FOR SUBMISSION

#################################################################################################
#MISTAKE: Fields with 0 in veg values because it is wrong in access
#Create a new file without this fields to check if results change
# Names to delete:
del <- c("BE107A_2016","BE109A_2016", "BE110A_2016", "BE88A_2016", "SI123A_2016", "SI133A_2016", "SI41A_2016" )

f2 <- f[ !grepl(paste(del, collapse="|"), f$CF_A),] # Data without those 7 fields

#write.csv(f2, "Data_path_manuscript2.csv")







