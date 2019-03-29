
rm(list=ls())

library(dplyr)

# ---- 1. Calculate mean vegetation per field
# ------ CEREAL ----

setwd("S:/PhD/Second chapter/Data")
cer <- read.csv("DataCEREAL.csv", sep = ";") 

names(cer)
colnames(cer)[which(colnames(cer) == "Transecte_detall_Id_transecte_detall")] <- "Sample.Label"
colnames(cer)[which(colnames(cer) == "Codi_seca")] <- "Region.Label"
colnames(cer)[which(colnames(cer) == "Any")] <- "Year"

# Create variable transectID, than matches with the code of the GIS layers (i.e., two digits: 09) 

#1. Add a 0 before the transect number
for (i in 1:nrow(cer)){ 
  cer$Num_transecte[i] <- paste(0,cer$Num_transecte[i], sep = "")
}

#2. Keep only the last 2 digits (but not for the 100!!)
library(stringr)
for (i in 1:nrow(cer)){ 
  if (as.numeric(cer$Num_transecte[i]) < 100){
  cer$Num_transecte[i] <- str_sub(cer$Num_transecte[i], start = -2)
  } else { 
  cer$Num_transecte[i] <- str_sub(cer$Num_transecte[i], start = -3)}
}


# Create variable by pasting it
for (i in 1:nrow(cer)){ 
  cer$transectID[i] <- paste(cer$Region.Label[i],cer$Num_transecte[i], sep = "")
}

# Paste Transect-Year
for (i in 1:nrow(cer)){ 
  cer$T_Y[i] <- paste(cer$transectID[i],cer$Year[i], sep = "_")
}

# --- Cover 

cer_cov <- cer[ ,which(colnames(cer) %in% c("transectID","T_Y", "Region.Label", "Num_transecte", 
                                       "Year", "Codi_cereal","Rec_1","Rec_2", 
                                       "Rec_3", "Rec_4", "Rec_5"))]
cer_cov$field_cov <- apply(cer_cov[5:9], 1, mean, na.rm=TRUE) # Mean per field
covCereal <- aggregate(cer_cov$field_cov, list(cer_cov$T_Y), mean, na.rm=TRUE)
colnames(covCereal)[1] <- "T_Y"
colnames(covCereal)[2] <- "covCereal"
############################################################ HERE IM STUPID
join <- cer_cov[which(!duplicated(cer_cov$T_Y)),c(3,10:11)]

# Add transect id and year to keep the information for later
covCereal <- left_join(covCereal, join)
covCereal <- covCereal[ ,c(1,4,3,2)]

# --- Height

cer_h <- cer[ ,which(colnames(cer) %in% c("transectID","T_Y", "Region.Label", "Num_transecte", 
                                            "Year", "Codi_cereal","Al.ada_1","Al.ada_2", 
                                            "Al.ada_3", "Al.ada_4", "Al.ada_5"))]
cer_h$field_cov <- apply(cer_h[5:9], 1, mean, na.rm=TRUE) # Mean per field
heightCereal <- aggregate(cer_h$field_cov, list(cer_h$T_Y), mean, na.rm=TRUE)
colnames(heightCereal)[1] <- "T_Y"
colnames(heightCereal)[2] <- "heightCereal"

vegCereal <- left_join(covCereal, heightCereal, by = "T_Y")



# ------ FALLOW ----

fal <- read.csv("DataFALLOW.csv", sep = ";") # 
colnames(fal)
colnames(fal)[which(colnames(fal) == "Codi_seca")] <- "Region.Label"
colnames(fal)[which(colnames(fal) == "Any")] <- "Year"

# Create variable transectID, than matches with the code of the GIS layers (i.e., two digits: 09) 

#1. Add a 0 before the transect number
for (i in 1:nrow(fal)){ 
  fal$Num_transecte[i] <- paste(0,fal$Num_transecte[i], sep = "")
}

#2. Keep only the last 2 digits (but not for the 100!!)
library(stringr)
for (i in 1:nrow(fal)){ 
  if (as.numeric(fal$Num_transecte[i]) < 100){
    fal$Num_transecte[i] <- str_sub(fal$Num_transecte[i], start = -2)
  } else { 
    fal$Num_transecte[i] <- str_sub(fal$Num_transecte[i], start = -3)}
}


# Create variable by pasting it
for (i in 1:nrow(fal)){ 
  fal$transectID[i] <- paste(fal$Region.Label[i],fal$Num_transecte[i], sep = "")
}

# Paste Transect-Year
for (i in 1:nrow(fal)){ 
  fal$T_Y[i] <- paste(fal$transectID[i],fal$Year[i], sep = "_")
}

# --- Cover 

fal_cov <- fal[ ,which(colnames(fal) %in% c("transectID","T_Y", "Region.Label", "Num_transecte", 
                                            "Year", "Codi_guaret","Rec_1_g","Rec_2_g", 
                                            "Rec_3_g", "Rec_4_g", "Rec_5_g"))]
fal_cov$field_cov <- apply(fal_cov[5:9], 1, mean, na.rm=TRUE) # Mean per field
covFallow <- aggregate(fal_cov$field_cov, list(fal_cov$T_Y), mean, na.rm=TRUE)
colnames(covFallow)[1] <- "T_Y"
colnames(covFallow)[2] <- "covFallow"


# --- Height

fal_h <- fal[ ,which(colnames(fal) %in% c("transectID","T_Y", "Region.Label", "Num_transecte", 
                                          "Year", "Codi_guaret","Al.ada_1_g","Al.ada_2_g", 
                                          "Al.ada_3_g", "Al.ada_4_g", "Al.ada_5_g"))]
fal_h$field_cov <- apply(fal_h[5:9], 1, mean, na.rm=TRUE) # Mean per field
heightFallow <- aggregate(fal_h$field_cov, list(fal_h$T_Y), mean, na.rm=TRUE)
colnames(heightFallow)[1] <- "T_Y"
colnames(heightFallow)[2] <- "heightFallow"

vegFallow <- left_join(covFallow, heightFallow, by = "T_Y")

veg <- left_join(vegCereal, vegFallow, by = "T_Y")

setwd("S:/PhD/Second chapter/Data")
write.csv(veg,"veg_variable.csv")

# 

# ---- 2. Relate it to management ----
# See in transects with management vS not management if average veg.height and cover changes
setwd("C:/Users/Ana/Documents/PhD/Second chapter/Data")
manag <- read.csv("management_area.csv")
manag$area_sg10 <- NA
manag$area_sg11 <- NA
manag$area_sg12 <- NA
manag$area_sg13 <- NA


colnames(manag)[2] <- "transectID"
veg$area_AES <- NA
veg$area_SG <- NA

year <- c(2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017)

for (i in 1:length(year)){
  veg_year <- veg[which(veg$Year == year[i]), ] # Select vegetation data for each year
  manag_year <- manag[ , c(2,which(grepl(str_sub(year[i], start = -2), names(manag))))] # Select area management per year
  manag_year$Year <- year[i] #Create year column to join to vegetation
  veg_manag <- left_join(veg,manag_year)
  veg[which(veg$Year == year[i]), which(colnames(veg) %in% c("area_AES", "area_SG"))] <- veg_manag[which(veg_manag$Year == year[i]), which(grepl(str_sub(year[i], start = -2), names(veg_manag)))]
}

setwd("C:/Users/Ana/Documents/PhD/Second chapter/Data")
#write.csv(veg,"veg_management.csv")

# ---- 3. Explore relation between vegetation height and management measure ----

rm(list=ls())

setwd("C:/Users/Ana/Documents/PhD/Second chapter/Data")
veg <- read.csv("veg_management.csv")
veg[which(veg$area_AES > 40), ] # They are outliers but they are fine

# Check AES
plot(covCereal ~ area_AES, data = veg, pch = 19)
abline(lm(covCereal ~ area_AES, data = veg), pch = 19)
plot(covCereal ~ area_AES, data = veg, xlim = c(0,20), pch = 19)

plot(heightCereal ~ area_AES, data = veg)
abline(lm(heightCereal ~ area_AES, data = veg), pch = 19)
plot(heightCereal ~ area_AES, data = veg, xlim = c(0,20), pch = 19)

plot(covFallow ~ area_AES, data = veg, pch = 19)
abline(lm(covFallow ~ area_AES, data = veg), pch = 19)
plot(covFallow ~ area_AES, data = veg,  xlim = c(0,20), pch = 19)

setwd("C:/Users/Ana/Documents/PhD/Second chapter/Data/Figures")
pdf("heightFallow_AES.pdf")
plot(heightFallow ~ area_AES, data = veg, pch = 19)
abline(lm(heightFallow ~ area_AES, data = veg), pch = 19)
dev.off()
summary(lm(heightFallow ~ area_AES, data = veg))
plot(heightFallow ~ area_AES, data = veg,  xlim = c(0,20), pch = 19) 
      #Maybe the height of fallow is affected by a bigger area of AES within the buffer


# Check Fallow
plot(covFallow ~ area_SG, data = veg, pch = 19)
abline(lm(covFallow ~ area_SG, data = veg), pch = 19)
plot(covFallow ~ area_SG, data = veg,  xlim = c(0,20), pch = 19)

setwd("C:/Users/Ana/Documents/PhD/Second chapter/Data/Figures")
pdf("heightFallow_SG.pdf")
plot(heightFallow ~ area_SG, data = veg, pch = 19)
abline(lm(heightFallow ~ area_SG, data = veg), pch = 19)
dev.off()
summary(lm(heightFallow ~ area_SG, data = veg))
plot(heightFallow ~ area_SG, data = veg,  xlim = c(0,20), pch = 19) 
      #Maybe the height of fallow is affected by a bigger area of SG within the buffer


