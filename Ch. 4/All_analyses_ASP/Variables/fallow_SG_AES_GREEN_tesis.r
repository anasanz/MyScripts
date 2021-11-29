

rm(list=ls())

library(rgdal)
library(raster)

## -------------------------------------------------
##       Calculate proportions descriptive
## ------------------------------------------------- 

# Fallow variable
gest17 <- readOGR("D:/PhD/Fourth chapter/Data/GIS/Capas_Rocío/barbechos y rebaños", "clipMCP_barbechos17_23031")
gest18 <- readOGR("D:/PhD/Fourth chapter/Data/GIS/Capas_Rocío/barbechos y rebaños", "clipMCP_barbechos18_23031")
gest19 <- readOGR("D:/PhD/Fourth chapter/Data/GIS/Capas_Rocío/barbechos y rebaños", "clipMCP_barbechos19_23031")

# AES
aes17 <- readOGR("D:/PhD/Third chapter/GIS/Fallow_FIX/AES", layer = "AEScutted_2017_EPSG23031_FIXED_FALLOW")
aes18 <- readOGR("D:/PhD/Third chapter/GIS/Fallow_FIX/AES", layer = "AEScutted_2018_EPSG23031_FALLOW")
aes19 <- readOGR("D:/PhD/Third chapter/GIS/Fallow_FIX/AES", layer = "AEScutted_2019_EPSG23031_FALLOW")

aes17 <- aes17[which(aes17$PROD_NOM == "FALLOW"), ]
aes18 <- aes18[which(aes18$PROD_NOM == "FALLOW"), ]
aes19 <- aes19[which(aes19$PROD_NOM == "FALLOW"), ]

green17 <- readOGR("D:/PhD/Third chapter/GIS/Fallow_FIX/Greening", layer = "GREENcutted_2017_EPSG23031")
green18 <- readOGR("D:/PhD/Third chapter/GIS/Fallow_FIX/Greening", layer = "GREENcutted_2018_EPSG23031")
green19 <- readOGR("D:/PhD/Third chapter/GIS/Fallow_FIX/Greening", layer = "GREENcutted_2019_EPSG23031")

green17$ID <- rownames(green17@data)
green18$ID <- rownames(green18@data)
green19$ID <- rownames(green19@data)

## ---- 1. Proportion of TFM and CFM ----

gest17_TFM <- gest17[which(gest17$manag %in% "SI"), ]
gest17_CFM <- gest17[which(gest17$manag %in% "NO"), ]

gest18_TFM <- gest18[which(gest18$manag %in% "SI"), ]
gest18_CFM <- gest18[which(gest18$manag %in% "NO"), ]

gest19_TFM <- gest19[which(gest19$manag %in% "SI"), ]
gest19_CFM <- gest19[which(gest19$manag %in% "NO"), ]

# Per year
prop_17 <- sum(area(gest17_TFM))/sum(area(gest17))
prop_18 <- sum(area(gest18_TFM))/sum(area(gest18))
prop_19 <- sum(area(gest19_TFM))/sum(area(gest19))

# Total
sum(sum(area(gest17_TFM)) + sum(area(gest18_TFM)) + sum(area(gest19_TFM))) / sum(sum(area(gest17)) + sum(area(gest18)) + sum(area(gest19)))

## ---- From the CFM: Proportion AES and Greening ----

prop <- data.frame(matrix(nrow = 3, ncol = 2))
colnames(prop) <- c("AES", "Green")
rownames(prop) <- c("2017", "2018", "2019")

# 2017

# AES
ID_aes_CFM_17 <- over(gest17_CFM,aes17)
aes17_CFM <- aes17[which(aes17$OBJECTID %in% unique(ID_aes_CFM_17$OBJECTID)), ]

prop[1,1] <- round(sum(area(aes17_CFM))/sum(area(gest17_CFM)),2)

# GREEN
crs(gest17_CFM)
ID_green_CFM_17 <- over(gest17_CFM,green17)
green17_CFM <- green17[which(green17$ID %in% unique(ID_green_CFM_17$ID)), ]

prop[1,2] <- round(sum(area(green17_CFM))/sum(area(gest17_CFM)),2)

plot(gest17_CFM)
plot(aes17_CFM, col = "orange", border = NA, add = TRUE)
plot(green17_CFM, col = "red", border = NA, add = TRUE)

# 2018

# AES
ID_aes_CFM_18 <- over(gest18_CFM,aes18)
aes18_CFM <- aes18[which(aes18$OBJECTID %in% unique(ID_aes_CFM_18$OBJECTID)), ]

prop[2,1] <- round(sum(area(aes18_CFM))/sum(area(gest18_CFM)),2)

# GREEN

ID_green_CFM_18 <- over(gest18_CFM,green18)
green18_CFM <- green18[which(green18$ID %in% unique(ID_green_CFM_18$ID)), ]

prop[2,2] <- round(sum(area(green18_CFM))/sum(area(gest18_CFM)),2)

plot(gest18_CFM)
plot(aes18_CFM, col = "orange", border = NA, add = TRUE)
plot(green18_CFM, col = "red", border = NA, add = TRUE)

# 2019

# AES
ID_aes_CFM_19 <- over(gest19_CFM,aes19)
aes19_CFM <- aes19[which(aes19$OBJECTID %in% unique(ID_aes_CFM_19$OBJECTID)), ]

prop[3,1] <- round(sum(area(aes19_CFM))/sum(area(gest19_CFM)),2)

# GREEN

ID_green_CFM_19 <- over(gest19_CFM,green19)
green19_CFM <- green19[which(green19$ID %in% unique(ID_green_CFM_19$ID)), ]

prop[3,2] <- round(sum(area(green19_CFM))/sum(area(gest19_CFM)),2)

plot(gest19_CFM)
plot(aes19_CFM, col = "orange", border = NA, add = TRUE)
plot(green19_CFM, col = "red", border = NA, add = TRUE)
               
colMeans(prop)                                                                  
