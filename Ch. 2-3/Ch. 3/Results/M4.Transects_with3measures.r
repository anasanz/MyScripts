
library(rgdal)
library(rgeos)
library(raster)

setwd("D:/PhD/Third chapter/Data")

sg <- read.csv("SG_15_19.csv")
aes <- read.csv("AES_15_19.csv")
green <- read.csv("GREEN_15_19.csv")

aes <- aes[,-c(1,2)]
sg <- sg[,-c(1,2)]
green <- green[,-c(1,2)]


## ---- % of total transect area ----

tr <- readOGR("D:/PhD/Third chapter/GIS", "Trans_2018_EPSG23031") 
buf <- gBuffer(tr, byid = TRUE, width = 500)
area_buf <- sum(area(buf)/10000)
area_buf_allyears <- area_buf*5

prop_sg <- (sum(sg)/area_buf_allyears)*100
prop_aes <- (sum(aes)/area_buf_allyears)*100
prop_green <- (sum(green)/area_buf_allyears)*100


## ---- Overlap ----

# putting the matrices into an array first (of 264 rows, 5 columns, and 3 strata), 
# and then summing within each row and column (1:2) of each stratum

aes <- ifelse(aes>0,1,0)
sg <- ifelse(sg>0,1,0)
green <- ifelse(green>0,1,0)


m <- apply(array(c(aes,sg,green),dim=c(264,5,3)),1:2,sum,na.rm=TRUE) 
prop3 <- (length(which(m==3))/length(m))*100
prop2 <- (length(which(m==2))/length(m))*100
prop1 <- (length(which(m==1))/length(m))*100
prop0 <- (length(which(m==0))/length(m))*100

sum(prop0,prop1,prop2,prop3)
sum(prop2,prop3)




