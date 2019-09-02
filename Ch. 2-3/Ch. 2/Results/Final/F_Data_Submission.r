

# DATA READY FOR SUBMISSION

library(dplyr)

setwd("S:/PhD/Second chapter/Data")

d <- read.csv("DataDS_ready_ALL.csv")
colnames(d)[which(colnames(d) == "Count")] <- "Cluster" 

s_good <- c("ALRUF","BUOED","CACAR","COOEN","COPAL","GACRI","GATHE","MEAPI","MECAL","PAMAJ","SESER","STSSP","SYCAN","SYMEL","TERAX","UPEPO",
            "MICAL","HIRUS","PADOM","PIPIC","PAMON", "COMON")

d <- d[which(d$Species %in% s_good), ] 

setwd("S:/PhD/Second chapter/Data")
leg <- read.csv("leg_species.csv", sep = ";")
leg <- leg[ ,-c(2)]
colnames(leg)[2] <- "species"
d$species <- NA
colnames(d)[8] <- "codiEspecie"
d <- left_join(d, leg, by = "codiEspecie")

d <- d[ ,which(colnames(d) %in% c("Year", "Banda", "Cluster", "Observer", "Temp", "transectID", "distbegin", "distend",
                                  "species.y"))]
d <- d[ , c(6,1,9,2,3,4,5)]
d$transectID <- as.numeric(d$transectID)
d$Observer <- as.numeric(d$Observer)

colnames(d)[3] <- "Species"
colnames(d)[7] <- "Temperature"

unique(d$Temperature)
d$Temperature[which(d$Temperature == 2)] <- 12
d$Temperature[which(d$Temperature == 3)] <- 13
d$Temperature[which(d$Temperature == 4)]<- 14

setwd("S:/PhD/Second chapter/Docs/SUBMISSION 1/Submit/Biodiversity and Conservation")
write.csv(d, "data_detectability_manuscript.csv")

