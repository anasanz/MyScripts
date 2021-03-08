
rm(list = ls())

library(rgdal)
library(dplyr)
library(tidyr)

## -------------------------------------------------
## FIX DATA BASE (ADD FLYING POSITIONS TO ORIGINAL DATABASE):
# - With NA from missing habitat
# - With periods sorted
## ------------------------------------------------- 

## ---- 1. Load data with flying positions to select them ----

setwd("D:/PhD/Fourth chapter/GPS Cataluña/Ganga/Updated_24-2-2020")
d <- readOGR("D:/PhD/Fourth chapter/GPS Cataluña/Ganga/Updated_24-2-2020", "XYgps_positions_with_flying_no_regadio")
d <- d@data

## Divide periods ----

d$period <- 0

# Pre-bottleneck (1st December – 25th February) 
d$period[d$Month %in% c(12,1,2)] <- "Pre" # There is no positions from 25th feb

# Bottleneck (8th March – 31st May)
d$period[d$Month %in% c(3,4,5)] <- "PreRep"
d$period[which(d$Month == 3 & d$Day < 8)] <- 0

# Post-Bottleneck (10th June – 31st August)
d$period[d$Month %in% c(6,7,8)] <- "Rep"
d$period[which(d$Month == 6 & d$Day < 10)] <- 0

d <- d[-which(d$period == 0), ] # Son mas que en el analisis potque estan las posiciones volando + posiciones en el regadío

d_fly <- d[which(d$fly == 1), ] # 858 volando

write.table(x = d_fly, file = "D:/PhD/Fourth chapter/GPS Cataluña/Ganga/Updated_24-2-2020/Flying_no_regadio_ETRS89.txt", sep = "\t", dec = ",",
            col.names = T, row.names = F)

## ---- 2. Load original data (used in analyses + NA) ----

pos <- read.table("D:/PhD/Fourth chapter/GPS Cataluña/Ganga/Updated_24-2-2020/D_gangas_no_regadio_ETRS89_tot_ANALISIS_PERIODS_NAHABITAT.txt", header = T, dec = ",",
                  sep = "\t")

colnames(pos)[21] <- "period"
pos$fly <- 0
colnames(d_fly)

new_names <- c("Logger_ID","Year","Month","Day","Hour","Minute","Second",
               "Latitude", "Longitude","Speed","Raw_latitu","Raw_Longit", "period", "fly")

## ---- 3. Join ----

pos <- pos[,which(colnames(pos) %in% new_names)]
d_fly <- d_fly[,which(colnames(d_fly) %in% new_names)]

all_data <- rbind(pos,d_fly)

write.table(x = all_data, file = "D:/PhD/Fourth chapter/GPS Cataluña/Ganga/Updated_24-2-2020/FINAL_ALLpos_no_regadio_ETRS89.txt", sep = "\t", dec = ",",
            col.names = T, row.names = F)
