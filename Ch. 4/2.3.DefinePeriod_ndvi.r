

rm(list=ls())

library(dplyr)

setwd("D:/PhD/Fourth chapter/GIS/Capas_Rocío/GPS")
pos <- read.csv("positions_feb_ab_periods1_2.csv")
pos[which(pos$Month == 2 & pos$Day >= 15), ] # No hay posiciones en Febrero a partir del 15

pos <- pos[,c(2:10, 21:23)]
pos <- pos[which(complete.cases(pos)), ] # Select 15feb-30ab (which have values of NDVI)

# Create date column
pos$date <- paste(pos$Month,"/",pos$Day, sep = "")
pos <- pos[order(as.Date(pos$date, format= "%m/%d")),]
pos$date <- factor(pos$date, levels = unique(pos$date)) # change factor to sort factor levels

# Posiciones con NDVI ALTO
hist(pos$ndvi) # Hay posiciones con un ndvi máximo (255)
pos_max_ndvi <- pos[which(pos$ndvi > 230), ] 
981/5732 # Sólo un 17%
barplot(table(pos_max_ndvi$date), main = "Nº posiciones GPS", xlab = "", ylab = "", las = 2) # La mayoría a finales de abril (porque el hábitat disponible en general tiene ndvi muy alto)

# Evolution of mean ndvi/day
ndvi_date <- pos %>%
  group_by(date) %>%
  summarize(mean(ndvi))

barplot(ndvi_date$`mean(ndvi)` ~ ndvi_date$date, las = 2)
plot(ndvi_date$`mean(ndvi)` ~ ndvi_date$date, type = "p", las = 2)
lines(ndvi_date$date, ndvi_date$`mean(ndvi)`, type="l")



