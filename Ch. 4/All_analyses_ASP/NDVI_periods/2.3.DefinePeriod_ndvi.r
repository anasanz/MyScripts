

rm(list=ls())

library(dplyr)

# 1. --- Join datasets ----

setwd("D:/PhD/Fourth chapter/GIS/Capas_Rocío/GPS")
pos <- read.csv("positions_feb_ab_periods1_2.csv")
pos[which(pos$Month == 2 & pos$Day >= 15), ] # No hay posiciones en Febrero a partir del 15

pos <- pos[,c(2:10, 21:24)]
#pos <- pos[which(complete.cases(pos)), ] # Select 15feb-30ab (which have values of NDVI)
pos <- pos[which(pos$Month == 2 | pos$Month == 3 | pos$Month == 4), ]
# Create 2 dates column
# (Day-Month)
pos$date <- paste(pos$Month,"/",pos$Day, sep = "")
pos <- pos[order(as.Date(pos$date, format= "%m/%d")),]
pos$date <- factor(pos$date, levels = unique(pos$date)) # change factor to sort factor levels
# Year-Month-Day (To join with real NDVI values)
pos$Date <- as.Date(paste(pos$Year, pos$Month, pos$Day, sep = "-")) # Generate date column
colnames(pos)[12] <- "ndvi_DN"

# Join real positions NDVI from 2017
setwd("D:/PhD/Fourth chapter/GIS/Capas_Rocío/GPS")
pos2 <- read.csv("positions_ndvi_2017.csv")
pos2 <- pos2[,c(6,8,9)]

posit <- left_join(pos, pos2, by = c("ID_pos"))

# Join with positions in cereal
setwd("D:/PhD/Fourth chapter/GIS/Capas_Rocío/GPS")
pos3 <- read.csv("positions_cereal.csv")
## AQUI: Define positions (Only Feb_Abril) Our just join?
pos3 <- pos3[which(pos3$Month == 2 | pos3$Month == 3 | pos3$Month == 4), ]
pos3 <- pos3[,c(13, 17)]
# Check if join in right, and delete columns
posit2 <- left_join(posit, pos3, by = "ID_pos")


# Correlación DN-ndvi
posit3 <- posit2[which(!is.na(posit2$ndvi)), ]
cor(posit3$ndvi_DN, posit3$ndvi, method = "pearson") # Negative correlation? Higher values NDVI, lower of DN
plot(posit3$ndvi~posit3$ndvi_DN)

#### In any case: Descartar DN --> La resolución es muy baja y la relación es extraña
#### Centrarse en NDVI 2017 y posiciones en cereal para ver si se puede buscar alguna relación

# 2. --- Explore NDVI in cereal ----

pos_cereal <- posit2[posit2$cereal == 1, ]
c <- pos_cereal[which(!is.na(pos_cereal$ndvi)), ] # Remove NA in ndvi (data from 2018 and 2019)

# Adjust data with reliable NDVI ( First image = 15feb (cut in 7feb); Last image: 16Ab (cut in 23 Ab))
#c <- c[which(c$Date > "2017-02-07" & c$Date < "2017-04-24"), ]
#c$date <- as.factor(as.character(c$date))

nrow(c)/nrow(pos_cereal) # Conclusions based on the 40% of positions in cereal

hist(c$ndvi)


# ---- Evolution of mean ndvi/day ----
ndvi_date <- c %>%
  group_by(date) %>%
  summarize(mean(ndvi))

summary(ndvi_date)


barplot(ndvi_date$`mean(ndvi)` ~ ndvi_date$date, las = 2)
res <- barplot(table(ndvi_date$date), xaxs="i",xaxt="n") # To get midpoints

setwd("D:/PhD/Fourth chapter/Results/Preliminar results")
pdf("ndvi.pdf", 9, 5)
plot(ndvi_date$`mean(ndvi)` ~ ndvi_date$date, type = "p", las = 2, xlab = "Fecha", ylab = "NDVI en cereal")
lines(ndvi_date$date, ndvi_date$`mean(ndvi)`, type="l")
abline(h = 0.8, lty = 2)
abline(v = 33.1, lwd = 2) # A ojo...20Marzo (División anterior)
abline(v = 45.1, lty = 2) # A ojo...1Abril

points(37, 0.8796296, col= adjustcolor("red",alpha.f = 0.8), type='p', pch = 8, cex = 1.7)

dev.off()


# MÁXIMO ES EL 24 MARZO 
# ÚLTIMA FECHA EN QUE NDVI > 0.8 ES EL 1 DE ABRIL (Ignorar la del 27 de abril que está basada en el 16?)

# Posiciones con NDVI ALTO

pos_max_ndvi <- c[which(c$ndvi > 0.8), ] 
barplot(table(pos_max_ndvi$date), main = "Nº posiciones GPS", xlab = "", ylab = "", las = 2) # La mayoría a finales de abril (porque el hábitat disponible en general tiene ndvi muy alto)

# ---- Evolution of mean ndvi/week ----

x <- c %>% 
  mutate(week = cut.Date(Date, breaks = "1 week", labels = FALSE)) %>% 
  arrange(date)

x2 <- c %>% 
  mutate(week = cut.Date(Date, breaks = "1 week", labels = FALSE)) %>% 
  arrange(date)

ndvi_week <- x %>%
  group_by(week) %>%
  mutate(int = min(Date)) %>%
  

min(c$Date)

plot(ndvi_week$`mean(ndvi)` ~ ndvi_week$week, ylim = c(0.5, 0.8), type = "p", las = 2, xlab = "Fecha", ylab = "NDVI en cereal")
lines(ndvi_week$week, ndvi_week$`mean(ndvi)`, type="l")
