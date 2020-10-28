
#########################
# CEREAL


# Load 

setwd("D:/PhD/Fourth chapter/GIS/Capas_Rocío/GPS")
pos <- read.csv("positions_cereal.csv")
positions_cereal <- pos[ ,c(2:12,17)]


# Create date column
positions_cereal$date <- paste(positions_cereal$Month,"/",positions_cereal$Day, sep = "")
positions_cereal <- positions_cereal[order(as.Date(positions_cereal$date, format= "%m/%d")),]

# Arrange cutre by date so that it is in order (paste manually the ones from december up):
pos_cereal_dic <- positions_cereal[grep("12/", as.character(positions_cereal$date)), ]
positions_cereal <- positions_cereal[-grep("12/", as.character(positions_cereal$date)), ]
positions_cereal <- rbind(pos_cereal_dic, positions_cereal)

# Plot

positions_cereal$date <- factor(positions_cereal$date, levels = unique(positions_cereal$date)) # change factor to sort factor levels

# Create vector for names x axes
dates <- c("1-Dic", "18-Ene", "20-Mar", "28-Ab", "6-Jun", "18-Jul", "29-Agos")
res <- barplot(table(positions_cereal$date), xaxs="i",xaxt="n") # To get midpoints
coordx <- res[c(1, 45, 91, 130, 169, 211, 253)]
# Get coordinates division lines
f <- table(positions_cereal$date)
names(f)
res[59] #1feb
res[132] #1feb
# Percentage of positions in cereal per day

positions_cereal$cereal <- as.factor(positions_cereal$cereal) # Need to be factor to not drop levels with empty observations (keep the 0 where there is no positions in cereal)
data_cereal <- positions_cereal %>%
  group_by(date, cereal, .drop = FALSE) %>%
  summarise(count = n() )

per_cereal <- data_cereal %>%
  group_by(date) %>%
  mutate(countT= sum(count)) %>%
  group_by(cereal, add=TRUE) %>%
  mutate(per=round(100*count/countT,2))

per_cereal <- per_cereal[which(per_cereal$cereal == 1), ]
per_cereal$date <- factor(per_cereal$date, levels = unique(per_cereal$date)) # change factor to sort factor levels


setwd("D:/PhD/Fourth chapter/Results/Preliminar results")
pdf("posiciones_cereal_dataNDVI.pdf")

par(mfrow = c(2,1))
barplot(per_cereal$per ~ per_cereal$date, ylab = "% Posiciones en cereal", xlab = "", xaxt = "n", las = 2, main = "2017-2019") # Use vector "names" as well for xlabs 
abline(v = 108.7, lwd = 2)
abline(v = 202.3, lwd = 2)
mtext(dates, side = 1, line = 0.5, at = coordx, las = 2)
polcoord <- cbind(c(70.3, 70.3, 157.9, 157.9), c(0,43, 43, 0))
polygon(polcoord[,1], polcoord[,2], col = NA, border = "red", lwd = 1.5)


########################
# NDVI 

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



# 2. --- Explore NDVI in cereal ----

pos_cereal <- posit2[posit2$cereal == 1, ]
c <- pos_cereal[which(!is.na(pos_cereal$ndvi)), ] # Remove NA in ndvi (data from 2018 and 2019)

# Adjust data with reliable NDVI ( First image = 15feb (cut in 7feb); Last image: 16Ab (cut in 23 Ab))
#c <- c[which(c$Date > "2017-02-07" & c$Date < "2017-04-24"), ]
#c$date <- as.factor(as.character(c$date))

nrow(c)/nrow(pos_cereal) # Conclusions based on the 40% of positions in cereal

#hist(c$ndvi)

# Evolution of mean ndvi/day
ndvi_date <- c %>%
  group_by(date) %>%
  summarize(mean(ndvi))

#barplot(ndvi_date$`mean(ndvi)` ~ ndvi_date$date, las = 2)
#res <- barplot(table(ndvi_date$date), xaxs="i",xaxt="n") # To get midpoints


plot(ndvi_date$`mean(ndvi)` ~ ndvi_date$date, ylim = c(0.37, 0.9), type = "p", las = 2, xlab = "Fecha", ylab = "NDVI en cereal", main = "2017", border = 1)
lines(ndvi_date$date, ndvi_date$`mean(ndvi)`, type="l")
abline(h = 0.8, lty = 2)
abline(v = 33.1, lwd = 2) # A ojo...20Marzo (División anterior)
abline(v = 45.1, lty = 2) # A ojo...1Abril

points(37, 0.8796296, type='p', pch = 8, cex = 1.7)
box(which = "plot", lty = "solid", col = "red", lwd = 2)
dev.off()


hist(c$ndvi)

