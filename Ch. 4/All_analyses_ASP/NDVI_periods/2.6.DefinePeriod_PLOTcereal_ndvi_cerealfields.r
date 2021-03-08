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

# Add dates manually where there are no observations (1 row with NA per observation)

missing_dates <- c("12/28", "12/29", "12/30", "12/31","2/15", "2/16", "2/17", "2/18", "2/19", "2/20", "2/21", "2/22",
                   "2/23", "2/24", "2/25", "2/26", "2/27", "2/28", "3/1")

df <- as.data.frame(matrix(NA, nrow = 19, ncol = 13))
df[,13] <- missing_dates
colnames(df) <- colnames(positions_cereal)

positions_cereal <- rbind(positions_cereal[1:519,],df[1:4, ],positions_cereal[-(1:519),]) # Insert-December (after 519)
positions_cereal <- rbind(positions_cereal[1:1694,],df[5:19, ],positions_cereal[-(1:1694),])# Insert-Febrero 

# Plot

positions_cereal$date <- factor(positions_cereal$date, levels = unique(positions_cereal$date)) # change factor to sort factor levels

# Create vector for names x axes
dates <- c("1-Dic", "18-Ene", "20-Mar", "28-Ab", "6-Jun", "18-Jul", "29-Agos")
res <- barplot(table(positions_cereal$date), xaxs="i",xaxt="n") # To get midpoints
coordx <- res[c(1, 49, 110, 149, 188, 230, 272)]
# Get coordinates division lines
f <- table(positions_cereal$date)
names(f)

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
pdf("posiciones_cereal_dataNDVI3.pdf")

par(mfrow = c(2,1))
barplot(per_cereal$per ~ per_cereal$date, ylab = "% Posiciones en cereal", xlab = "", xaxt = "n", las = 2, main = "2017-2019") # Use vector "names" as well for xlabs 
abline(v = 131.5, lwd = 1.3)
abline(v = 225.1, lwd = 1.3)
mtext(dates, side = 1, line = 0.5, at = coordx, las = 2)
polcoord <- cbind(c(res[77], res[77], res[137], res[137]), c(0,43, 43, 0))
polygon(polcoord[,1], polcoord[,2], col = NA, border = "red", lwd = 1.3)
abline(v = res[91], lwd = 1.3, col = "blue") # 1 Marzo 
abline(v = res[97], lwd = 1.3, col = "magenta") # 7 Marzo

#######################################################
# NDVI CEREAL FIELDS

setwd("D:/PhD/Fourth chapter/Results/Preliminar results")

load("ndvi_cereal_ALL.RData")

mean <- apply(ndvi_all2, 2, mean)
sd <- apply(ndvi_all2, 2, sd)
se <- apply(ndvi_all2, 2, sd) / sqrt(24)
lower <- mean - 1.96 * se
upper <- mean + 1.96 * se

df2 <- cbind(mean, lower, upper)

df2 <- as.data.frame(df2[c(1,2,4,3,6,5), ])
rownames(df2) <- gsub("X", "", rownames(df2))
names <- rownames(df2)
y <- seq(1,6)


plot(500, pch = 19, ylim = c(0.4, 0.9), xlim = c(1,6), axes = FALSE, xlab = " ", ylab = "NDVI", main = "Todos campos cereal del area de estudio (n = 745)")
points(df2$mean, pch = 19)
axis(1, at = c(1, 2, 3, 4, 5, 6), labels = names, cex.axis = 1)
axis(2)
arrows(y, df2$lower, y, df2$upper, code = 3, angle = 90, length = 0.1)
points(df2$mean, pch = 19, type = "l")
abline(v = 4.5, lwd = 1.3)
abline(v = 2.5, lwd = 1.3, col = "blue")
abline(v = 3, lwd = 1.3, col = "Magenta")

mtext("Fecha (Imágenes NDVI)", line = 0, side = 1,  outer = TRUE)
dev.off()

#plot(cereal17)
#plot(cereal17_ndvi, add = TRUE, col = "red")
#points(pos_2, pch = 19, col = "green")


