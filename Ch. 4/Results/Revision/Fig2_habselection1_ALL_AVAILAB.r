## -------------------------------------------------
##    ALL FIGURES HABITAT SELECTION ANALISIS 1 
##          -> DIFFERENT AVAILAB
## ------------------------------------------------- 


rm(list=ls())


library(dplyr)


period <- c("Pre", "PreRep", "Rep")
avail <- c("A1", "A2", "A3", "Aprop1", "Aprop2", "Aprop0.5", "Aprop0.25")

##REV: Take p1 from Revision analyses and the other 2 periods from original analysis

temp <- setwd("D:/PhD/Fourth chapter/Results/RSF_Ana/Analisis 1/results_rspf/Revision")
temp <- list.files(pattern = "*.csv")
data <- lapply(temp, read.csv, sep = ",")

#for(xxx in 1:length(avail)){ 
xxx = 1
file <- grep(avail[xxx], temp) # Files with one type of availability sampling
d <- data[file]
d_names <- temp[file]

p1 <- d[[1]]
p1 <- p1[,-5]
p1$period <- "p1"
p1 <- p1[nrow(p1):1, ]
p1[,1] <- c("DistAsp", "Slope", "DistGrav", "NatVeg", "Forest", "Herb.irri", "Fruit.irri", "Almond", "Olive", "Cereal", "Fallow", "Intercept")
colnames(p1)[1] <- "X"

temp <- setwd("D:/PhD/Fourth chapter/Results/RSF_Ana/Analisis 1/results_rspf")
temp <- list.files(pattern = "*.csv")
data <- lapply(temp, read.csv, sep = ";")

#for(xxx in 1:length(avail)){ 
xxx = 1
file <- grep(avail[xxx], temp) # Files with one type of availability sampling
d <- data[file]
d_names <- temp[file]

p2 <- d[[2]]
p2 <- p2[,-5]
p2$period <- "p2"
p2 <- p2[nrow(p2):1, ]
p2[,1] <- c("DistAsp", "Slope", "DistGrav", "NatVeg", "Forest", "Herb.irri", "Fruit.irri", "Almond", "Olive", "Cereal", "Fallow", "Intercept")
#p2[6,2] <- p2[5,2]-0.2 # Put the same value than for forest, because it doesnt fit (dashed line and explain in legend)
colnames(p2)[1] <- "X"

p3 <- d[[3]]
p3 <- p3[,-5]
p3$period <- "p3"
p3 <- p3[nrow(p3):1, ]
p3[,1] <- c("DistAsp", "Slope", "DistGrav", "NatVeg", "Forest", "Herb.irri", "Fruit.irri", "Almond", "Olive", "Cereal", "Fallow", "Intercept")
colnames(p3)[1] <- "X"

allperiods <- rbind(p1,p2,p3)
colnames(allperiods) <- c("X", "Estimate", "SE", "p", "period")
p <- c("p1","p2","p3")

# Calculate Confint
allperiods$lower <- NA
allperiods$upper <- NA

for (i in 1:length(p)){
  data.period <- allperiods[which(allperiods$period == p[i]), ]
  lower <- data.period$Estimate - 1.96 * data.period$SE
  upper <- data.period$Estimate + 1.96 * data.period$SE
  allperiods$lower[which(allperiods$period == p[i])] <- lower
  allperiods$upper[which(allperiods$period == p[i])] <- upper
}

# Calculate +- se to plot
allperiods$lower.se <- NA
allperiods$upper.se <- NA

for (i in 1:length(p)){
  data.period <- allperiods[which(allperiods$period == p[i]), ]
  lower <- data.period$Estimate - data.period$SE
  upper <- data.period$Estimate + data.period$SE
  allperiods$lower.se[which(allperiods$period == p[i])] <- lower
  allperiods$upper.se[which(allperiods$period == p[i])] <- upper
}

# Representing SE/CI:
# It doesn't look nice on the plot, look at plot rocio, maybe print mean+-SE?

# Set colours for plotting

allperiods$colour <- NA


for (i in 1:nrow(allperiods)){
  if (allperiods$p[i] > 0.05) {allperiods$colour[i] = "grey"} else {
    allperiods$colour[i] <- ifelse (allperiods$Estimate[i] > 0, "cyan4", "darkred") 
  }
}

use <- c("DistAsp", "Slope", "DistGrav", "NatVeg", "Forest", "Herb.irri", "Fruit.irri", "Almond", "Olive", "Cereal", "Fallow")

# Plot

setwd("D:/PhD/Fourth chapter/Results/Figures/Fig1/Availability check")
pdf(paste("Fig1_", avail[xxx], ".pdf", sep = ""), 8,5)

par(mfrow = c(1,3),
    mar = c(4,0,2,0), 
    oma = c(2,7,2,1))

# p1
data.period <- allperiods[which(allperiods$period == p[1]), ] # Choose period
data.period <- data.period[-12, ] # Remove intercept
barCenters <- barplot(data.period$Estimate, 
                      horiz=TRUE, 
                      xlim=c(-18,+7), 
                      ylim = c(0.5,13),
                      xlab=" ", 
                      axes = FALSE,
                      col = data.period$colour)
#arrows(data.period$lower.se, barCenters, data.period$upper.se, barCenters, code = 3, angle = 90, length = 0.1) #  For CI but I dont like it, is a mess. Barcenters is what barplot is called
axis(1, cex.axis = 1.2)
mtext(use, side = 2, line = 1, at = barCenters, las = 2)
mtext("Period 1", side = 3, line = 1, cex = 1.5)
mtext("Beta", side = 1, line = 3, cex = 1)


# p2
data.period <- allperiods[which(allperiods$period == p[2]), ] # Choose period
data.period <- data.period[-12, ] # Remove intercept
barCenters <- barplot(data.period$Estimate, 
                      horiz=TRUE, 
                      xlim=c(-4,+5), 
                      ylim = c(0.5,13),
                      xlab=" ", 
                      axes = FALSE,
                      col = data.period$colour)
segments(x0 = -3.995, y0 = 5, x1 = -3.995, y1 = 8, lty = 2, col = "white")
# arrows(data.period$lower, barCenters, data.period$upper, barCenters, code = 3, angle = 90, length = 0.1) #  For CI but I dont like it, is a mess. Barcenters is what barplot is called
axis(1, cex.axis = 1.2)
mtext("Period 2", side = 3, line = 1, cex = 1.5)
mtext("Beta", side = 1, line = 3, cex = 1)


# p3
data.period <- allperiods[which(allperiods$period == p[3]), ] # Choose period
data.period <- data.period[-12, ] # Remove intercept
barCenters <- barplot(data.period$Estimate, 
                      horiz=TRUE, 
                      xlim=c(-4,+5), 
                      ylim = c(0.5,13),
                      xlab=" ", 
                      axes = FALSE,
                      col = data.period$colour)
# arrows(data.period$lower, barCenters, data.period$upper, barCenters, code = 3, angle = 90, length = 0.1) #  For CI but I dont like it, is a mess. Barcenters is what barplot is called
axis(1, cex.axis = 1.2)
mtext("Period 3", side = 3, line = 1, cex = 1.5)
mtext("Beta", side = 1, line = 3, cex = 1)
dev.off()

#}


## -------------------------------------------------
##                FIGURE LONG FORMAT
## ------------------------------------------------- 

setwd("D:/PhD/Fourth chapter/Results/Figures_Tables/Fig_bottleneck/Revision")
pdf("Fig1_A1_vert.pdf", 4,10)

par(mfrow = c(3,1),
    mar = c(4,3,2,0), 
    oma = c(2,5,1,2))

# p1
data.period <- allperiods[which(allperiods$period == p[1]), ] # Choose period
data.period <- data.period[-12, ] # Remove intercept
barCenters <- barplot(data.period$Estimate, 
                      horiz=TRUE, 
                      xlim=c(-18,+18), 
                      ylim = c(0.5,13),
                      xlab=" ", 
                      axes = FALSE,
                      col = data.period$colour)
#arrows(data.period$lower.se, barCenters, data.period$upper.se, barCenters, code = 3, angle = 90, length = 0.1) #  For CI but I dont like it, is a mess. Barcenters is what barplot is called
axis(1, cex.axis = 1.2)
mtext(use, side = 2, line = 1, at = barCenters, las = 2, cex = 0.8)
#mtext("Period 1", side = 3, line = 1, cex = 1)
mtext("Beta", side = 1, line = 2.5, cex = 0.8)


# p2
data.period <- allperiods[which(allperiods$period == p[2]), ] # Choose period
data.period <- data.period[-12, ] # Remove intercept
data.period[5,2] <- -7 # Modify only for plot
data.period[6,2] <- -7# Modify only for plot
barCenters <- barplot(data.period$Estimate, 
                      horiz=TRUE, 
                      xlim=c(-7,+7), 
                      ylim = c(0.5,13),
                      xlab=" ", 
                      axes = FALSE,
                      col = data.period$colour)
segments(x0 = -7, y0 = 4, x1 = -7, y1 = 7.5, lty = 2, col = "white", lwd = 1)
# arrows(data.period$lower, barCenters, data.period$upper, barCenters, code = 3, angle = 90, length = 0.1) #  For CI but I dont like it, is a mess. Barcenters is what barplot is called
axis(1, cex.axis = 1.2)
mtext(use, side = 2, line = 1, at = barCenters, las = 2, cex = 0.8)
#mtext("Period 2", side = 3, line = 1, cex = 1)
mtext("Beta", side = 1, line = 2.5, cex = 0.8)


# p3
data.period <- allperiods[which(allperiods$period == p[3]), ] # Choose period
data.period <- data.period[-12, ] # Remove intercept
barCenters <- barplot(data.period$Estimate, 
                      horiz=TRUE, 
                      xlim=c(-5,+5), 
                      ylim = c(0.5,13),
                      xlab=" ", 
                      axes = FALSE,
                      col = data.period$colour)
# arrows(data.period$lower, barCenters, data.period$upper, barCenters, code = 3, angle = 90, length = 0.1) #  For CI but I dont like it, is a mess. Barcenters is what barplot is called
axis(1, cex.axis = 1.2)
mtext(use, side = 2, line = 1, at = barCenters, las = 2, cex = 0.8)
#mtext("Period 3", side = 3, line = 1, cex = 1)
mtext("Beta", side = 1, line = 2.5, cex = 0.8)
dev.off()



