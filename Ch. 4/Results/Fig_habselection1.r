

## -------------------------------------------------
##        FIGURE HABITAT SELECTION ANALISIS 1
## ------------------------------------------------- 


rm(list=ls())

library(dplyr)

setwd("D:/PhD/Fourth chapter/Results/RSF_habitatsel3/othersec_bootstrap_IDyear")
p1 <- read.csv("RSF.results.pre.othersec.IDyear.csv", sep = ",")
p1 <- p1[,-5]
p1$period <- "p1"
p1 <- p1[nrow(p1):1, ]
p1$X <- c("DistAsp", "Slope", "DistGrav", "NatVeg", "Forest", "Herb.irri", "Fruit.irri", "Almond", "Olive", "Cereal", "Fallow", "Intercept")

p2 <- read.csv("RSF.results.prerep.othersec.IDyear.csv", sep = ",")
p2 <- p2[,-5]
p2$period <- "p2"
p2 <- p2[nrow(p2):1, ]
p2$X <- c("DistAsp", "Slope", "DistGrav", "NatVeg", "Forest", "Herb.irri", "Fruit.irri", "Almond", "Olive", "Cereal", "Fallow", "Intercept")
p2[6,2] <- p2[5,2]-0.2 # Put the same value than for forest, because it doesnt fit (dashed line and explain in legend)

p3 <- read.csv("RSF.results.rep.othersec.IDyear.csv", sep = ",")
p3 <- p3[,-5]
p3$period <- "p3"
p3 <- p3[nrow(p3):1, ]
p3$X <- c("DistAsp", "Slope", "DistGrav", "NatVeg", "Forest", "Herb.irri", "Fruit.irri", "Almond", "Olive", "Cereal", "Fallow", "Intercept")


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
    allperiods$colour[i] <- ifelse (allperiods$Estimate[i] > 0, "blue", "red") 
  }
}

use <- c("DistAsp", "Slope", "DistGrav", "NatVeg", "Forest", "Herb.irri", "Fruit.irri", "Almond", "Olive", "Cereal", "Fallow")

# Plot

setwd("D:/PhD/Fourth chapter/Results/Figures")
pdf("Fig1.pdf", 8,5)

par(mfrow = c(1,3),
    mar = c(4,0,2,0), 
    oma = c(2,7,2,1))

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
mtext(use, side = 2, line = 1, at = barCenters, las = 2)
mtext("Period 1", side = 3, line = 1, cex = 1.5)
mtext("Beta", side = 1, line = 3, cex = 1)


# p2
data.period <- allperiods[which(allperiods$period == p[2]), ] # Choose period
data.period <- data.period[-12, ] # Remove intercept
barCenters <- barplot(data.period$Estimate, 
                      horiz=TRUE, 
                      xlim=c(-7,+7), 
                      ylim = c(0.5,13),
                      xlab=" ", 
                      axes = FALSE,
                      col = data.period$colour)
segments(x0 = -6.995, y0 = 6, x1 = -6.995, y1 = 7.5, lty = 2, col = "white")
# arrows(data.period$lower, barCenters, data.period$upper, barCenters, code = 3, angle = 90, length = 0.1) #  For CI but I dont like it, is a mess. Barcenters is what barplot is called
axis(1, cex.axis = 1.2)
mtext("Period 2", side = 3, line = 1, cex = 1.5)
mtext("Beta", side = 1, line = 3, cex = 1)


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
mtext("Period 3", side = 3, line = 1, cex = 1.5)
mtext("Beta", side = 1, line = 3, cex = 1)
dev.off()


## -------------------------------------------------
##                FIGURE LONG FORMAT
## ------------------------------------------------- 

setwd("D:/PhD/Fourth chapter/Results/Figures")
pdf("Fig1_vert.pdf", 4,10)

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
barCenters <- barplot(data.period$Estimate, 
                      horiz=TRUE, 
                      xlim=c(-7,+7), 
                      ylim = c(0.5,13),
                      xlab=" ", 
                      axes = FALSE,
                      col = data.period$colour)
segments(x0 = -6.995, y0 = 6, x1 = -6.995, y1 = 7.5, lty = 2, col = "white")
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



