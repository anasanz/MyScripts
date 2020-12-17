## -------------------------------------------------
##    ALL FIGURES HABITAT SELECTION ANALISIS 1 
##          -> DIFFERENT AVAILAB
## ------------------------------------------------- 


rm(list=ls())

library(dplyr)

period <- c("Pre", "PreRep", "Rep")
st <- c("St1", "St2") 

temp <- setwd("D:/PhD/Fourth chapter/Results/RSF_Ana/Analisis 2/results_prov_rspf/For plotting")
temp <- list.files(pattern = "*.csv")
data <- lapply(temp, read.csv, sep = ";")

xxx = 1
file <- grep(st[xxx], temp) # Files with one type of availability sampling
d <- data[file]
d_names <- temp[file]

p1 <- d[[1]]
p1 <- p1[,-5]
p1$period <- "p1"
p1 <- p1[nrow(p1):1, ]
p1[,1] <- c("DistLin", "Slope", "Manag", "No.Manag", "Intercept")

p2 <- d[[2]]
p2 <- p2[,-5]
p2$period <- "p2"
p2 <- p2[nrow(p2):1, ]
p2[,1] <- c("DistLin", "Slope", "Manag", "No.Manag", "Intercept")

p3 <- d[[3]]
p3 <- p3[,-5]
p3$period <- "p3"
p3 <- p3[nrow(p3):1, ]
p3[,1] <- c("DistLin", "Slope", "Manag", "No.Manag", "Intercept")


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

use <- c("DistLin", "Slope", "Manag", "No.Manag")

# Plot

setwd("D:/PhD/Fourth chapter/Results/Figures/Fig3_fallow")
pdf(paste("Fig3_", st[xxx], ".pdf", sep = ""), 7,4)

par(mfrow = c(1,3),
    mar = c(4,3,2,2), 
    oma = c(2,7,2,1))

# p1
data.period <- allperiods[which(allperiods$period == p[1]), ] # Choose period
data.period <- data.period[-5, ] # Remove intercept
barCenters <- barplot(data.period$Estimate, 
                      horiz=TRUE, 
                      xlim=c(-2.1,+1), 
                      ylim = c(0.5,5),
                      xlab=" ", 
                      axes = FALSE,
                      col = data.period$colour)
#arrows(data.period$lower.se, barCenters, data.period$upper.se, barCenters, code = 3, angle = 90, length = 0.1) #  For CI but I dont like it, is a mess. Barcenters is what barplot is called
axis(1, cex.axis = 1.2, pos = 0)
mtext(use, side = 2, line = 1, at = barCenters, las = 2, cex = 1.1)
mtext("Pre-Bottleneck", side = 3, line = 1, cex = 1.2)
mtext("Beta", side = 1, line = 3.7, cex = 1)


# p2
data.period <- allperiods[which(allperiods$period == p[2]), ] # Choose period
data.period <- data.period[-5, ] # Remove intercept
barCenters <- barplot(data.period$Estimate, 
                      horiz=TRUE, 
                      xlim=c(-2.1,+1), 
                      ylim = c(0.5,5),
                      xlab=" ", 
                      axes = FALSE,
                      col = data.period$colour)
# arrows(data.period$lower, barCenters, data.period$upper, barCenters, code = 3, angle = 90, length = 0.1) #  For CI but I dont like it, is a mess. Barcenters is what barplot is called
axis(1, cex.axis = 1.2, pos = 0)
mtext("Bottleneck", side = 3, line = 1, cex = 1.2)
mtext("Beta", side = 1, line = 3.7, cex = 1)


# p3
data.period <- allperiods[which(allperiods$period == p[3]), ] # Choose period
data.period <- data.period[-5, ] # Remove intercept
barCenters <- barplot(data.period$Estimate, 
                      horiz=TRUE, 
                      xlim=c(-2.1,+1), 
                      ylim = c(0.5,5),
                      xlab=" ", 
                      axes = FALSE,
                      col = data.period$colour)
# arrows(data.period$lower, barCenters, data.period$upper, barCenters, code = 3, angle = 90, length = 0.1) #  For CI but I dont like it, is a mess. Barcenters is what barplot is called
axis(1, cex.axis = 1.2, pos = 0)
mtext("Post-Bottleneck", side = 3, line = 1, cex = 1.2)
mtext("Beta", side = 1, line = 3.7, cex = 1)
dev.off()


