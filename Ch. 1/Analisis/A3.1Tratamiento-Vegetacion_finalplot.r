
# Plot agripractice - vegetation

setwd("C:/Users/Ana/Documents/PhD/First chapter/Datos/Datos barbechos arrendados/Variables")

f<-read.csv("Data_path_submission.csv",sep = ",",header=TRUE,fill = TRUE)
f<-f[which(f$Species == "SC"),]

f$agri_practice <- as.character(f$agri_practice)

f$agri_practice[which(f$agri_practice == "C")]<-"Control"
f$agri_practice[which(f$agri_practice == "A")]<-"Alfalfa"
f$agri_practice[which(f$agri_practice == "T")]<-"Tillage"
f$agri_practice[which(f$agri_practice == "S")]<-"Shredding"
f$agri_practice[which(f$agri_practice == "S+H")]<-"Shred+Herb"

setwd("C:/Users/Ana/Documents/PhD/First chapter/METHODS")

pdf(file="Veg_agriprac.pdf", width=6, height = 6.5)

par(mfrow = c(3,1),
    oma = c(3,3,4,1) + 0.1,
    mar = c(2,2,2,0) + 0.1)

boxplot(Cover ~ agri_practice, f)
mtext("% Cover",side=2,line=3, cex = 0.9)
boxplot(Cover_dead ~ agri_practice, f) 
mtext("% Dead cover",side=2,line=3, cex = 0.9)
boxplot(Height ~ agri_practice, f)
mtext("Height",side=2,line=3, cex = 0.9)

mtext("Agricultural practice",side=1,line=1,outer = TRUE,cex = 0.9)

dev.off()
