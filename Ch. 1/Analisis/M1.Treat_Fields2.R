
# METHODS: Field distribution years - Treatments
# Modified with the 7 fields removed when repeating the analysis in the 2nd submission

setwd("C:/Users/ana.sanz/OneDrive/PhD/First chapter/Datos/Datos barbechos arrendados/Variables")
#setwd("C:/Users/Ana/Documents/PhD/Datos/Datos barbechos arrendados/Variables")


f <- read.csv("Data_path_manuscript2.csv", sep = ",", header=TRUE, fill = TRUE)
f <- f[which(f$Species == "SC"), ]

#Select 
f <- f[ , which(colnames(f) %in% c("Codi_Finca", "Year", "agri_practice"))]
f$agri_practice <- as.character(f$agri_practice)
f$Codi_Finca <- as.character(f$Codi_Finca)
f$Year <- as.character(f$Year)

#Frequency
freq <- xtabs(  ~ agri_practice + Year, data = f)




# APPENDIX: Vegetation- Treatments

setwd("C:/Users/ana.sanz/OneDrive/PhD/First chapter/Datos/Datos barbechos arrendados/Variables")

f <- read.csv("Data_path_manuscript2.csv", sep = ",", header=TRUE, fill = TRUE)
f <- f[which(f$Species == "SC"), ]

#Select 
f <- f[ , which(colnames(f) %in% c("CF_A", "agri_practice", "Height", "Cover", "Cover_dead"))]

#Values mean + se
m <- aggregate(f[ ,c(3:5)], list(f$agri_practice), mean)
m_round <- round(m[, c(2:4)], digits = 2)

sd <- aggregate(f[ ,c(3:5)], list(f$agri_practice), sd) 
sd_round <- round(sd [ ,c(2:4)], digits = 2)

tab<-matrix(ncol = dim(m_round)[2], nrow = dim(m_round)[1])
for (i in 1:nrow(m_round)){
  pas<-paste(m_round[i, ], sd_round[i, ], sep=" +/- ")
  tab[i,]<-pas }

tab <- as.data.frame(tab)
colnames(tab) <- c("Cover", "Cover_dead", "Height")
tab$Treatment <- m$Group.1

setwd("C:/Users/ana.sanz/OneDrive/PhD/First chapter/RESULTS")
write.csv(tab, "Av_height2.csv")

# Min and max

min <- aggregate(f[ ,c(1:3)], list(f$agri_practice), min)
min_round <- round(min [, c(2:4)], digits = 2)

max <- aggregate(f[ ,c(1:3)], list(f$agri_practice), max)
max_round <- round(max [, c(2:4)], digits = 2)

maxmin<-matrix(ncol = dim(min_round)[2], nrow = dim(min_round)[1])
for (i in 1:nrow(min_round)){
  pas<-paste(min_round[i, ], max_round[i, ], sep=" - ")
  maxmin[i,]<-pas }

maxmin <- as.data.frame(maxmin)
colnames(maxmin) <- c("Cover", "Cover_dead", "Height")
maxmin$Treatment <- m$Group.1

write.csv(maxmin, "Maxmin_height2.csv")

# Data for the main text: ha covered
total_area <- sum(f$area)
ha <- total_area/10000 #Total area covered

mean_area_ha <- mean(f$area)/10000 # Average area of FF
sd_area_ha <- sqrt(var(f$area))/10000

# Check FF that were repeated accross years
f$Codi_Finca <- as.character(f$Codi_Finca)
f15 <- f[which(f$Year == 2015), 2] 
f16 <- f[which(f$Year == 2016), 2] 
f17 <- f[which(f$Year == 2017), 2] 

rep <- Reduce(intersect, list(f15, f16, f17))

