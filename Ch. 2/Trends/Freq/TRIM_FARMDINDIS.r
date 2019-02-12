

rm(list=ls())

library(rtrim)

setwd("C:/Users/ana.sanz/OneDrive/PhD/Second chapter/TRIM/Dades")

ambSG <- read.csv("Ambit_SG.csv", header = TRUE, sep = ";")
zepaSG <- read.csv("ZEPA_SG.csv", header = TRUE, sep = ";")
occSG <- read.csv("ZEPA_occidentals.csv", header = TRUE, sep = ";")
oriSG <- read.csv("ZEPA_orientals.csv", header = TRUE, sep = ";")

count_data <- rbind(ambSG, zepaSG, occSG, oriSG)

sp <- unique(count_data$sp)
ambit <- unique(count_data$id_ambit)

m <- as.data.frame(matrix(NA, ncol = 13, nrow = 9)) # nrow es numero de años * numero de zonas en la que quiero estimar tendencias (para una especie)
colnames(m) <- c("sp", "sector", "year", "index", "index_se", "IC95+", "IC95-", "coef_trend", "coef_trend_se", "ptrend", "pgof", "annual_trend", "description")


#TRIM ALRUF----


ALRUF<- subset(ambSG, sp == "ALRUF")
m$sp <- "ALRUF"
m2 <- trim(count ~ site + year, data = ALRUF, model = 2, serialcor = TRUE, overdisp = TRUE)
i2 <- index(m2, which="imputed")



# summarize the model
summary(m2)

#return time totals
totals(m1)

wald(m1)

#Retrieve goodness-of-fit

gof(m1)

#Extract the coefficients

coefficients(m3)
index(m3, which="both")

#Plot with overall slope

plot(overall(m1))

plot(i1)

# tendencia anual: (Mulplicative - 1)*100
(0.803705 - 1)*100



