
# METHODS: Field distribution years - Treatments

setwd("C:/Users/ana.sanz/Documents/Datos/Datos barbechos arrendados/Variables")
setwd("C:/Users/Ana/Documents/PhD/Datos/Datos barbechos arrendados/Variables")

f <- read.csv("Variables.csv", sep = ",", header=TRUE, fill = TRUE)

f <- f[which(f$especieObjectiu == "BUOED"), ]
f[f == 99.00] <- NA
f <- f[-which(duplicated(f[ , 2:15])), ]
f <- f[which(complete.cases(f$Contatge)), ]

#Select 
f <- f[ , which(colnames(f) %in% c("Codi_Finca", "Any", "Tractament"))]
f$Tractament <- as.character(f$Tractament)
f$Codi_Finca <- as.character(f$Codi_Finca)
f$Any <- as.character(f$Any)
f$Tractament[which(f$Tractament == "Curronar")] <- "Llaurar" 
#Frequency
freq <- xtabs(  ~ Tractament + Any, data = f)




# APPENDIX: Vegetation- Treatments

setwd("C:/Users/ana.sanz/Documents/Datos/Datos barbechos arrendados/Variables")

f <- read.csv("Variables.csv", sep = ",", header=TRUE, fill = TRUE)

f <- f[which(f$especieObjectiu == "BUOED"), ]
f[f == 99.00] <- NA
f <- f[-which(duplicated(f[ , 2:15])), ]
f <- f[which(complete.cases(f$Contatge)), ]

#Select 
f <- f[ , which(colnames(f) %in% c("Tractament", "PromigAltura1Plot", "Recob_plotViu", "Recob_plotMort"))]
f$Tractament <- as.character(f$Tractament)
f$Tractament[which(f$Tractament == "Curronar")] <- "Llaurar" 
#Values
m <- aggregate(f[ ,c(1:3)], list(f$Tractament), mean)
m <- round(m [, c(2:4)], digits = 2)
sd <- aggregate(f[ ,c(1:3)], list(f$Tractament), sd) 
sd <- round(sd [, c(2:4)], digits = 2)


