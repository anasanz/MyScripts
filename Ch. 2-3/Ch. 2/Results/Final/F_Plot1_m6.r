
rm(list=ls())


###### PLOT FOR MODEL 6 ######


# Load data


s_good <- c("ALRUF","BUOED","CACAR","COOEN","COPAL","GACRI","GATHE","MEAPI","MECAL","PAMAJ","SESER","STSSP","SYCAN","SYMEL","TERAX","UPEPO",
            "MICAL","HIRUS","PADOM","PIPIC","PAMON", "COMON") # Species in order of appearance in the list

setwd("S:/PhD/Second chapter/Data/Results/TRIM/6temp/Final")
load("spConvergence_light_FINAL.RData")

species[[4]][[1]]

# Create data frame to plot each method

hds <- data.frame(matrix(NA, ncol = 5,nrow = length(s_good)))
colnames(hds) <- c("method", "sp", "est", "lci", "uci")
hds$method <- "hds"
trim <- data.frame(matrix(NA, ncol = 5,nrow = length(s_good)))
colnames(trim) <- c("method", "sp", "est", "lci", "uci")
trim$method <- "trim"


for (i in 1:length(s_good)){
  #HDS
  sum <- data.frame(species[[i]][[2]]) # Here it is wrong when the length of s_good and species differ
  est_HDS <- sum[which(rownames(sum) %in% "bYear.lam"), c(1,3,7)]
  hds[i,2] <- s_good[i]
  hds[i,c(3:5)] <- est_HDS 
  #TRIM
  est_TRIM <- read.csv(paste("res_trim",s_good[i],".csv", sep = ""))
  trim[i,2] <- s_good[i]
  trim[i,c(3:5)] <- est_TRIM[ ,c(2:4)]
}

# Sort table by species alphabetic order
hds <- dplyr::arrange(hds, sp)
trim <- dplyr::arrange(trim, sp)

# Plot:

setwd("S:/PhD/Second chapter/Data/Results/Final")
pdf("Fig1.pdf",6.2,6)
par(mfrow = c(2,2),
    mar = c(2.5,0.1,2,0.1),
    oma = c(2,0.1,2,0.1)
)

# RESULTS for 1:7 species:

plot(511, xlim = c(-0.7, 1), ylim = c(0,21), xlab = "Estimate", ylab = "Species", axes = FALSE)
axis(1, at = c(-0.5, -0.2, 0.2, 0.5), labels = c("-0.5", "-0.2", "0.2", "0.5"), cex.axis = 0.9)
abline(v = 0)
y <- sort(seq(1:21), decreasing = TRUE)

y_hds <- c(20,17,14,11,8,5,2)
y_trim <- y_hds-1
y_sp <- y_trim + 0.5

# Subset species
hds_sub <- hds[1:7, ]
trim_sub <- trim[1:7, ]

# Significance 

ci_hds <- as.matrix(hds_sub[,c(4,5)])
is_hds_sig <- as.numeric(cbind( ci_hds[,1] <= 0 & ci_hds[,2] >= 0 ))
hds_sub$pch <- ifelse(is_hds_sig == 0,19,21)

ci_trim <- as.matrix(trim_sub[,c(4,5)])
is_trim_sig <- as.numeric(cbind( ci_trim[,1] <= 0 & ci_trim[,2] >= 0 ))
trim_sub$pch <- ifelse(is_trim_sig == 0,19,21)

arrows(hds_sub$lci, y_hds, hds_sub$uci, y_hds, code = 3, angle = 90, length = 0.02, col = "black")
points(hds_sub$est, y_hds, pch = hds_sub$pch , col = "black", cex = 0.8, bg = "white")

arrows(trim_sub$lci, y_trim, trim_sub$uci, y_trim, code = 3, angle = 90, length = 0.02, col = "red")
points(trim_sub$est, y_trim, pch = trim_sub$pch, cex = 0.8, bg = "white", col = "red")

# Add * if significance from trim result is different from hds result

txt <- ifelse(is_hds_sig == is_trim_sig, hds$sp[1:7], paste(hds$sp[1:7], "*", sep = ""))
text(0.75,y_sp,labels = txt, cex = 0.9)



# RESULTS for 8:14 species:

plot(511, xlim = c(-0.7, 1), ylim = c(0,21), xlab = "Estimate", ylab = "Species", axes = FALSE)
mtext("Trend coefficient", side = 1, line = 2.5, cex = 0.8, at = c(0))
axis(1, at = c(-0.5, -0.2, 0.2, 0.5), labels = c("-0.5", "-0.2", "0.2", "0.5"), cex.axis = 0.9)
abline(v = 0)
y <- sort(seq(1:21), decreasing = TRUE)

y_hds <- c(20,17,14,11,8,5,2)
y_trim <- y_hds-1
y_sp <- y_trim + 0.5

# Subset species
hds_sub <- hds[8:14, ]
trim_sub <- trim[8:14, ]

# Significance 

ci_hds <- as.matrix(hds_sub[,c(4,5)])
is_hds_sig <- as.numeric(cbind( ci_hds[,1] <= 0 & ci_hds[,2] >= 0 ))
hds_sub$pch <- ifelse(is_hds_sig == 0,19,21)

ci_trim <- as.matrix(trim_sub[,c(4,5)])
is_trim_sig <- as.numeric(cbind( ci_trim[,1] <= 0 & ci_trim[,2] >= 0 ))
trim_sub$pch <- ifelse(is_trim_sig == 0,19,21)

arrows(hds_sub$lci, y_hds, hds_sub$uci, y_hds, code = 3, angle = 90, length = 0.02, col = "black")
points(hds_sub$est, y_hds, pch = hds_sub$pch , col = "black", cex = 0.8, bg = "white")

arrows(trim_sub$lci, y_trim, trim_sub$uci, y_trim, code = 3, angle = 90, length = 0.02, col = "red")
points(trim_sub$est, y_trim, pch = trim_sub$pch, cex = 0.8, bg = "white", col = "red")

# Add * if significance from trim result is different from hds result

txt <- ifelse(is_hds_sig == is_trim_sig, hds$sp[8:14], paste(hds$sp[8:14], "*", sep = ""))
text(0.75,y_sp,labels = txt, cex = 0.9)


# RESULTS for 15:22 species:

plot(511, xlim = c(-0.7, 1), ylim = c(0,24), xlab = "Estimate", ylab = "Species", axes = FALSE)
mtext("Trend coefficient", side = 1, line = 2.5, cex = 0.8, at = c(0))
axis(1, at = c(-0.5, -0.2, 0.2, 0.5), labels = c("-0.5", "-0.2", "0.2", "0.5"), cex.axis = 0.9)
abline(v = 0)
y <- sort(seq(1:24), decreasing = TRUE)

y_hds <- c(23,20,17,14,11,8,5,2)

y_trim <- y_hds-1
y_sp <- y_trim + 0.5

# Subset species
hds_sub <- hds[15:22, ]
trim_sub <- trim[15:22, ]

# Significance 

ci_hds <- as.matrix(hds_sub[,c(4,5)])
is_hds_sig <- as.numeric(cbind( ci_hds[,1] <= 0 & ci_hds[,2] >= 0 ))
hds_sub$pch <- ifelse(is_hds_sig == 0,19,21)

ci_trim <- as.matrix(trim_sub[,c(4,5)])
is_trim_sig <- as.numeric(cbind( ci_trim[,1] <= 0 & ci_trim[,2] >= 0 ))
trim_sub$pch <- ifelse(is_trim_sig == 0,19,21)

arrows(hds_sub$lci, y_hds, hds_sub$uci, y_hds, code = 3, angle = 90, length = 0.02, col = "black")
points(hds_sub$est, y_hds, pch = hds_sub$pch , col = "black", cex = 0.8, bg = "white")

arrows(trim_sub$lci, y_trim, trim_sub$uci, y_trim, code = 3, angle = 90, length = 0.02, col = "red")
points(trim_sub$est, y_trim, pch = trim_sub$pch, cex = 0.8, bg = "white", col = "red")

# Add * if significance from trim result is different from hds result

txt <- ifelse(is_hds_sig == is_trim_sig, hds$sp[15:22], paste(hds$sp[15:22], "*", sep = ""))
text(0.75,y_sp,labels = txt, cex = 0.9)




# Legend

s_good <- sort(s_good)

setwd("S:/PhD/Second chapter/Data")
leg <- read.csv("Tespecies.csv", sep = ";")

leg$codiEspecie <- as.character(leg$codiEspecie)
leg$nomActual <- as.character(leg$nomActual)
leg <- leg[which(leg$codiEspecie %in% s_good), c(2,4)]
leg$nomActual[leg$codiEspecie == "CACHL"] <- "Carduelis chloris"
write.csv(leg,"leg_species.csv")


#leg$english <- c("Red-legged partridge", "Stone curlew", "Linnet", "Goldfinch", )

plot(511, xlim = c(-11, 10), ylim = c(0,10), xlab = " ", ylab = " ", axes = FALSE)
text(-10, sort(seq(1:11), decreasing = TRUE), labels = leg$codiEspecie[1:11], cex = 0.7)
text(-9, sort(seq(1:11), decreasing = TRUE), labels = leg$nomActual[1:11], cex = 0.7, pos = 4)

text(1, sort(seq(12:22), decreasing = TRUE), labels = leg$codiEspecie[12:22], cex = 0.7)
text(2, sort(seq(12:22), decreasing = TRUE), labels = leg$nomActual[12:22], cex = 0.7, pos = 4)
par(xpd = TRUE)
legend("bottomleft", inset = c(0, -0.1), legend = c("HDS", "TRIM"), col = c("black", "red"), fill = c("black", "red"), bty = "n", horiz = TRUE)

dev.off()

############################################################################################
###############                     PLOT IN OTHER FORMAT            ########################
############################################################################################

# Plot:

setwd("S:/PhD/Second chapter/Data/Results/Final")
pdf("Fig1_long.pdf",7,7)
par(mfrow = c(1,2),
    mar = c(1.5,0.1,0.5,0.1),
    oma = c(1.5,0.1,1,0.1)
)


# RESULTS for 1:22 species:

plot(511, xlim = c(-0.7, 0.7), ylim = c(0,65), xlab = "Estimate", ylab = "Species", axes = FALSE)
axis(1, at = c(-0.5, -0.2, 0.2, 0.5), labels = c("-0.5", "-0.2", "0.2", "0.5"), cex.axis = 0.9)
abline(v = 0)
y <- sort(seq(1:65), decreasing = TRUE)

y_hds <- c(65,62,59,56,53,50,47,44,41,38,35,32,29,26,23,20,17,14,11,8,5,2)
y_trim <- y_hds-1
y_sp <- y_trim + 0.5

# Subset species (all)
hds_sub <- hds[1:22, ]
trim_sub <- trim[1:22, ]

# Significance 

ci_hds <- as.matrix(hds_sub[,c(4,5)])
is_hds_sig <- as.numeric(cbind( ci_hds[,1] <= 0 & ci_hds[,2] >= 0 ))
hds_sub$pch <- ifelse(is_hds_sig == 0,19,21)

ci_trim <- as.matrix(trim_sub[,c(4,5)])
is_trim_sig <- as.numeric(cbind( ci_trim[,1] <= 0 & ci_trim[,2] >= 0 ))
trim_sub$pch <- ifelse(is_trim_sig == 0,19,21)

arrows(hds_sub$lci, y_hds, hds_sub$uci, y_hds, code = 3, angle = 90, length = 0.02, col = "black")
points(hds_sub$est, y_hds, pch = hds_sub$pch , col = "black", cex = 0.8, bg = "white")

arrows(trim_sub$lci, y_trim, trim_sub$uci, y_trim, code = 3, angle = 90, length = 0.02, col = "red")
points(trim_sub$est, y_trim, pch = trim_sub$pch, cex = 0.8, bg = "white", col = "red")

# Add * if significance from trim result is different from hds result

txt <- ifelse(is_hds_sig == is_trim_sig, hds$sp[1:22], paste(hds$sp[1:22], "*", sep = ""))
text(0.60,y_sp,labels = txt, cex = 0.9)

legend(-0.55, 4, legend = c("HDS", "TRIM"), col = c("black", "red"), fill = c("black", "red"), bty = "n", cex = 0.9)


# LEGEND 

s_good <- sort(s_good)
library(dplyr)
setwd("S:/PhD/Second chapter/Data")
leg <- read.csv("leg_species.csv", sep = ";")
leg <- arrange(leg,codiEspecie)


#leg$english <- c("Red-legged partridge", "Stone curlew", "Linnet", "Goldfinch", )

plot(511, xlim = c(-11, 10), ylim = c(0,65), xlab = " ", ylab = " ", axes = FALSE)
text(-11, y_sp, labels = leg$nomActual[1:22], cex = 0.8, adj = 0, font = 3)
text(-0, y_sp, labels = leg$English[1:22], cex = 0.8, adj = 0)


dev.off()

############################################################################################
###############                     PLOT IN OTHER FORMAT            ########################
############################################################################################

# Plot:

setwd("S:/PhD/Second chapter/Data/Results/Final")
pdf("Fig1_long.pdf",7,7)
par(mfrow = c(1,2),
    mar = c(1.5,0.1,0.5,0.1),
    oma = c(1.5,0.1,1,0.1)
)


# RESULTS for 1:22 species:

plot(511, xlim = c(-0.7, 0.7), ylim = c(0,65), xlab = "Estimate", ylab = "Species", axes = FALSE)
axis(1, at = c(-0.5, -0.2, 0.2, 0.5), labels = c("-0.5", "-0.2", "0.2", "0.5"), cex.axis = 0.9)
abline(v = 0)
y <- sort(seq(1:65), decreasing = TRUE)

y_hds <- c(65,62,59,56,53,50,47,44,41,38,35,32,29,26,23,20,17,14,11,8,5,2)
y_trim <- y_hds-1
y_sp <- y_trim + 0.5

# Subset species (all)
hds_sub <- hds[1:22, ]
trim_sub <- trim[1:22, ]

# Significance 

ci_hds <- as.matrix(hds_sub[,c(4,5)])
is_hds_sig <- as.numeric(cbind( ci_hds[,1] <= 0 & ci_hds[,2] >= 0 ))
hds_sub$pch <- ifelse(is_hds_sig == 0,19,21)

ci_trim <- as.matrix(trim_sub[,c(4,5)])
is_trim_sig <- as.numeric(cbind( ci_trim[,1] <= 0 & ci_trim[,2] >= 0 ))
trim_sub$pch <- ifelse(is_trim_sig == 0,19,21)

arrows(hds_sub$lci, y_hds, hds_sub$uci, y_hds, code = 3, angle = 90, length = 0.02, col = "black")
points(hds_sub$est, y_hds, pch = hds_sub$pch , col = "black", cex = 0.8, bg = "white")

arrows(trim_sub$lci, y_trim, trim_sub$uci, y_trim, code = 3, angle = 90, length = 0.02, col = "red")
points(trim_sub$est, y_trim, pch = trim_sub$pch, cex = 0.8, bg = "white", col = "red")

# Add * if significance from trim result is different from hds result

txt <- ifelse(is_hds_sig == is_trim_sig, hds$sp[1:22], paste(hds$sp[1:22], "*", sep = ""))
text(0.60,y_sp,labels = txt, cex = 0.9)

legend(-0.55, 4, legend = c("HDS", "TRIM"), col = c("black", "red"), fill = c("black", "red"), bty = "n", cex = 0.9)


# LEGEND 

s_good <- sort(s_good)
library(dplyr)
setwd("S:/PhD/Second chapter/Data")
leg <- read.csv("leg_species.csv", sep = ";")
leg <- arrange(leg,codiEspecie)


#leg$english <- c("Red-legged partridge", "Stone curlew", "Linnet", "Goldfinch", )

plot(511, xlim = c(-11, 10), ylim = c(0,65), xlab = " ", ylab = " ", axes = FALSE)
text(-11, y_sp, labels = leg$nomActual[1:22], cex = 0.8, adj = 0, font = 3)
text(-0, y_sp, labels = leg$English[1:22], cex = 0.8, adj = 0)


dev.off()

############################################################################################
###############                     PLOT IN OTHER (LAST) FORMAT            ########################
############################################################################################

# Plot:

setwd("S:/PhD/Second chapter/Data/Results/Final")
pdf("Fig1_long2.pdf",7,7)
par(mfrow = c(1,1),
    mar = c(1.5,0.1,0.5,0.1),
    oma = c(1.5,0.1,1,0.1)
)

# Order the HDS table ascending
hds_as <- arrange(hds,est)
# Order trim as hds (join tables)
order_hds <- left_join(hds_as,trim, by = "sp")
trim_as <- order_hds[ ,c(6,2,7:9)]
colnames(trim_as) <- colnames(hds_as)

# RESULTS for 1:22 species:

plot(511, xlim = c(-0.7, 3), ylim = c(0,65), xlab = "Estimate", ylab = "Species", axes = FALSE)
axis(1, at = c(-0.5, -0.2, 0.2, 0.5), labels = c("-0.5", "-0.2", "0.2", "0.5"), cex.axis = 0.9)
abline(v = 0)
y <- sort(seq(1:65), decreasing = TRUE)

y_hds <- c(65,62,59,56,53,50,47,44,41,38,35,32,29,26,23,20,17,14,11,8,5,2)
y_trim <- y_hds-1
y_sp <- y_trim + 0.5

# Subset species (all)
hds_sub <- hds_as[1:22, ]
trim_sub <- trim_as[1:22, ]

# Significance 

ci_hds <- as.matrix(hds_sub[,c(4,5)])
is_hds_sig <- as.numeric(cbind( ci_hds[,1] <= 0 & ci_hds[,2] >= 0 ))
hds_sub$pch <- ifelse(is_hds_sig == 0,19,21)

ci_trim <- as.matrix(trim_sub[,c(4,5)])
is_trim_sig <- as.numeric(cbind( ci_trim[,1] <= 0 & ci_trim[,2] >= 0 ))
trim_sub$pch <- ifelse(is_trim_sig == 0,19,21)

arrows(hds_sub$lci, y_hds, hds_sub$uci, y_hds, code = 3, angle = 90, length = 0.02, col = "black")
points(hds_sub$est, y_hds, pch = hds_sub$pch , col = "black", cex = 0.6, bg = "white")

arrows(trim_sub$lci, y_trim, trim_sub$uci, y_trim, code = 3, angle = 90, length = 0.02, col = "red")
points(trim_sub$est, y_trim, pch = trim_sub$pch, cex = 0.6, bg = "white", col = "red")

# Text: Add legend

s_good <- sort(s_good)
library(dplyr)
setwd("S:/PhD/Second chapter/Data")
leg <- read.csv("leg_species.csv", sep = ";")
leg <- arrange(leg,codiEspecie)
colnames(leg)[1] <- "sp"

order_hds_leg <- left_join(order_hds, leg, by = "sp")
leg_order <- order_hds_leg[ ,c(10, 11)]
leg_order$nomActual <- as.character(leg_order$nomActual)
leg_order$English <- as.character(leg_order$English)
hds_as <- cbind(hds_as, leg_order)

# Add * if significance from trim result is different from hds result

txt <- ifelse(is_hds_sig == is_trim_sig, hds_as$English[1:22], paste("*", hds_as$English[1:22], sep = ""))
text(0.55,y_sp,labels = txt, cex = 0.8, adj = 0)
text(1.45,y_sp,labels = hds_as$nomActual[1:22], cex = 0.8, adj = 0, font = 3)

#legend(-0.55, 9, legend = c("HDS", "TRIM"), col = c("black", "red"), fill = c("black", "red"), bty = "n", cex = 0.9)

par(xpd = NA)
legend("bottomright", inset = c(+0.5, -0.06), legend = c("HDS", "TRIM"), col = c("black", "red"), fill = c("black", "red"), bty = "n", horiz = FALSE, cex = 0.9)



dev.off()


