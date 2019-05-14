
rm(list=ls())

###### EXAMPLE ######
# Load data
setwd("S:/PhD/Second chapter/Data/Results/TRIM/3.2autoreg_simple")
load("spConvergence_light.RData")

setwd("S:/PhD/Second chapter/Data/Results/TRIM/3.2autoreg_simple")

# Create data frame to plot each method
hds <- data.frame(matrix(NA,ncol = 5,nrow = length(s_good)))
colnames(hds) <- c("method", "sp", "est", "lci", "uci")
hds$method <- "hds"
trim <- data.frame(matrix(NA, ncol = 5,nrow = length(s_good)))
colnames(trim) <- c("method", "sp", "est", "lci", "uci")
trim$method <- "trim"

for (i in 1:length(s_good)){
  #HDS
  sum <- data.frame(species[[i]][[2]]) 
  est_HDS <- sum[which(rownames(sum) %in% "bYear.lam"), c(1,3,7)]
  hds[i,2] <- s_good[i]
  hds[i,c(3:5)] <- est_HDS 
  #TRIM
  est_TRIM <- read.csv(paste("res_trim",s_good[i],".csv", sep = ""))
  trim[i,2] <- s_good[i]
  trim[i,c(3:5)] <- est_TRIM[ ,c(2:4)]
}

# Plot all species: TRY EXAMPLE

setwd("S:/PhD/Second chapter/Data/Results/Plots/5allcov")
pdf("prueba.pdf")
par(mfrow = c(2,2),
    mar = c(2,2,2,2),
    oma = c(2,2,2,2)
    )

# RESULTS for 1:10 species:

plot(511, xlim = c(-1.5, 2), ylim = c(0,30), xlab = "Estimate", ylab = "Species", axes = FALSE)
axis(1, at = c(-1.5, -0.5, 0.5, 1.5, 2), labels = c("-1.5", "-0.5", "0.5", "1.5", ""), cex = 0.8)
abline(v = 0)
y <- sort(seq(1:30), decreasing = TRUE)

y_hds <- c(29,26,23,20,17,14,11,8,5,2)
y_trim <- y_hds-1
y_sp <- y_trim + 0.5

#ONLY FOR TRYING:
hds <- data.frame(method = rep("hds", 10), sp = s_good[1:10], est = c(rep(-1, 5), rep(-0.2,5)), lci = rep(-0.5, 10), uci = c(rep(-1.5, 5), rep(0.2,5)))
trim <- data.frame(method = rep("hds", 10), sp = s_good[1:10], est = rep(-1, 10), lci = rep(-0.5, 10), uci = rep(-1.5, 10))

# Significance (19 significant; 1 not significant)

ci_hds <- as.matrix(hds[,c(4,5)])
is_hds_sig <- as.numeric(cbind( ci_hds[,1] <= 0 & ci_hds[,2] >= 0 ))
hds$pch <- ifelse(is_hds_sig == 0,19,21)

ci_trim <- as.matrix(trim[,c(4,5)])
is_trim_sig <- as.numeric(cbind( ci_trim[,1] <= 0 & ci_trim[,2] >= 0 ))
trim$pch <- ifelse(is_trim_sig == 0,19,21)

arrows(hds$lci, y_hds, hds$uci, y_hds, code = 3, angle = 90, length = 0.02, col = "black")
points(hds$est, y_hds, pch = hds$pch , col = "black", cex = 0.8, bg = "white")

arrows(trim$lci, y_trim, trim$uci, y_trim, code = 3, angle = 90, length = 0.02, col = "red")
points(trim$est, y_trim, pch = trim$pch, cex = 0.8, bg = "white", col = "red")

# Add * if significance from trim result is different from hds result

txt <- ifelse(is_hds_sig == is_trim_sig, s_good[1:10], paste(s_good[1:10], "*", sep = ""))
text(1.5,y_sp,labels = txt, cex = 0.9)


# Legend
setwd("S:/PhD/Second chapter/Data")
leg <- read.csv("Tespecies.csv", sep = ";")
leg$codiEspecie <- as.character(leg$codiEspecie)
leg$nomActual <- as.character(leg$nomActual)
leg <- leg[which(leg$codiEspecie %in% s_good), c(2,4)]
leg$nomActual[leg$codiEspecie == "CACHL"] <- "Carduelis chloris"
#leg$english <- c("Red-legged partridge", "Stone curlew", "Linnet", "Goldfinch", )

dev.off()


###### PLOT FOR MODEL 3.2 ######

# For legend:

setwd("S:/PhD/Second chapter/Data")
s <- read.csv("sp_trend_dg.csv", sep = ";")
s_good <- as.vector(s$Species[which(s$include_samplesize == 1)])
remove_3.2 <- c("CACHL", "CAINA", "CIJUN", "COCOT", "COLIV", "LUARB","LUMEG","MIMIG", "OEHIS","ORORI","PIVIR", "PYRAX","SESER","STUNI", "STVUL","TUMER", "TUVIS")
s_good <- s_good[-which(s_good %in% remove_3.2)] # SPECIES THAT CONVERGE FOR MODEL 3.2

# Load data

setwd("S:/PhD/Second chapter/Data/Results/TRIM/3.2autoreg_simple")
load("spConvergence_light.RData")

# Make s_good with the same order than species[[]]
# 1. Remove from species the ones that dont converge to make it 26
species <- species[-c(4, 5, 19, 24, 25)]

# Create data frame to plot each method

hds <- data.frame(matrix(NA,ncol = 5,nrow = length(s_good)))
colnames(hds) <- c("method", "sp", "est", "lci", "uci")
hds$method <- "hds"
trim <- data.frame(matrix(NA, ncol = 5,nrow = length(s_good)))
colnames(trim) <- c("method", "sp", "est", "lci", "uci")
trim$method <- "trim"

for (i in 1:length(s_good)){
  #HDS
  sum <- data.frame(species[[i]][[2]]) 
  est_HDS <- sum[which(rownames(sum) %in% "bYear.lam"), c(1,3,7)]
  hds[i,2] <- s_good[i]
  hds[i,c(3:5)] <- est_HDS 
  #TRIM
  est_TRIM <- read.csv(paste("res_trim",s_good[i],".csv", sep = ""))
  trim[i,2] <- s_good[i]
  trim[i,c(3:5)] <- est_TRIM[ ,c(2:4)]
}




# Plot:

setwd("S:/PhD/Second chapter/Data/Results/Plots/3.2autoreg_simple")
pdf("3.2.pdf")
par(mfrow = c(2,2),
    mar = c(2,1,2,1),
    oma = c(2,1,2,1)
)

# RESULTS for 1:10 species:

plot(511, xlim = c(-0.7, 1), ylim = c(0,30), xlab = "Estimate", ylab = "Species", axes = FALSE)
axis(1, at = c(-0.5, -0.2, 0.2, 0.5), labels = c("-0.5", "-0.2", "0.2", "0.5"), cex = 0.8)
abline(v = 0)
y <- sort(seq(1:30), decreasing = TRUE)

y_hds <- c(29,26,23,20,17,14,11,8,5,2)
y_trim <- y_hds-1
y_sp <- y_trim + 0.5

# Subset species
hds_sub <- hds[1:10, ]
trim_sub <- trim[1:10, ]

# Significance (19 significant; 1 not significant)

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

txt <- ifelse(is_hds_sig == is_trim_sig, s_good[1:10], paste(s_good[1:10], "*", sep = ""))
text(0.75,y_sp,labels = txt, cex = 0.9)



# RESULTS for 11:20 species:

plot(511, xlim = c(-0.7, 1), ylim = c(0,30), xlab = "Estimate", ylab = "Species", axes = FALSE)
axis(1, at = c(-0.5, -0.2, 0.2, 0.5), labels = c("-0.5", "-0.2", "0.2", "0.5"), cex = 0.8)
abline(v = 0)
y <- sort(seq(1:30), decreasing = TRUE)

y_hds <- c(29,26,23,20,17,14,11,8,5,2)
y_trim <- y_hds-1
y_sp <- y_trim + 0.5

# Subset species
hds_sub <- hds[11:20, ]
trim_sub <- trim[11:20, ]

# Significance (19 significant; 1 not significant)

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

txt <- ifelse(is_hds_sig == is_trim_sig, s_good[11:20], paste(s_good[11:20], "*", sep = ""))
text(0.75,y_sp,labels = txt, cex = 0.9)


# RESULTS for 21:25 species:

plot(511, xlim = c(-0.7, 1), ylim = c(0,30), xlab = "Estimate", ylab = "Species", axes = FALSE)
axis(1, at = c(-0.5, -0.2, 0.2, 0.5), labels = c("-0.5", "-0.2", "0.2", "0.5"), cex = 0.8)
abline(v = 0)
y <- sort(seq(1:30), decreasing = TRUE)

#y_hds <- c(29,26,23,20,17,14,11,8,5,2)
y_hds <- c(29,26,23,20,17)
y_trim <- y_hds-1
y_sp <- y_trim + 0.5

# Subset species
hds_sub <- hds[21:25, ]
trim_sub <- trim[21:25, ]

# Significance (19 significant; 1 not significant)

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

txt <- ifelse(is_hds_sig == is_trim_sig, s_good[21:25], paste(s_good[21:25], "*", sep = ""))
text(0.75,y_sp,labels = txt, cex = 0.9)


# Legend
setwd("S:/PhD/Second chapter/Data")
leg <- read.csv("Tespecies.csv", sep = ";")
leg$codiEspecie <- as.character(leg$codiEspecie)
leg$nomActual <- as.character(leg$nomActual)
leg <- leg[which(leg$codiEspecie %in% s_good), c(2,4)]

#leg$english <- c("Red-legged partridge", "Stone curlew", "Linnet", "Goldfinch", )

plot(511, xlim = c(-11, 10), ylim = c(0,15), xlab = " ", ylab = " ", axes = FALSE)
text(-10, sort(seq(1:15), decreasing = TRUE), labels = leg$codiEspecie[1:15], cex = 0.7)
text(-9, sort(seq(1:15), decreasing = TRUE), labels = leg$nomActual[1:15], cex = 0.7, pos = 4)

text(1, sort(seq(1:15), decreasing = TRUE)[-c(11:15)], labels = leg$codiEspecie[16:25], cex = 0.7)
text(2, sort(seq(1:15), decreasing = TRUE)[-c(11:15)], labels = leg$nomActual[16:25], cex = 0.7, pos = 4)
par(xpd = TRUE)
legend("bottomleft", inset = c(0, -0.1), legend = c("HDS", "TRIM"), col = c("black", "red"), fill = c("black", "red"), bty = "n", horiz = TRUE)

dev.off()












