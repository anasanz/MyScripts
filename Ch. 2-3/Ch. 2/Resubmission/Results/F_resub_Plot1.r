
rm(list=ls())

library(dplyr)

###### PLOT FOR MODEL 6 ######


# Load data


s_good <- c("SYCAN", "CACHL", "LASEN", "ALRUF", "PAMAJ", "ALARV", "CABRA", "CACAR", "COPAL", "PIPIC",
            "COOEN", "HIRUS", "PYRAX", "CAINA", "COMON", "PADOM", "PAMON", "FATIN", "SESER", "TUMER", "GATHE",
            "BUOED", "SYMEL", "UPEPO", "GACRI", "STSSP", "MICAL", "MEAPI", "TERAX", "MECAL") # Species in order of appearance in the list

setwd("D:/PhD/Second chapter/Resubmission/Results/Compiled_FINAL")
load("spConvergence_light_resub.RData")

for (i in 1:length(s_good)){ # to check that s_good and species are in the same order
  print(species[[i]][[1]])
}


# Create data frame to plot each method

hds <- data.frame(matrix(NA,ncol = 5,nrow = length(s_good)))
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

# Remove species with bad Bayesian p-values

bad_bp <- c("GACRI", "STSSP", "MICAL", "TERAX", "MEAPI", "MECAL")
hds <- hds[-which(hds$sp %in% bad_bp), ]
trim <- trim[-which(trim$sp %in% bad_bp), ]


############################################################################################
###############                     PLOT IN OTHER (LAST) FORMAT            ########################
############################################################################################

# Plot:

setwd("D:/PhD/Second chapter/Resubmission/Results/Final")

pdf("Fig1_noBP2.pdf",6,8)
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

# RESULTS for 1:30 species:

plot(511, xlim = c(-0.7, 3), ylim = c(0,71), xlab = "Estimate", ylab = "Species", axes = FALSE)
axis(1, at = c(-0.5, -0.2, 0.2, 0.5, 0.7), labels = c("-0.5", "-0.2", "0.2", "0.5", "0.7"), cex.axis = 0.9)
abline(v = 0)
y <- sort(seq(1:71), decreasing = TRUE)

y_hds <- c(71,68,65,62,59,56,53,50,47,44,41,38,35,32,29,26,23,20,17,14,11,8,5,2)
y_trim <- y_hds-1
y_sp <- y_trim + 0.5

# Subset species (all)
hds_sub <- hds_as[1:24, ]
trim_sub <- trim_as[1:24, ]

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
setwd("D:/PhD/Second chapter/Data")
leg <- read.csv("leg_species.csv", sep = ";")
leg <- arrange(leg,codiEspecie)
colnames(leg)[1] <- "sp"

order_hds_leg <- left_join(order_hds, leg, by = "sp")
leg_order <- order_hds_leg[ ,c(10, 11)]
leg_order$nomActual <- as.character(leg_order$nomActual)
leg_order$English <- as.character(leg_order$English)
hds_as <- cbind(hds_as, leg_order)

# Add * if significance from trim result is different from hds result

txt <- ifelse(is_hds_sig == is_trim_sig, hds_as$English[1:24], paste("*", hds_as$English[1:24], sep = ""))
text(0.75,y_sp,labels = txt, cex = 0.8, adj = 0)
text(1.8,y_sp,labels = hds_as$nomActual[1:24], cex = 0.8, adj = 0, font = 3)

#legend(-0.55, 9, legend = c("HDS", "TRIM"), col = c("black", "red"), fill = c("black", "red"), bty = "n", cex = 0.9)

par(xpd = NA)
legend("bottomright", inset = c(+0.45, -0.05), legend = c("HDS", "TRIM"), col = c("black", "red"), fill = c("black", "red"), bty = "n", horiz = FALSE, cex = 0.9)



dev.off()


