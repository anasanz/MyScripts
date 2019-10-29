rm(list = ls())

library(lme4)
library(MuMIn)

# Run GLMM

setwd("C:/Users/ana.sanz/Documents/PhD_20_sept/Fourth chapter/Data")
data <- read.csv ("covariates.csv", header = TRUE)

# Check correlation

data <- data[complete.cases(data), ] # I don't know but I delete because I don't have time

correlation <- cor(data[ ,c(8:21)])
correlation[which(correlation > 0.5 & correlation < 1)] # Correlated: Dist carreteras - Dist nucleos urbanos (0.6)
#             Frut secano - Olivo y almendro 
names(data) # Quitar: Dist nucleos urbanos, olivo y almendro
data <- data[ , -c(10, 19, 20)]

#### Period 1 ####

ID_p <- unique(data$ID_p)
data_p1 <- data[which(data$ID_p %in% ID_p[grep("p1", ID_p)]), ]

# Scale variables

data_p1$dist_caminos_sc <- as.numeric(scale(data_p1$dist_caminos, center = TRUE, scale = TRUE))
data_p1$dist_carreteras_sc <- as.numeric(scale(data_p1$dist_carreteras, center = TRUE, scale = TRUE))
data_p1$pendiente_sc <- as.numeric(scale(data_p1$pendiente, center = TRUE, scale = TRUE))
data_p1$pastos_sc <- as.numeric(scale(data_p1$pastos, center = TRUE, scale = TRUE))
data_p1$forestal_sc <- as.numeric(scale(data_p1$forestal, center = TRUE, scale = TRUE))
data_p1$cereal_sc <- as.numeric(scale(data_p1$cereal, center = TRUE, scale = TRUE))
data_p1$barbecho_sc <- as.numeric(scale(data_p1$barbecho, center = TRUE, scale = TRUE))
data_p1$herb_secano_sc <- as.numeric(scale(data_p1$herb_secano, center = TRUE, scale = TRUE))
data_p1$herb_regadio_sc <- as.numeric(scale(data_p1$herb_regadio, center = TRUE, scale = TRUE))
data_p1$frut_regadio_sc <- as.numeric(scale(data_p1$frut_regadio, center = TRUE, scale = TRUE))
data_p1$frut_secano_sc <- as.numeric(scale(data_p1$frut_secano, center = TRUE, scale = TRUE))

# Full model:

p1_3_1 <- glm(used ~ dist_caminos_sc + dist_carreteras_sc + pendiente_sc +      # Without forestal, herb_secano, frut regadio, ID: It doesn't converge
                pastos_sc + 
                #forestal + 
                cereal_sc + barbecho_sc + 
                #herb_secano + 
                herb_regadio_sc + 
                #frut_regadio + 
                frut_secano_sc 
              #+ (1|Logger_ID), 
              ,family = binomial (link = "logit"),
              data = data_p1)

summary(p1_3_1)

# ---- Predict ----

# 1. CEREAL
# Create new data frame to predict values. I will predict over the real values (not scaled)
# Remove the extra variables, forestal, cereal (because it is what I want to predict), herb secano, frut regadio and scaled variables
newdata <- as.data.frame(lapply(lapply(data_p1[, -c(1:7,12,13,15,17,19:29)], mean), rep, 100)) 
newdata$cereal <- seq(min(data_p1$cereal), max(data_p1$cereal), length.out = 100)
newdata <- newdata[ ,c(1:4,8,5:7)]

colnames(newdata) <- paste(colnames(newdata), "_sc", sep = "")

# Predict response (probability of selection)

pred <- predict(p1_3_1, newdata = newdata, type = "response", se.fit = TRUE )

lcl <- pred$fit - 1.96*pred$se.fit
lch <- pred$fit + 1.96*pred$se.fit

plot(pred$fit ~ newdata$cereal_sc, ylim = c(-3, 3), main = "p1", type = "l")
polygon( x = c(newdata$cereal_sc, rev(newdata$cereal_sc)),
         y = c(lcl, rev(lch)), 
         col = adjustcolor(c("grey"),alpha.f = 0.6),
         border = NA)
newx <- seq(min(newdata$cereal_sc),max(newdata$cereal_sc), length.out = 100)
lines(newx, lcl, col = "red")
lines(newx, lch, col = "red")

m <- ggpredict(p1_3_1, "cereal_sc", type = "re")
plot(m)

# 2. PENDIENTE
# Remove pendiente instead of cereal (10)
newdata_pend <- as.data.frame(lapply(lapply(data_p1[, -c(1:7,12,10,15,17,19:29)], mean), rep, 100)) 
newdata_pend$pendiente <- seq(min(data_p1$pendiente), 5, length.out = 100) # Cojo 5 de máximo para mejorar la gráfica
newdata_pend <- newdata_pend[ ,c(1:2,8,3:7)]

colnames(newdata_pend) <- paste(colnames(newdata_pend), "_sc", sep = "")

# Predict response (probability of selection)

pred <- predict(p1_3_1, newdata = newdata_pend, type = "response", se.fit = TRUE )

lcl <- pred$fit - 1.96*pred$se.fit
lch <- pred$fit + 1.96*pred$se.fit

plot(pred$fit ~ newdata_pend$pendiente_sc, ylim = c(-1, 2), main = "p1", type = "l", xlab = "Pendiente", ylab = "Probabilidad de selección")
polygon( x = c(newdata_pend$pendiente_sc, rev(newdata_pend$pendiente_sc)),
         y = c(lcl, rev(lch)), 
         col = adjustcolor(c("grey"),alpha.f = 0.6),
         border = NA)
newx <- seq(min(newdata_pend$pendiente_sc),5, length.out = 100)
lines(newx, lcl, col = "red")
lines(newx, lch, col = "red")

m <- ggpredict(p1_3_1, "pendiente_sc [all]", type = "re")
plot(m)

# 3. BARBECHO
# Remove barbecho instead of pendiente (14)
newdata_bar <- as.data.frame(lapply(lapply(data_p1[, -c(1:7,12,14,15,17,19:29)], mean), rep, 100)) 
newdata_bar$barbecho <- seq(min(data_p1$barbecho), max(data_p1$barbecho), length.out = 100) 
newdata_bar <- newdata_bar[ ,c(1:5,8,6:7)]

colnames(newdata_bar) <- paste(colnames(newdata_bar), "_sc", sep = "")

# Predict response (probability of selection)

pred <- predict(p1_3_1, newdata = newdata_bar, type = "response", se.fit = TRUE )

lcl <- pred$fit - 1.96*pred$se.fit
lch <- pred$fit + 1.96*pred$se.fit

plot(pred$fit ~ newdata_bar$barbecho_sc, ylim = c(-1, 1), main = "p1", type = "l", xlab = "% Barbecho", ylab = "Probabilidad de selección")
polygon( x = c(newdata_bar$barbecho_sc, rev(newdata_bar$barbecho_sc)),
         y = c(lcl, rev(lch)), 
         col = adjustcolor(c("grey"),alpha.f = 0.6),
         border = NA)
newx <- seq(min(newdata_bar$barbecho_sc),max(newdata_bar$barbecho_sc), length.out = 100)
lines(newx, lcl, col = "red")
lines(newx, lch, col = "red")

m <- ggpredict(p1_3_1, "barbecho_sc", type = "re")
plot(m)
