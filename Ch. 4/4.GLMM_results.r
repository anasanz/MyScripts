
rm(list = ls())

library(lme4)
library(MuMIn)
library(ggeffects)

# Run GLMM

setwd("C:/Users/Ana/Documents/PhD/PhD_12_Nov/Fourth chapter/Data")
data <- read.csv ("covariates.csv", header = TRUE)

# Check correlation

data <- data[complete.cases(data), ] # I don't know but I delete because I don't have time

correlation <- cor(data[ ,c(8:21)])
correlation[which(correlation > 0.5 & correlation < 1)] # Correlated: Dist carreteras - Dist nucleos urbanos (0.6)
#             Frut secano - Olivo y almendro 
names(data) # Quitar: Dist nucleos urbanos, olivo y almendro
data_unscaled <- data[ , -c(10, 19, 20)]
data[ ,c(8:18)] <- scale(data[ ,c(8:18)], center = TRUE, scale = TRUE)

#### Separate by periods ####

ID_p <- unique(data$ID_p)
ID_p[grep("p1", ID_p)]
data_p1 <- data[which(data$ID_p %in% ID_p[grep("p1", ID_p)]), ]
data_p2 <- data[which(data$ID_p %in% ID_p[grep("p2", ID_p)]), ]
data_p3 <- data[which(data$ID_p %in% ID_p[grep("p3", ID_p)]), ]

# ---- Cereal ----

# Predict values of model 1

# Period 1
setwd("C:/Users/ana.sanz/Documents/PhD_20_sept/Fourth chapter/Data/Results")

load("dredge_p1.RData")
topmodels_p1 <- get.models(models_p1,subset = delta < 2)
avg_p1 <- model.avg(topmodels_p1)

newdata <- as.data.frame(lapply(lapply(data_p1[, -c(1:7,13, 14)], mean), rep, 25))
newdata$cereal <- seq(min(data_p1$cereal), max(data_p1$cereal), length.out = 25)

pred <- predict(avg_p1, newdata = newdata, type = "response", se.fit = TRUE )

lcl <- pred$fit - 1.96*pred$se.fit
lch <- pred$fit + 1.96*pred$se.fit

plot(pred$fit ~ newdata$cereal, ylim = c(-1, 1), main = "p1", type = "l")
polygon( x = c(newdata$cereal, rev(newdata$cereal)),
         y = c(lcl, rev(lch)), 
         col = adjustcolor(c("grey"),alpha.f = 0.6),
         border = NA)
newx <- seq(min(newdata$cereal),max(newdata$cereal), length.out = 25)
lines(newx, lcl, col = "red")
lines(newx, lch, col = "red")

m <- ggpredict(avg_p1, "cereal", type = "fe")

# Period 2
setwd("C:/Users/ana.sanz/Documents/PhD_20_sept/Fourth chapter/Data/Results")

load("dredge_p2.RData")
topmodels_p2 <- get.models(models_p2,subset = delta < 2)
avg_p2 <- model.avg(topmodels_p2)

newdata <- as.data.frame(lapply(lapply(data_p2[, -c(1:7,13, 14)], mean), rep, 25))
newdata$cereal <- seq(min(data_p2$cereal), max(data_p2$cereal), length.out = 25)
newdata$Logger_ID <- rep(unique(data_p2$Logger_ID), 5)

pred <- predict(avg_p2, newdata = newdata, type = "response", re.form = NA ) # It doesn't work
plot(pred ~ newdata$cereal, ylim = c(0, 0.3), main = "p2", type = "l")



# Try with full model (best model)
p2_3 <- glmer(used ~ dist_caminos + dist_carreteras + pendiente +      # Without forestal, frut regadio and herb_secano: It CONVERGES
                pastos + cereal + barbecho + herb_regadio + frut_secano + (1|Logger_ID), 
              family = binomial (link = "logit"),
              data = data_p2)
summary(p2_3)
pred2 <- predict(p2_3, newdata = newdata, type = "response", re.form = NA)
plot(pred2 ~ newdata$cereal, ylim = c(0, 0.1), main = "p2", type = "l")
m <- ggpredict(p2_3, "cereal", type = "fe")
plot(m)

m2 <- ggpredict(p2_3, "cereal", type = "re", back.transform = TRUE)
plot(m2)

pred3 <- predict(p2_3, type = "response", re.form = NA)
cereal <- seq(min(data_p2$cereal), max(data_p2$cereal), length.out = 23015)
plot(pred3 ~ cereal, ylim = c(0, 0.1), main = "p2")
# This doesn't work because there are too many datapoints, it has to be with the new data frame

# Try with other model that converges
p2_4 <- glmer(used ~ dist_caminos + 
                #dist_carreteras + 
                pendiente +      # Without forestal, frut regadio and herb_secano: It CONVERGES
                pastos + cereal + barbecho + herb_regadio + frut_secano + (1|Logger_ID), 
              family = binomial (link = "logit"),
              data = data_p2)
m2 <- ggpredict(p2_4, "cereal", type = "re")
plot(m2)
summary(p2_4)

# ---- Pendiente ----

# Period 1
setwd("C:/Users/ana.sanz/Documents/PhD_20_sept/Fourth chapter/Data/Results")
setwd("C:/Users/Ana/Documents/PhD/PhD_12_Nov/Fourth chapter/Data/Results")


load("dredge_p1.RData")
topmodels_p1 <- get.models(models_p1,subset = delta < 2)
avg_p1 <- model.avg(topmodels_p1)

newdata_pend<- as.data.frame(lapply(lapply(data_p1[, -c(1:7,11, 13)], mean), rep, 25))
newdata_pend$pendiente <- seq(min(data_p1$pendiente), max(data_p1$pendiente), length.out = 25)

pred <- predict(avg_p1, newdata = newdata_pend, type = "response", se.fit = TRUE )

lcl <- pred$fit - 1.96*pred$se.fit
lch <- pred$fit + 1.96*pred$se.fit

plot(pred$fit ~ newdata_pend$pendiente, ylim = c(0, 0.05), main = "p1", type = "l")
polygon( x = c(newdata_pend$pendiente, rev(newdata_pend$pendiente)),
         y = c(lcl, rev(lch)), 
         col = adjustcolor(c("grey"),alpha.f = 0.6),
         border = NA)
newx <- seq(min(newdata_pend$pendiente),max(newdata_pend$pendiente), length.out = 25)
lines(newx, lcl, col = "red")
lines(newx, lch, col = "red")
