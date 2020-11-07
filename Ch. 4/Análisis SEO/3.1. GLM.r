
rm(list = ls())

library(lme4)
library(MuMIn)
library(ggeffects)


#######

setwd("D:/PhD/Fourth chapter/Congreso SEO/Data_SEO")
data <- read.csv ("covariates.csv", header = TRUE)

# Check correlation

data <- data[complete.cases(data), ] # I don't know but I delete because I don't have time

correlation <- cor(data[ ,c(8:21)])
correlation[which(correlation > 0.5 & correlation < 1)] # Correlated: Dist carreteras - Dist nucleos urbanos (0.6)
#             Frut secano - Olivo y almendro 
names(data) # Quitar: Dist nucleos urbanos, olivo y almendro
data_unscaled <- data[ , -c(10, 19, 20)]
data[ ,c(8:18)] <- scale(data[ ,c(8:18)], center = TRUE, scale = TRUE)

#####
# For the GLM, scale differently the variables to plot them unscaled

#setwd("S:/PhD/Fourth chapter/Data")
data <- read.csv ("covariates.csv", header = TRUE)

# Check correlation

data <- data[complete.cases(data), ] # I don't know but I delete because I don't have time

correlation <- cor(data[ ,c(8:21)])
correlation[which(correlation > 0.5 & correlation < 1)] # Correlated: Dist carreteras - Dist nucleos urbanos (0.6)
#             Frut secano - Olivo y almendro 
names(data) # Quitar: Dist nucleos urbanos, olivo y almendro
variables_unscaled <- data[ , -c(1:7, 10, 19, 20)]
sdata <- scale(variables_unscaled) # Numeric data scaled
sdata <- as.data.frame(sdata) # To be able to include it in a model
# Save scaled attibutes:
scaleList <- list(scale = attr(sdata, "scaled:scale"),
                  center = attr(sdata, "scaled:center"))

#### Separate by periods ####
# SCALED
# Join sdata with IDs and periods to select periods
data_f <- cbind(data$Logger_ID, data$ID_p, data$used, sdata)
colnames(data_f)[1] <- "Logger_ID"
colnames(data_f)[2] <- "ID_p"
colnames(data_f)[3] <- "used"

data_f_p1 <- data_f[which(data_f$ID_p %in% ID_p[grep("p1", ID_p)]), ]
data_f_p2 <- data_f[which(data_f$ID_p %in% ID_p[grep("p2", ID_p)]), ]
data_f_p3 <- data_f[which(data_f$ID_p %in% ID_p[grep("p3", ID_p)]), ]

# UNSCALED
data_p1 <- data[which(data$ID_p %in% data$ID_p[grep("p1", data$ID_p)]), ]
data_p2 <- data[which(data$ID_p %in% ID_p[grep("p2", ID_p)]), ]
data_p3 <- data[which(data$ID_p %in% ID_p[grep("p3", ID_p)]), ]


# ---- Period 1 ----

# Choose the full model to include in dredge function


p1 <- glm(used ~ dist_caminos + dist_carreteras + pendiente +     
                pastos + forestal + cereal + barbecho + herb_secano + 
                herb_regadio + frut_regadio + frut_secano 
              ,family = binomial (link = "logit"),
              data = data_f_p1) # DATA SCALED

data_p1 <- data_p1[,colnames(data_p1) %in% c("dist_caminos","dist_carreteras","pendiente",     
                                               "pastos", "forestal","cereal","barbecho" ,"herb_secano", 
                                               "herb_regadio","frut_regadio","frut_secano" )]

p1 <- glm(used ~ dist_caminos + dist_carreteras + pendiente +     
            pastos + forestal + cereal + barbecho + herb_secano + 
            herb_regadio + frut_regadio + frut_secano 
          ,family = binomial (link = "logit"),
          data = data_p1) # DATA SCALED
summary(p1)

# Model selection

options(na.action = na.fail)
models_p1 <- dredge(p1, beta = "none", evaluate = TRUE )

setwd("S:/PhD/Fourth chapter/Data/Results")
save(models_p1, file = "dredge_p1_glm.RData")

load("dredge_p1.RData")
topmodels_p1 <- get.models(models_p1,subset = delta < 2)
avg_p1 <- model.avg(topmodels_p1)

# Because I did not scale the response, I can just do the predictions in a new data frame with unscaled data?

# On full model:
# Predictions on scaled data:
newdata <- as.data.frame(lapply(lapply(data_f_p1[, -c(1:3, 9)], mean), rep, 25))
newdata$cereal <- seq(min(data_f_p1$cereal), max(data_f_p1$cereal), length.out = 25)

pred <- predict(p1, newdata = newdata, type = "response", se.fit = TRUE )
lcl <- pred$fit - 1.96*pred$se.fit
lch <- pred$fit + 1.96*pred$se.fit

plot(pred$fit ~ newdata$cereal, ylim = c(-1, 1), main = "p1", type = "l")
polygon( x = c(newdata$cereal, rev(newdata$cereal)),
         y = c(lcl, rev(lch)), 
         col = adjustcolor(c("grey"),alpha.f = 0.6),
         border = NA)

# on original scale (data_p1 is in original scale)
newdata <- as.data.frame(lapply(lapply(data_p1[, -c(1:7, 10, 14, 19, 20)], mean), rep, 25))
newdata$cereal <- seq(min(data_p1$cereal), max(data_p1$cereal), length.out = 25)

pred <- predict(p1, newdata = newdata, type = "response", se.fit = TRUE )
lcl <- pred$fit - 1.96*pred$se.fit
lch <- pred$fit + 1.96*pred$se.fit

plot(pred$fit ~ newdata$cereal, ylim = c(-1, 1), main = "p1", type = "l")
polygon( x = c(newdata$cereal, rev(newdata$cereal)),
         y = c(lcl, rev(lch)), 
         col = adjustcolor(c("grey"),alpha.f = 0.6),
         border = NA)

# On model averaging:
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

# ---- Period 2 ----

# Choose the full model to include in dredge function

p2 <- glm(used ~ dist_caminos + dist_carreteras + pendiente +     
           pastos + forestal + cereal + barbecho + herb_secano + 
           herb_regadio + frut_regadio + frut_secano 
         ,family = binomial (link = "logit"),
         data = data_f_p2) # data scaled


options(na.action = na.fail)
models_p2 <- dredge(p2, beta = "none", evaluate = TRUE )

setwd("S:/PhD/Fourth chapter/Data/Results")
save(models_p2, file = "dredge_p2_glm.RData")

load("dredge_p2_glm.RData")
topmodels_p2 <- get.models(models_p2,subset = delta < 2)
avg_p2 <- model.avg(topmodels_p2)

# Plot
# On full model:

# Predictions on scaled data:
newdata <- as.data.frame(lapply(lapply(data_f_p2[, -c(1:3, 9)], mean), rep, 25))
newdata$cereal <- seq(min(data_f_p2$cereal), max(data_f_p2$cereal), length.out = 25)

pred <- predict(p2, newdata = newdata, type = "response", se.fit = TRUE )
lcl <- pred$fit - 1.96*pred$se.fit
lch <- pred$fit + 1.96*pred$se.fit

plot(pred$fit ~ newdata$cereal, ylim = c(-1, 1), main = "p2", type = "l")
polygon( x = c(newdata$cereal, rev(newdata$cereal)),
         y = c(lcl, rev(lch)), 
         col = adjustcolor(c("grey"),alpha.f = 0.6),
         border = NA)

m2 <- ggpredict(p2, "cereal", type = "fe", back.transform = TRUE)
plot(m2)

# on original scale (data_p1 is in original scale)
newdata <- as.data.frame(lapply(lapply(data_p2[, -c(1:7, 10, 14, 19, 20)], mean), rep, 25))
newdata$cereal <- seq(min(data_p2$cereal), max(data_p2$cereal), length.out = 25)

pred <- predict(p2, newdata = newdata, type = "response", se.fit = TRUE )
lcl <- pred$fit - 1.96*pred$se.fit
lch <- pred$fit + 1.96*pred$se.fit

plot(pred$fit ~ newdata$cereal, ylim = c(-1, 1), main = "p2", type = "l")
polygon( x = c(newdata$cereal, rev(newdata$cereal)),
         y = c(lcl, rev(lch)), 
         col = adjustcolor(c("grey"),alpha.f = 0.6),
         border = NA)
summary(p2)

# I don't know what is wrong. 
# - The estimate is significant
# - The prediction in type "response" should give the backtransformed predicted values
# - I don't scale the response, but trying with both the scaled and unscaled predictors is weird 
# - Is it problematic because cereal is 0/1?

# ---- Period 3 ----

# Choose the full model to include in dredge function

p3 <- glm(used ~ dist_caminos + dist_carreteras + pendiente +     
            pastos + forestal + cereal + barbecho + herb_secano + 
            herb_regadio + frut_regadio + frut_secano 
          ,family = binomial (link = "logit"),
          data = data_p3)

options(na.action = na.fail)
models_p3 <- dredge(p3, beta = "none", evaluate = TRUE )

setwd("S:/PhD/Fourth chapter/Data/Results")
save(models_p3, file = "dredge_p3_glm.RData")

load("dredge_p3_glm.RData")
topmodels_p3 <- get.models(models_p3,subset = delta < 2)
avg_p3 <- model.avg(topmodels_p3)


#### RESULTS ####
# ---- Period 1 ----

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

# ---- Period 2 ----

load("dredge_p2.RData")
topmodels_p2 <- get.models(models_p2,subset = delta < 2)
avg_p2 <- model.avg(topmodels_p2)


