
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
data_unscaled <- data[ , -c(10, 19, 20)]
data[ ,c(8:18)] <- scale(data[ ,c(8:18)], center = TRUE, scale = TRUE)

#### Separate by periods ####

ID_p <- unique(data$ID_p)
ID_p[grep("p1", ID_p)]
data_p1 <- data[which(data$ID_p %in% ID_p[grep("p1", ID_p)]), ]
data_p2 <- data[which(data$ID_p %in% ID_p[grep("p2", ID_p)]), ]
data_p3 <- data[which(data$ID_p %in% ID_p[grep("p3", ID_p)]), ]


# ---- Period 1 ----

# Choose the full model to include in dredge function

p1 <- glmer(used ~ dist_caminos + dist_carreteras + pendiente +      # Full model: It doesn't converge
              pastos + forestal + cereal + barbecho + herb_secano + herb_regadio + 
              frut_regadio + frut_secano + (1|Logger_ID), 
            family = binomial (link = "logit"),
            data = data_p1)

p1_1 <- glmer(used ~ dist_caminos + dist_carreteras + pendiente +      # Without forestal: It doesn't converge
              pastos + 
                #forestal + 
                cereal + barbecho + herb_secano + herb_regadio + 
              frut_regadio + frut_secano + (1|Logger_ID), 
            family = binomial (link = "logit"),
            data = data_p1)
p1_2 <- glmer(used ~ dist_caminos + dist_carreteras + pendiente +      # Without forestal and herb_secano: It doesn't converge
                pastos + 
                #forestal + 
                cereal + barbecho + 
                #herb_secano + 
                herb_regadio + 
                frut_regadio + frut_secano + (1|Logger_ID), 
              family = binomial (link = "logit"),
              data = data_p1)
p1_3 <- glmer(used ~ dist_caminos + dist_carreteras + pendiente +      # Without forestal, herb_secano, frut regadio: It doesn't converge
                pastos + 
                #forestal + 
                cereal + barbecho + 
                #herb_secano + 
                herb_regadio + 
                #frut_regadio + 
                frut_secano + (1|Logger_ID), 
              family = binomial (link = "logit"),
              data = data_p1)
p1_3_1 <- glm(used ~ dist_caminos + dist_carreteras + pendiente +      # Without forestal, herb_secano, frut regadio, ID: It doesn't converge
                pastos + 
                #forestal + 
                cereal + barbecho + 
                #herb_secano + 
                herb_regadio + 
                #frut_regadio + 
                frut_secano 
              #+ (1|Logger_ID), 
              ,family = binomial (link = "logit"),
              data = data_p1)
p1_4 <- glmer(used ~ dist_caminos + dist_carreteras + pendiente +      # Without forestal, herb_secano, frut regadio: It doesn't converge
                pastos + 
                #forestal + 
                cereal + barbecho + 
                #herb_secano + 
                herb_regadio + 
                #frut_regadio + 
                frut_secano + (1|Logger_ID), 
              family = binomial (link = "logit"),
              data = data_p1)
# Model selection

options(na.action = na.fail)
models_p1 <- dredge(p1_3_1, beta = "none", evaluate = TRUE )

setwd("S:/PhD/Fourth chapter/Data/Results")
save(models_p1, file = "dredge_p1.RData")

load("dredge_p1.RData")
topmodels_p1 <- get.models(models_p1,subset = delta < 2)
avg_p1 <- model.avg(topmodels_p1)

# ---- Period 2 ----

# Choose the full model to include in dredge function

p2 <- glmer(used ~ dist_caminos + dist_carreteras + pendiente +      # Full model: It doesn't converge
              pastos + forestal + cereal + barbecho + herb_secano + herb_regadio + 
              frut_regadio + frut_secano + (1|Logger_ID), 
            family = binomial (link = "logit"),
            data = data_p2)

p2_1 <- glmer(used ~ dist_caminos + dist_carreteras + pendiente +      # Without forestal: It doesn't converge
              pastos + 
                #forestal + 
                cereal + barbecho + herb_secano + 
                herb_regadio + 
              frut_regadio + frut_secano + (1|Logger_ID), 
            family = binomial (link = "logit"),
            data = data_p2)
p2_2 <- glmer(used ~ dist_caminos + dist_carreteras + pendiente +      # Without forestal and herb_secano: It doesn't converge
                pastos + 
                #forestal + 
                cereal + barbecho + 
                #herb_secano + 
                herb_regadio + 
                frut_regadio + frut_secano + (1|Logger_ID), 
              family = binomial (link = "logit"),
              data = data_p2)
p2_3 <- glmer(used ~ dist_caminos + dist_carreteras + pendiente +      # Without forestal, frut regadio and herb_secano: It CONVERGES
                pastos + 
                #forestal + 
                cereal + barbecho + 
                #herb_secano + 
                herb_regadio + 
                #frut_regadio + 
                frut_secano + (1|Logger_ID), 
              family = binomial (link = "logit"),
              data = data_p2)

summary(p2_1)

# Model selection

options(na.action = na.fail)
models_p2 <- dredge(p2_3, beta = "none", evaluate = TRUE )

setwd("S:/PhD/Fourth chapter/Data/Results")
save(models_p2, file = "dredge_p2.RData")

load("dredge_p2.RData")
topmodels_p2 <- get.models(models_p2,subset = delta < 2)
avg_p2 <- model.avg(topmodels_p2)
summary(avg_p2)

# ---- Period 3 ----

# Choose the full model to include in dredge function

p3 <- glmer(used ~ dist_caminos + dist_carreteras + pendiente +      # Full model: Converges
              pastos + forestal + cereal + barbecho + herb_secano + herb_regadio + 
              frut_regadio + frut_secano + (1|Logger_ID), 
            family = binomial (link = "logit"),
            data = data_p3)

p3_1 <- glmer(used ~ dist_caminos + dist_carreteras + pendiente +      # Without forestal: Converges (... removing more variables as well)
                pastos + 
                #forestal + 
                cereal + barbecho + herb_secano + herb_regadio + 
                frut_regadio + frut_secano + (1|Logger_ID), 
              family = binomial (link = "logit"),
              data = data_p3)
p3_3 <- glmer(used ~ dist_caminos + dist_carreteras + pendiente +      # Without forestal, frut regadio and herb_secano: It CONVERGES
                pastos + 
                #forestal + 
                cereal + barbecho + 
                #herb_secano + 
                herb_regadio + 
                #frut_regadio + 
                frut_secano + (1|Logger_ID), 
              family = binomial (link = "logit"),
              data = data_p3)

# Model selection

options(na.action = na.fail)
models_p3 <- dredge(p3_3, beta = "none", evaluate = TRUE )

setwd("S:/PhD/Fourth chapter/Data/Results")
save(models_p3, file = "dredge_p3.RData")

load("dredge_p3.RData")
topmodels_p3 <- get.models(models_p3,subset = delta < 2)
avg_p3 <- model.avg(topmodels_p3)
summary(avg_p3)


#

#### Results ####

# Results model averaging in a table
# Take the conditional average (only averages among the models where the parameter appears)

setwd("S:/PhD/Fourth chapter/Data/Results")

results <- as.data.frame(matrix(NA, nrow = 9*3, ncol = 5))
colnames(results) <- c("Period", "Variable", "Estimate", "SE", "Pvalue")
results$Period <- c(rep("p1",9),rep("p2",9),rep("p3",9))

# Period 1

load("dredge_p1.RData")
topmodels_p1 <- get.models(models_p1,subset = delta < 2)
avg_p1 <- model.avg(topmodels_p1)
sum_p1 <- summary(avg_p1)

coef_p1 <- as.data.frame(sum_p1$coefmat.subset)
coef_p1$Period <- "p1"
results$Variable[results$Period == "p1"] <- rownames(coef_p1) 
results$Estimate[results$Period == "p1"] <- coef_p1$Estimate
results$SE[results$Period == "p1"] <- coef_p1$`Std. Error`
results$Pvalue[results$Period == "p1"] <- coef_p1$`Pr(>|z|)`
results$Pvalue[results$Period == "p1"] <- round(results$Pvalue[results$Period == "p1"], 3)


# Period 2

load("dredge_p2.RData")
topmodels_p2 <- get.models(models_p2,subset = delta < 2)
avg_p2 <- model.avg(topmodels_p2)
sum_p2 <- summary(avg_p2)

coef_p2 <- as.data.frame(sum_p2$coefmat.subset)
coef_p2$Period <- "p2"
results$Variable[results$Period == "p2"] <- rownames(coef_p2) 
results$Estimate[results$Period == "p2"] <- coef_p2$Estimate
results$SE[results$Period == "p2"] <- coef_p2$`Std. Error`
results$Pvalue[results$Period == "p2"] <- coef_p2$`Pr(>|z|)`
results$Pvalue[results$Period == "p2"] <- round(results$Pvalue[results$Period == "p2"], 3)


# Period 3

load("dredge_p3.RData")
topmodels_p3 <- get.models(models_p3,subset = delta < 2)
avg_p3 <- model.avg(topmodels_p3)
sum_p3 <- summary(avg_p3)

coef_p3 <- as.data.frame(sum_p3$coefmat.subset)
coef_p3$Period <- "p3"
results$Variable[results$Period == "p3"] <- rownames(coef_p3) 
results$Estimate[results$Period == "p3"] <- coef_p3$Estimate
results$SE[results$Period == "p3"] <- coef_p3$`Std. Error`
results$Pvalue[results$Period == "p3"] <- coef_p3$`Pr(>|z|)`
results$Pvalue[results$Period == "p3"] <- round(results$Pvalue[results$Period == "p3"], 3)


results$Pvalue <- ifelse(results$Pvalue < 0.01, "< 0.01*", results$Pvalue)
results[ ,c(3:4)] <- round(results[ ,c(3:4)], 3)

# Wide format
results1 <- results[1:9, ]
results2 <- results[10:18, 3:5]
results3 <- results[19:27, 3:5]

results_wide <- cbind(results1, results2, results3)
results_wide <- results_wide[ ,-c(1)]

write.csv(results_wide, "coefficients.csv")

#### PLOT ####

# Unscale values ????????????????????????
library(DMwR)

data_unscaled
obj_scale <- scale(data_unscaled[ ,c(8:18)], center = TRUE, scale = TRUE)

p1_uns <- unscale(data_p1[ ,c(8:18)], obj_scale)

data[ ,c(8:18)] <- unscale(data[ ,c(8:18)], center = TRUE, scale = TRUE)


# Predict values of each model (3 periods) for the cereal variable

newdata <- as.data.frame(lapply(lapply(data_p3[, -c(1:7,13)], mean), rep, 25))
newdata$cereal_new <- seq(min(data_p3$cereal), max(data_p3$cereal), length.out = 25)

pred <- predict(avg_p2, newdata = newdata, type = "response", se.fit = TRUE )

lcl <- pred$fit - 1.96*pred$se.fit
lch <- pred$fit + 1.96*pred$se.fit

plot(pred$fit ~ newdata$cereal_new ,main = "p1")
segments(x0=c(1:6),
         x1=c(1:6),
         y0=lcl,
         y1=lch)


# Predict values of model 1

# Period 1

load("dredge_p1.RData")

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

# Period 2

load("dredge_p2.RData")
topmodels_p2 <- get.models(models_p2,subset = delta < 2)
avg_p2 <- model.avg(topmodels_p2)

newdata <- as.data.frame(lapply(lapply(data_p2[, -c(1:7,13, 14)], mean), rep, 25))
newdata$cereal <- seq(min(data_p2$cereal), max(data_p2$cereal), length.out = 25)

pred <- predict(avg_p2, newdata = newdata, type = "response", se.fit = TRUE ) 
pred2 <- predict(p2_3, newdata = newdata, type = "response", se.fit = TRUE ) 
