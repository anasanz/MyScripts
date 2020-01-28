
# EXPLORE VARIABLES
# Check correlation between variables and outliers
#   There is no correlation between variables
#   Need to log-transform the fallow variables

rm(list=ls())

# Load variables

setwd("C:/Users/Ana/Documents/PhD/Third chapter/Data")

crich <- read.csv("crop_richness_500.csv")
fsize <- read.csv("av_fieldsize_500.csv")
aes <- read.csv("AES_15_19.csv")
sg <- read.csv("SG_15_19.csv")
green <- read.csv("GREEN_15_19.csv")

# ---- Check correlation ----

crich_cor <- as.vector(as.matrix(crich[ ,c(3:7)]))
fsize_cor <- as.vector(as.matrix(fsize[ ,c(3:7)]))
aes_cor <- as.vector(as.matrix(aes[ ,c(3:7)]))
sg_cor <- as.vector(as.matrix(sg[ ,c(3:7)]))
green_cor <- as.vector(as.matrix(green[ ,c(3:7)]))

cor_matrix <- cbind(crich_cor,fsize_cor,aes_cor,sg_cor,green_cor)

cor(cor_matrix, method = "pearson") # NOT CORRELATED

# ---- Check outliers ----

crich_df <- as.data.frame(crich[ ,c(3:7)])
fsize_df <- as.data.frame(fsize[ ,c(3:7)])
aes_df <- as.data.frame(aes[ ,c(3:7)])
sg_df <- as.data.frame(sg[ ,c(3:7)])
green_df <- as.data.frame(green[ ,c(3:7)])


#### Crop richness ####
# is okay
hist(crich_cor)
summary(crich_cor)

##### Field size ####
#(I don't know...)
hist(fsize_cor)
summary(fsize_cor)
fsize_df[apply(fsize_df, 1, max) > 10,]

##### AES ####
# It has very extreme values, better log transform it to
# incude it in the model, predict it in the log-scale and then plot it in the 
# normal scale
aes_df[apply(aes_df, 1, max) > 40,]
# Not log
hist(aes_cor)
summary(aes_cor)
aes_cor_sc <- scale(aes_cor) # When you scale it you get the same difference between values (they just get other numbers)
hist(aes_cor_sc)

# log transform
aes_cor[aes_cor == 0] <- 0.01
log_aes_cor <- log(aes_cor) # When you log transform it the difference between values change (big values get smaller, while small values dont change)
hist(log_aes_cor)
hist(exp(log_aes_cor))

# Example
log(aes_cor)[1]
aes_cor[1]
beta <- 2
y <- log_aes_cor *beta # Predict y with the log transformed data
plot(y~log_aes_cor)
plot(y~aes_cor) # Plot predicted y with values in the exp (real) scale

##### SG ####
#Same than AES (very extreme values), log-transform
hist(sg_cor)
summary(sg_cor)
sg_df[apply(sg_df, 1, max) > 40,]
# log-transform
sg_cor[sg_cor == 0] <- 0.01
log_sg_cor <- log(sg_cor) # When you log transform it the difference between values change (big values get smaller, while small values dont change)
hist(log_sg_cor)


##### GREEN ####
#Same than AES (very extreme values), log-transform
hist(green_cor)
summary(green_cor)
green_df[apply(green_df, 1, max) > 40,]
# log-transform
green_cor[green_cor == 0] <- 0.01
log_green_cor <- log(green_cor) # When you log transform it the difference between values change (big values get smaller, while small values dont change)
hist(log_green_cor)






