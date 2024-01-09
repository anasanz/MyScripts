
eudist <- read.table("D:/PhD/Fourth chapter/Results/Analisis3_bottleneck_effect/Revision/Euclidean_distances.txt", header = T, dec = ",", sep = "\t")

eudist$Period <- str_sub(eudist$Period,1,nchar(eudist$Period)-5)

##REV: Interested in year2, which is the "dun year" or "academic year"
eudist[which(eudist$Year != eudist$Year2), ]

eudist <- eudist[,-c(4:6)]
eudist <- eudist[,c(1,4,2,3)]
colnames(eudist)[2] <- "Year"


# Summary statistics

mean_dist <- eudist %>%
  group_by(Period) %>%
  summarise(
    count = n(),
    mean = mean(eu.dist, na.rm = TRUE),
    sd = sd(eu.dist, na.rm = TRUE),
    se = stderr(eu.dist, na.rm = TRUE)
  )

mean_dist <- as.data.frame(mean_dist)

setwd("D:/PhD/Fourth chapter/Results/Analisis3_bottleneck_effect/Revision")
pdf("Fig_movements_lmm_groups.pdf", 8, 6)
par(mfrow = c(2,2),
    mar = c(2,2,2,0),
    oma = c(3,3,2,2))

plot(mean_dist$mean, ylim = c(200,400), xlim = c(0,4), axes = FALSE, xaxs="i", yaxs = "i",
     ylab = " ", xlab = " ", pch = 19)
axis(1, labels = c(" ", "Short", "Tall", "Stubble"), at = c(0.5,1,2,3))
axis(2, pos = 0.5)
mtext("Euclidean distance (m)", side = 2, line = 0.5, cex = 1)
x <- c(1,2,3)
segments(x, mean_dist$mean - mean_dist$se * 2, 
         x, mean_dist$mean + mean_dist$se * 2, 
         lwd = 1.5)
arrows(x, mean_dist$mean - mean_dist$se * 2, 
       x, mean_dist$mean + mean_dist$se * 2, 
       lwd = 1.5, angle = 90, code = 3, length = 0.05)

lines(mean_dist$mean, col = "blue")


## ---- Anova ----

# Random ID
eudist$log_eudist <- log(eudist$eu.dist + 0.00001)
m1 <- lme(log_eudist ~ Period, random = ~ 1|Bird.ID,
          data = eudist)
anova(m1)
emmeans(m1, list(pairwise ~ Period), adjust = "tukey")

# Random year and ID
m2 <- lme(log_eudist ~ Period, random = list(Bird.ID = ~ 1, Year = ~ 1),
          data = eudist)
anova(m2)
emmeans(m2, list(pairwise ~ Period), adjust = "tukey")

AIC(m1,m2)