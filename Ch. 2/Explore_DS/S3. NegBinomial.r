
black <- c(119,16,12,7,3,2,0)
white <- c(1070,60,14,4,0,0,1)
resp <- c(rep(0:6,times=black), rep(0:6,times=white))
race <- factor(c(rep("black", sum(black)), rep("white", sum(white))),
                levels = c("white","black"))
victim <- data.frame(resp, race)

table(race)
race

with(victim, tapply(resp, race, mean))
with(victim, tapply(resp, race, var))

table(resp, race)
pGLM <- glm(resp ~ race, data=victim, family = poisson)
summary(pGLM)

exp(coef(pGLM)[2])

exp(predict(pGLM, newdata = data.frame(race=c("white","black"))))

# fitted counts for Poisson GLM:
fmeans <- exp(predict(pGLM, newdata = data.frame(race = c("white","black"))))
fmeans

fittedW <- dpois(0:6,lambda = fmeans[1]) * sum(victim$race=="white") 
fittedB <- dpois(0:6,lambda = fmeans[2]) * sum(victim$race=="black") 
data.frame(Response=0:6,BlackObs=black, BlackFit=round(fittedB,1), 
                                WhiteObs=white, WhiteFit=round(fittedW,1))


library(MASS)
nbGLM <- glm.nb(resp ~ race, data=victim)
summary(nbGLM)
