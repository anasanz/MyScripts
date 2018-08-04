
#FIELD SCALE_ALCARAVÁN (BUOED)

library(ggplot2)

setwd("C:/Users/ana.sanz/Documents/Análisis BUOED/Datos barbechos arrendados/Variables")
setwd("C:/Users/ana.sanz/Documents/Datos/Datos barbechos arrendados/Variables")

f<-read.csv("Variables.csv",sep = ",",header=TRUE,fill = TRUE)
#Olivier
o<-f[c(1:120),which(colnames(f) %in% c("IDfinca","Codi_Finca","CF_A","Subzone","Zone","EspecieObj","Contatge",
                               "Recob_plotViu","Recob_plotMort","PromigAltura1Plot","Cluster",
                               "Simpson","lev_ind","age","area","shape","Tractament","Any","TBL_500","PAR_500","Fallow_500",
                               "Tree_500","Irri_500","biom","LAI_sd","SAI_sd"))]

o<-o[,c(2,3,4,22,23,5:14,18,17,15,16,19,20,21,24,25,26)]
colnames(o)[6]<-"sp"
colnames(o)[7]<-"count"
colnames(o)[8]<-"cov_live"
colnames(o)[9]<-"cov_dead"
colnames(o)[10]<-"height"
colnames(o)[17]<-"trat"

o<-o[ which(o$sp %in% c("BUOED","ALRUF","CABRA","MECAL","PTALC","TERAX_M")), ]

#write.csv(o,"Data_subset.csv")


f<-f[,which(colnames(f) %in% c("IDfinca","Codi_Finca","CF_A","Subzone","Zone","EspecieObj","Contatge",
                               "Recob_plotViu","Recob_plotMort","PromigAltura1Plot","Cluster",
                               "Simpson","lev_ind","age","area","shape","Any"))]

b<-f[which(f$EspecieObj == "BUOED"),]
b<-b[complete.cases(b),] #Si añado lev_ind se van 9 observaciones
b<-b[-which(duplicated(b[,2:17])),]
b<-b[-which(duplicated(b$CF_A)),] 

b<-b[-which(row.names(b) == "11122"),] #En el casi de eliminar la obs de 7 individuos


################################################################################
#1. DATA EXPLORATION

#Relation response - Predictors

ggplot(b,aes(Recob_plotViu,Contatge))+geom_point()+geom_smooth()+theme_classic()
ggplot(b,aes(Recob_plotMort,Contatge))+geom_point()+geom_smooth()+theme_classic() #Is it relevant?
ggplot(b,aes(PromigAltura1Plot,Contatge))+geom_point()+geom_smooth()+theme_classic() #Quadratic?
ggplot(b,aes(Simpson,Contatge))+geom_point()+geom_smooth()+theme_classic()
ggplot(b,aes(lev_ind,Contatge))+geom_point()+geom_smooth()+theme_classic() #quadratic? 
ggplot(b,aes(area,Contatge))+geom_point()+geom_smooth()+theme_classic()
ggplot(b,aes(shape,Contatge))+geom_point()+geom_smooth()+theme_classic()
ggplot(b,aes(age,Contatge))+geom_point()+geom_smooth()+theme_classic()#Continuous. categorical?

ggplot(b,aes(as.factor(Cluster),Contatge))+geom_boxplot()+theme_classic()
ggplot(b,aes(as.factor(age),Contatge))+geom_boxplot()+theme_classic() #categorical

ggplot(b,aes(as.factor(Tractament),Contatge))+geom_boxplot()+theme_classic()

ggplot(b,aes(as.factor(Subzone),Contatge))+geom_boxplot()+theme_classic() 
ggplot(b,aes(as.factor(Zone),Contatge))+geom_boxplot()+theme_classic()#Possible effect of zone
ggplot(b,aes(as.factor(Any),Contatge))+geom_boxplot()+theme_classic() #No year effect

#CORRELATION BETWEEN VARIABLES (Generic)
b
cor(b[,c(2:10,12)],method = "spearman")

#Es posible que haya alta correlación entre las variables de vegetación y el tratamiento
ggplot(b,aes(as.factor(Tractament),Recob_plotViu))+geom_boxplot()+theme_classic() #Correlation
ggplot(b,aes(as.factor(Tractament),PromigAltura1Plot))+geom_boxplot()+theme_classic() #Some correlation,not much
ggplot(b,aes(as.factor(Tractament),Recob_plotMort))+geom_boxplot()+theme_classic() #Some correlation,not much
ggplot(b,aes(as.factor(Tractament),Simpson))+geom_boxplot()+theme_classic() 

#RESPONSE

hist(b$Contatge) #Many 0: 385/483; 98 presences mÃ¡s o menos distribuidas
length(which(b$Contatge == 0))/458*100 #78%
xtabs(~ Contatge + Subzone,b)

#CHECK ASSUMPTIONS, QUADRATIC EFFECTS, OVERDISPERSION

#I start by doing a glm poisson model with all covariates 
b<-b[,c(3,6:18)]
mp1<-glm(Contatge ~ Recob_plotViu + PromigAltura1Plot + Simpson + lev_ind + area + shape + as.factor(Cluster)
         + as.factor(age) + as.factor(Tractament),
         data = b,
         family = poisson)

mp2<-glm(Contatge ~ Recob_plotViu + PromigAltura1Plot + I(PromigAltura1Plot^2) + Simpson + lev_ind + 
           area + shape + as.factor(Cluster) + as.factor(age) + as.factor(Tractament),
         data = b,
         family = poisson)

mp3<-glm(Contatge ~ Recob_plotViu + PromigAltura1Plot + I(PromigAltura1Plot^2) + Simpson + lev_ind + 
           I(lev_ind^2) + area + shape + as.factor(Cluster) + as.factor(age) + as.factor(Tractament),
         data = b,
         family = poisson)

AIC(mp1,mp2,mp3) #Lower AIC quadratic effect of height and heterogeneity

summary(mp3) # Dev/df = 1.03; No overdispersion
#In poisson dist: variance = mean
m<-mean(b$Contatge)
v<-var(b$Contatge)
ratio<-v/m
ratio    # r=2.03 so we have overdispersion (>1), but in mp3 d/df=1

par(mfrow=c(2,2))
plot(mp3) #Not normal, not linear, not homogeneous
influencePlot(mp3) #Obs 11122 very influencing (because 7 individuals?????) Removed.

#Residuals against predictors to check homogeneity
ggplot(b,aes(Recob_plotViu,residuals(mp3)))+geom_point()+geom_smooth() #Homo?
ggplot(b,aes(PromigAltura1Plot,residuals(mp3)))+geom_point()+geom_smooth() #Not homo
ggplot(b,aes(area,residuals(mp3)))+geom_point()+geom_smooth() #Not homo
ggplot(b,aes(Subzone,rstandard(mp3)))+geom_boxplot()+theme_classic() #Mean around 0...no effect?
#Dont see the effect of zone so maybe I dont need a glmm

############################################################################################
#2. CHOOSE A MODEL STRUCTURE

#There is a lot of 0, but data is not overdispersed: use zero inflated
library(pscl)     # for zero inflated poisson model

#Variables are too different so model gives problems. Solve by scaling them, but then I will have to unscale
#them to make the prediction (Scale: Mean 0 and sd 1)

b$sviv<-scale(b$Recob_plotViu)
b$smuer<-scale(b$Recob_plotMort)
b$salt<-scale(b$PromigAltura1Plot)
b$salt_q<-scale(I(b$PromigAltura1Plot^2))
b$ssim<-scale(b$Simpson)
b$shet<-scale(b$lev_ind)
b$shet_q<-scale(I(b$lev_ind^2))
b$sshap<-scale(b$shape)
b$sar<-scale(b$area)
b$sage<-scale(b$age)


zip1<-zeroinfl(Contatge ~ sviv + smuer + salt + ssim + shet + sshap + sar + sage
                + as.factor(Cluster) + as.factor(Tractament), 
                dist="poisson", link = "logit",data = b)
zip2<-zeroinfl(Contatge ~ sviv + smuer + salt + salt_q + ssim + shet + shet_q + sshap + sar + sage
               + as.factor(Cluster) + as.factor(Tractament), 
               dist="poisson", link = "logit",data = b)

AIC(zip1,zip2) #With quadratic is better,

#STRUCTURE OF THE BINOMIAL PART (0)

summary(zip2) #In the Zero inflated part I get significant: Height, quadratic height, Simpson, 
                                                            #het, quadratic het, Treatment

# These are variables that can affect the detectability of species and generate 0(?)
#In the binomial part I only keep the significant ones
zip3<-zeroinfl(Contatge ~ sviv + smuer + salt + salt_q + ssim + shet + shet_q + sshap + sar + sage
               + as.factor(Cluster) + as.factor(Tractament) |  # Model count
                 salt + salt_q + ssim + shet + shet_q + as.factor(Tractament), #Model 0
               dist="poisson", link = "logit",data = b)

AIC(zip1,zip2,zip3) # Seems like it is better to restrict the number of variables modelling the 0 

summary(zip4) #Treatment not significant in modelling 0. Remove it

zip4<-zeroinfl(Contatge ~ sviv + smuer + salt + salt_q + ssim + shet + shet_q + sshap + sar + sage
               + as.factor(Cluster) + as.factor(Tractament) |  # Model count
                 salt + salt_q + ssim + shet + shet_q , #Model 0
               dist="poisson", link = "logit",data = b)

AIC(zip1,zip2,zip3,zip4) 

#There was a slight(?) overdispersion. Check if the NB model is better. 
zinb<-zeroinfl(Contatge ~ sviv + smuer + salt + salt_q + ssim + shet + shet_q + sshap + sar + sage
               + as.factor(Cluster) + as.factor(Tractament) |  # Model count
                 salt + salt_q + ssim + shet + shet_q , #Model 0
               dist="negbin", link = "logit",data = b)

AIC(zip4,zinb) # NB doesnt deal better. So the slight overdispersion has been corrected by modelling the 0

#Compare normal poisson with zero inflated poisson (CAN YOU ACTUALLY DO THAT???) 
AIC(mp3,zip4) #Much better zero inflated 

#Use vuong test to check if ZIP or poisson is better
mp<-glm(Contatge ~ sviv + smuer + salt + salt_q + ssim + shet + shet_q + sshap + sar + sage
               + as.factor(Cluster) + as.factor(Tractament),
               family = "poisson", data = b)
vuong(mp,zip4) #En todos casos, model 2 da un better fit que el model 1 (ZIP MEJOR QUE POISSON)


##################################################################################
#3. MODEL SELECTION

library(MuMIn)
options(na.action=na.fail)
 #Full model used: zip4
#Model with year and zone as normal covariates
b$Zone<-as.factor(b$Zone)
b$Any<-as.factor(b$Any)
b$Cluster<-as.factor(b$Cluster)

zip<-zeroinfl(Contatge ~ sviv + smuer + salt + salt_q + ssim + shet + shet_q + sshap + sage
              + Cluster+ Any |  # Model count
                salt + salt_q + ssim + shet + shet_q , #Model 0
              dist="poisson", link = "logit",data = b, offset = log(area))



m_buo<-dredge(zip, beta = "none", evaluate = TRUE)

save(m_buo,file = "buoed_dred_notrat.RData") #Without treatment in the model, same significant variables

load("buoed_dred_notrat.RData")
load("buoed_dred_anyzone.RData")

m<-get.models(m_buo,subset = delta < 2)
avg_buo_zero<-model.avg(m)

summary(avg_buo_zero) 

#Significativo: 
#Count: Cluster, Area, Tratamiento
#Zero: Alt(2),Het(2),Sim


b$pred_buo <- predict(avg_buo_zero, b)  #Predice valores escalados
 
 
ggplot(b, aes(x = PromigAltura1Plot, y = pred_buo)) +
  geom_point() +
  #geom_line() +
  geom_smooth(method = "auto", formula = y ~ x , size = 1) +
  theme_bw() +
  theme(text = element_text(size=23)) +
  labs(x = "\nAltura (m)", y = "Abundancia alcaraván\n")

ggplot(b, aes(x = area, y = pred_buo)) +
  geom_point() +
  #geom_line() +
  geom_smooth(method = "auto", formula = y ~ x , size = 1) +
  theme_bw() +
  theme(text = element_text(size=20)) +
  labs(x = "\nArea (m2)", y = "Abundancia alcaraván\n")

ggplot(b, aes(x = lev_ind, y = pred_buo)) +
  geom_point() +
  #geom_line() +
  geom_smooth(method = "auto", formula = y ~ x , size = 1) +
  theme_bw() +
  theme(text = element_text(size=23)) +
  labs(x = "\nHeterogeneidad (lev.index)", y = "Abundancia alcaraván\n")

ggplot(b, aes(x = Simpson, y = pred_buo)) +
  geom_point() +
  #geom_line() +
  geom_smooth(method = "lm", formula = y ~ x , size = 1) +
  theme_bw() +
  theme(text = element_text(size=23)) +
  labs(x = "\nDiversidad florística (Sim.index)", y = "Abundancia alcaraván\n")

#Esto está bien plotearlo asi??????
b$Tractament<-as.character(b$Tractament)

b$Tractament[which(b$Tractament == "Acontrol")]<-"Control"
b$Tractament[which(b$Tractament == "Alfals")]<-"Alfalfa"
b$Tractament[which(b$Tractament == "Curronar")]<-"Compactar"
b$Tractament[which(b$Tractament == "Llaurar")]<-"Labrar"
b$Tractament[which(b$Tractament == "Picar i herbicidar")]<-"Picar y herbicidar"
b$Tractament[which(b$Tractament == "Picar y herbicidar")]<-"Pic y Herb"
b$Tractament[which(b$Tractament == "Pic y Herb")]<-"Pic + Herb"

ggplot(b, aes(x = Tractament, y = pred_buo)) +
  geom_point() +
  geom_boxplot() +
  theme_bw() +
  theme(text = element_text(size=23)) +
  labs(x = "\nTratamiento", y = "Abundancia alcaraván\n")

ggplot(b, aes(x = Cluster, y = pred_buo)) +
  geom_point() +
  geom_boxplot() +
  labs(x = "Diversidad plantas", y = "Torlit")

#####################################################################################
#NADA FUNCIONA. CREAR MODEL AVG GLM POISSON NORMAL CON RANDOM EFFECTS. Usar modelo mp3 como full model
library(lme4)

#Categorical as factor
b$Zone<-as.factor(b$Zone)
b$Any<-as.factor(b$Any)
b$Tractament<-as.factor(b$Tractament)
b$Cluster<-as.factor(b$Cluster)
b$age<-as.factor(b$age)
#Scaled variables
b$sviv<-scale(b$Recob_plotViu)
b$smuer<-scale(b$Recob_plotMort)
b$salt<-scale(b$PromigAltura1Plot)
b$salt_q<-scale(I(b$PromigAltura1Plot^2))
b$ssim<-scale(b$Simpson)
b$shet<-scale(b$lev_ind)
b$shet_q<-scale(I(b$lev_ind^2))
b$sshap<-scale(b$shape)
b$sar<-scale(b$area)
b$sage<-scale(b$age)


mp<-glmer(Contatge ~ sviv + salt + salt_q + ssim + shet + 
       shet_q + sar + sshap + Cluster + sage + Tractament + (1 | Zone) + (1| Any),
     data = b,
     family = poisson)
buo_glm<-dredge(mp, beta = "none", evaluate = TRUE)
save(buo_glm,file = "buo_glm.RData")

load("buo_glm.RData")
m<-get.models(buo_glm,subset = delta < 2)
avg_buo<-model.avg(m)
summary(avg_buo)
pred2 <- predict( avg_buo, type="response" ) #No funciona por los random effects

ggplot(b, aes(x = PromigAltura1Plot, y = pred2)) +
  geom_point() +
  #geom_line() +
  geom_smooth(method = "auto", formula = y ~ x , size = 1) +
  labs(x = "Altura)", y = "Torlit")

ggplot(b, aes(x = lev_ind, y = pred2)) +
  geom_point() +
  #geom_line() +
  geom_smooth(method = "auto", formula = y ~ x , size = 1) +
  labs(x = "Heterogeneidad (levin index)", y = "Torlit")

ggplot(b, aes(x = Simpson, y = pred2)) +
  geom_point() +
  #geom_line() +
  geom_smooth(method = "lm", formula = y ~ x , size = 1) +
  labs(x = "Diversidad plantas", y = "Torlit")

#Esto está bien plotearlo asi??????
ggplot(b, aes(x = Tractament, y = pred2)) +
  geom_point() +
  geom_boxplot() +
  labs(x = "Diversidad plantas", y = "Torlit")

ggplot(b, aes(x = Cluster, y = pred2)) +
  geom_point() +
  geom_boxplot() +
  labs(x = "Diversidad plantas", y = "Torlit")




##############################################################################
#Try backwards selection
library(mpath)

zip<-zeroinfl(Contatge ~ sviv + smuer + salt_q + ssim + shet + shet_q + sshap|  
                salt + shet,
              dist="poisson", link = "logit",data = b)

z<-be.zeroinfl(zip, data = b, dist="poisson", alpha=0.05)



?get.models


#######################################################################
#See effect of the treatment alone
b$Tractament<-as.factor(b$Tractament)
d<-glm(Contatge ~ Tractament,family = "poisson",data=b)
summary(d)
pred <- predict(d,newdata = data.frame(Tractament=sort(unique(b$Tractament))),type="response",se.fit =TRUE )

lcl <- pred$fit - 1.96*pred$se.fit
lch <- pred$fit + 1.96*pred$se.fit

plot(pred$fit~sort(unique(b$Tractament)), ylim=c(0,1.5),main = "BUOED")
segments(x0=c(1:6),
         x1=c(1:6),
         y0=lcl,
         y1=lch)
unique(b$Tractament)


#################################################################################
#Try mixed models!!!Random: Zone and Year (BUOED, MECAL, ALRUF)

library(glmmADMB)

b$Zone<-as.factor(b$Zone)
b$Any<-as.factor(b$Any)
b$Tractament<-as.factor(b$Tractament)
b$Cluster<-as.factor(b$Cluster)
g<-glmmadmb(Contatge ~ sviv + smuer + salt + salt_q + ssim + shet + shet_q + sshap + sar + sage
            + Cluster + Tractament + (1 | Zone) + (1| Any), data = b, family = "poisson",
            zeroInflation = TRUE)
summary(g)
#Model averaging
m_buo_ran<-dredge(g, beta = "none", evaluate = TRUE)
#save(m_buo_ran,file = "buoed_dred_ran.RData")
#load("buoed_dred_ran.RData")
m<-get.models(m_buo_ran,subset = delta < 2)
avg_buo<-model.avg(m)
summary(avg_buo)
pred2 <- predict( avg_ter, type="count", se.fit = TRUE ) #It doest return se!!!

