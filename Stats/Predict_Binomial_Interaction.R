set.seed(666)
x1 = rnorm(1000)           # some continuous variables 
x2 = rbinom(1000,1,0.5)
z = 1 + 2*x1 + 3*x2        # linear combination with a bias
pr = 1/(1+exp(-z))         # pass through an inv-logit function

plot(pr~x1)

y = rbinom(1000,1,pr)      # bernoulli response variable
  
  #now feed it to glm:
  df = data.frame(y=y,x1=x1,x2=x2)
  mod <-glm(y~x1+x2,data=df,family="binomial")
summary(mod)    

inv.logit <- function(x){1/(1+exp(-x)) }


prpred <- inv.logit(mod$coefficients[1] + mod$coefficients[2]*x1 + mod$coefficients[3]*x2)

plot(prpred~pr)


x1p <- seq(-4,4, by=0.01)

prprednew <- inv.logit(mod$coefficients[1] + mod$coefficients[2]*x1p + mod$coefficients[3]*0)

plot(prprednew~x1p)

prprednew1 <- inv.logit(mod$coefficients[1] + mod$coefficients[2]*x1p + mod$coefficients[3]*1)

points(prprednew1~x1p, col="red")



##########################################
set.seed(666)
x1 = rnorm(1000)           # some continuous variables 
x2a<- t(rmultinom(1000,size =1,c(0.5,0.25,0.25)))#,0.25,0.25) ))

#x2 = rbinom(1000,1,0.25)

z = 1 + 2*x1 + 3* x2a[,2] + 2*x2a[,2]*x1 + -3* x2a[,3]     # linear combination with a bias
pr = 1/(1+exp(-z))         # pass through an inv-logit function
y = rbinom(1000,1,pr)      # bernoulli response variable

#now feed it to glm:
df = data.frame(y=y,x1=x1,x2=x2a[,2],x3=x2a[,3] )
mod <-glm( y~x1*x2 +x3 ,data=df,family="binomial")
summary(mod)    

inv.logit <- function(x){1/(1+exp(-x)) }

# 
# prpred <- inv.logit(mod$coefficients[1] + mod$coefficients[2]*x1 + mod$coefficients[3]*x2  + mod$coefficients[4]*x2*x1 )
# plot(prpred~pr)


x1p <- seq(-4,4, by=0.01)

 prprednew <- inv.logit(mod$coefficients[1] + mod$coefficients[2]*x1 + mod$coefficients[3]*0 + mod$coefficients[4]*1+ mod$coefficients[5]*x1*0 )
plot(prprednew~x1)

prprednew1 <- inv.logit(mod$coefficients[1] + mod$coefficients[2]*x1 + mod$coefficients[3]*1 + mod$coefficients[4]*0 + mod$coefficients[5]*x1*1)
points(prprednew1~x1, col="red")






plot(prpred~ )


plot(y~x1)
plot(y~x2)




