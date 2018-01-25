
#FIELD SCALE

setwd("C:/Users/ana.sanz/Documents/Datos/Datos barbechos arrendados/Variables")
f<-read.csv("Variables.csv",sep = ",",header=TRUE,fill = TRUE)
f$CF_A<-paste(f$Codi_Finca,f$Any,sep = "_")
f<-f[,c(1,19,2:14)]

#PERDIZ (ALRUF)
a<-f[which(f$EspecieObj == "ALRUF"),]
a<-a[complete.cases(a),]
a<-a[-which(duplicated(a[,2:15])),]
which(duplicated(a$CF_A)) #Eliminar 255 y 301 porque son observaciones repetidas de 2015 con ID proveniente del SIGPAC 2014
a<-a[-c(255,301),]

hist(a$Contatge) #Many 0

a<-a[,c(7:15)]
#Check collinearity between variables

panel.cor <- function(x, y, digits=2, prefix="", cex.cor) 
{
  usr <- par("usr"); on.exit(par(usr)) 
  par(usr = c(0, 1, 0, 1)) 
  r <- abs(cor(x, y)) 
  txt <- format(c(r, 0.123456789), digits=digits)[1] 
  txt <- paste(prefix, txt, sep="") 
  if(missing(cex.cor)) cex <- 0.8/strwidth(txt) 
  
  test <- cor.test(x,y) 
  # borrowed from printCoefmat
  Signif <- symnum(test$p.value, corr = FALSE, na = FALSE, 
                   cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
                   symbols = c("***", "**", "*", ".", " ")) 
  
  text(0.5, 0.5, txt, cex = cex * r) 
  text(.8, .8, Signif, cex=cex, col=2) 
}
pairs(a, lower.panel=panel.smooth, upper.panel=panel.cor)

#PERDIZ (ALRUF)
b<-f[which(f$EspecieObj == "BUOED"),]
b<-b[complete.cases(b),]
b<-b[-which(duplicated(b[,2:15])),]
which(duplicated(b$CF_A)) #Eliminar 255 y 301 porque son observaciones repetidas de 2015 con ID proveniente del SIGPAC 2014
b<-b[-c(255,301),]

hist(a$Contatge) #Many 0

a<-a[,c(7:15)]
#Check collinearity between variables

panel.cor <- function(x, y, digits=2, prefix="", cex.cor) 
{
  usr <- par("usr"); on.exit(par(usr)) 
  par(usr = c(0, 1, 0, 1)) 
  r <- abs(cor(x, y)) 
  txt <- format(c(r, 0.123456789), digits=digits)[1] 
  txt <- paste(prefix, txt, sep="") 
  if(missing(cex.cor)) cex <- 0.8/strwidth(txt) 
  
  test <- cor.test(x,y) 
  # borrowed from printCoefmat
  Signif <- symnum(test$p.value, corr = FALSE, na = FALSE, 
                   cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
                   symbols = c("***", "**", "*", ".", " ")) 
  
  text(0.5, 0.5, txt, cex = cex * r) 
  text(.8, .8, Signif, cex=cex, col=2) 
}
pairs(a, lower.panel=panel.smooth, upper.panel=panel.cor)

      