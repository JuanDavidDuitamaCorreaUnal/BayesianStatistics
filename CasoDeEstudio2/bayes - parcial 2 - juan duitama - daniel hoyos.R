
###########################Directorio, librerías e importación#################
setwd("C:/Users/juand/Desktop/Estadística/Semestres/Semestre 7/EstadisticaBayesiana/CasosDeEstudio/Directorio")

library(readxl)
library(readr)
library(stringr)
library(sqldf)
library(dplyr)
library(ggplot2)
library(metRology)


#Importación de la información

Personas<-read.csv("InfoCaso2")

#Resumen información

Resumen<-read.csv("ResumenDominios")

###########################Muestreadores########################

#SumaDeltas <- with(Personas, tapply(IngresoTotal, Dominio, length))

#Thetas

MuestrearTheta <- function(TamDominio,Dominio,obs,deltas,tau,mu){
  SumInvDeltas <- tapply(1/deltas,Dominio,sum)
  SumYijInvDeltas <- tapply(obs/deltas,Dominio,sum)
  Varianza <- 1/(SumInvDeltas+1/tau) 
  Media <- (SumYijInvDeltas+mu/tau)*Varianza
  NuevoTheta <- rnorm(n = TamDominio, mean = Media, sd = sqrt(Varianza))
  return(NuevoTheta)
}


#Sigmas

MuestrearSigma <- function(Dominio,tamanio,deltas,beta){
  SumInvDeltas <- tapply(1/deltas,Dominio,sum)
  Param1 <- 1.5*tamanio+0.5
  Param2 <- 0.5*(3*SumInvDeltas+beta)
  NuevoSigma <- rgamma(n = length(tamanio), Param1, Param2)
  return(NuevoSigma)
}


#Deltas

MuestrearDeltas <- function(y_ij,theta,sigma){
  Param <- 0.5*((y_ij-theta)^2+3*sigma)
  NuevoDelta <- 1/rgamma(n = length(y_ij),2,Param)
  return(NuevoDelta)
}


#Mus


MuestrearMus <- function(thetas,tau){
  Varianza <- 1/(25/tau+1/11.382)
  Media <- (sum(thetas)/tau+13.495/11.382)*Varianza
  NuevoMu <- rnorm(n = 1, Media, sqrt(Varianza))
  return(NuevoMu)
}


#Taus

MuestrearTaus <- function(thetas,mu){
  Param <- 0.5*(sum((thetas-mu)^2)+1.182)
  NuevoTau <- 1/rgamma(n = 1,13,Param)
  return(NuevoTau)
}



#Betas


MuestrearBetas <- function(sigmas){
  Param <- 0.5*(sum(sigmas)+1.182)
  NuevoBeta <- rgamma(n = 1,13,Param)
  return(NuevoBeta)
}



###########################Modelo 1#######################

#Hiperparámetros

mu0<-13.495
gamma20<-11.382
nu0<-1
sigma20<-1.182

############################ Repeticiones

B<-11000

############################ Constantes de los datos

ybarra<-mean(Personas$IngresoTotal)
varPer<-var(Personas$IngresoTotal)
sumaPers<-sum(Personas$IngresoTotal)
n<- length(Personas$IngresoTotal)


############################ Valores iniciales de theta y sigma2

set.seed(1729)
theta<-rnorm(n = 1, mean = ybarra, sd = sqrt(varPer))
sigma2<-1/rgamma(n = 1, shape = nu0/2, rate = nu0*varPer/2)

############################ Muestreador de Gibbs

# matriz para almacenar las muestras
MatrizModelo1 <- matrix(data = NA, nrow = B, ncol = 3)

set.seed(1729)
for(b in 1:B) {
  # actualizar el valor de theta
  gamma2Actual   <- 1/(1/gamma20 + n/sigma2)      
  muActual   <- (mu0/gamma20 + sumaPers/sigma2)*gamma2Actual
  theta <- rnorm(n = 1, mean = muActual, sd = sqrt(gamma2Actual))
  # actualizar el valor de sigma^2
  nun   <- nu0 + n
  s2n   <- (nu0*sigma20 + (n-1)*varPer + n*(ybarra - theta)^2)/nun
  sigma2<- 1/(rgamma(n = 1, shape = nun/2, rate = nun*s2n/2))
  logvero<-sum(dnorm(x=Personas$IngresoTotal, mean=theta, sd=sqrt(sigma2), log=T))
  # 2.3 almacenar
  MatrizModelo1[b,] <- c(theta, sigma2, logvero)
}

write.csv(MatrizModelo1[1001:11000,],"MuestrasModelo1")


###########################Modelo 2#######################

############################ Hiperparametros

mu_0<-13.495
gamma2_0<-11.382
eta_0<-1
tau2_0<-1.182
nu_0<-1
sigma2_0<-1.182

############################ Repeticiones

B<-11000

############################ Constantes de los datos

ybarra<-mean(Personas$IngresoTotal)
varPer<-var(Personas$IngresoTotal)
sumaPers<-sum(Personas$IngresoTotal)
n <- 3820
m <- 25


############################ Valores iniciales de thetas, mu, tao2 y sigma2

set.seed(1729)

Thetasj<-Resumen$Medias
Mu<-rnorm(n = 1, mean = mu_0, sd = sqrt(gamma2_0))
Tau2<-1/rgamma(n=1, shape = 0.5, rate =0.5*tau2_0 )
Sigma2<-1/rgamma(n = 1, shape = nu_0/2, rate = nu_0*varPer/2)


############################ Muestreador de Gibbs

# matriz para almacenar las muestras
MatrizModelo2 <- matrix(data = NA, nrow = B, ncol = 29)

set.seed(1729)

for(b in 1:B) {
  # actualizar los valores de thetas
  VarTheta   <- 1/(1/Tau2 + Resumen$Tamanios/Sigma2)      
  MedTheta   <- (Mu/Tau2 + Resumen$Tamanios*Resumen$Medias/Sigma2)*VarTheta
  Thetasj <- rnorm(n = 25, mean = MedTheta, sd = sqrt(VarTheta))
  # actualizar el valor de sigma^2
  s2n   <-(nu_0*sigma2_0 + sum((Resumen$Tamanios-1)*Resumen$Varianzas + Resumen$Tamanios*(Resumen$Medias - Thetasj)^2)) 
  Sigma2<- 1/(rgamma(n = 1, shape = 1910.5, rate = 0.5*s2n))
  
  # actualizar el valor de mu
  VarActual2   <- 1/(1/gamma2_0 + m/Tau2)      
  Mu  <- rnorm(n = 1, mean = VarActual2*(mu_0/gamma2_0 + m*mean(Thetasj)/Tau2), sd = sqrt(VarActual2))
  # actualizar el valor de tao ^2
  param2  <- (tau2_0 + sum((Thetasj-Mu)^2))
  tao2<- 1/(rgamma(n = 1, shape = 13, rate = param2/2))
  
  
  logvero<-sum(dnorm(x=Personas$IngresoTotal, mean=rep(Thetasj,Resumen$Tamanios), sd=sqrt(Sigma2), log=T))
  # Almacenar
  MatrizModelo2[b,] <- c(Thetasj, Sigma2, Mu, Tau2, logvero)
}

write.csv(MatrizModelo2[1001:11000,],"MuestrasModelo2")

###########################Modelo 3#######################

  # tamaños
  n <- 3820
  m <- 25
  # hiperparametros
  mu_0  <- 13.495 
  gamma2_0  <- 11.382
  eta_0 <- 1  
  tau2_0  <- 1.182
  nu <- 1  
  alpha_0  <- 1
  beta_0  <- 0.846
  #nus0 <- 1:50  # rango en p(nu | rest)
  
  # valores iniciales
  Thetasj <- Resumen$Medias
  Sigmas2j  <- Resumen$Varianzas  # sigma_j^2
  Mu    <- mean(Thetasj)
  Tau2  <- var(Thetasj)
  Nu    <- 1
  Ups2  <- mean(Resumen$Varianzas)  # sigma^2
  
  # almacenamiento
  MatrizModelo3 <- matrix(data = NA, nrow = 11000, ncol = 54)
  
  # cadena
  set.seed(1729)
  
  for (b in 1:B) {
    # actualizar theta
    vtheta <- 1/(1/Tau2 + Resumen$Tamanios/Sigmas2j)
    Thetasj  <- rnorm(n = 25, mean = vtheta*(Mu/Tau2 + Resumen$Tamanios*Resumen$Medias/Sigmas2j), sd = sqrt(vtheta))
    # actualizar sigma_j^2
    Sigmas2j <- 1/rgamma(n = 25, shape = 0.5*(Nu + Resumen$Tamanios), rate = 0.5*(Ups2 + (Resumen$Tamanios-1)*Resumen$Varianzas + Resumen$Tamanios*(Resumen$Medias - Thetasj)^2))
    # actualizar mu
    vmu <- 1/(1/gamma2_0 + 25/Tau2)
    Mu  <- rnorm(n = 1, mean = vmu*(mu_0/gamma2_0 + 25*mean(Thetasj)/Tau2), sd = sqrt(vmu))
    # actualizar tau2
    Tau2 <- 1/rgamma(n = 1, shape = 13, rate = 0.5*(tau2_0 + (m-1)*var(Thetasj) + m*(mean(Thetasj) - Mu)^2))
    # actualizar sigma^2
    Ups2 <- rgamma(n = 1, shape = 13, rate = 0.423 + 0.5*sum(1/Sigmas2j))
    #Log-Verosimilitud
    LogVero <- sum(dnorm(x = Personas$IngresoTotal, mean = rep(Thetasj, Resumen$Tamanios), sd = sqrt(rep(Sigmas2j, Resumen$Tamanios)), log = T))
    # almacenar
    MatrizModelo3[b,] <- c(Thetasj, Sigmas2j, Mu, Tau2,Ups2,LogVero)

  }
write.csv(MatrizModelo3[1001:11000,],"MuestrasModelo3")
  

###########################Modelo 4##########################

#Valores iniciales

NewDeltas <- rep(1,3820)
NewTheta <- 13
NewSigma <- 1.1

#Creación de matriz para almacenar las muestras
MatrizModelo4 <- matrix(data = NA, nrow = 11000, ncol = 4)


################Implementación muestreador de Gibbs

set.seed(1729)
for(i in 1:11000){
  NewDeltas <- MuestrearDeltas(Personas$IngresoTotal,NewTheta,NewSigma)
  NewTheta <- MuestrearTheta(1,rep("1",3820),Personas$IngresoTotal,NewDeltas,1.182,13.495)
  NewSigma <- MuestrearSigma(rep("1",3820),3820,NewDeltas,0.846)
  LogVero <- sum(dnorm(x = Personas$IngresoTotal, mean = NewTheta, sd = sqrt(NewDeltas), log = T))
  LogVeroDT <- sum(dt.scaled(x = Personas$IngresoTotal,df=3 ,mean = NewTheta, sd = sqrt(NewSigma), log = T))

  MatrizModelo4[i,] <- c(NewTheta,NewSigma,LogVero,LogVeroDT)
}

#Guardar matriz en disco duro
write.csv(MatrizModelo4[1001:11000,],"MuestrasModelo4")



###########################Modelo 5###############################

#Valores iniciales

set.seed(1729)

NewDeltas <- rep(1,3820)
NewThetas <- Resumen$Medias
NewSigma <- 1.1
NewMu <- rnorm(1,13.495,sqrt(11.382))
NewTau <- 1/rgamma(1,0.5,0.5*1.182)

#Creación de matriz para almacenar las muestras

MatrizModelo5 <- matrix(data = NA, nrow = 11000, ncol = 30)

#Implementación Muestreador de Gibbs


set.seed(1729)
for(i in 1:11000){
  NewDeltas <- MuestrearDeltas(Personas$IngresoTotal,rep(NewThetas,Resumen$Tamanios),NewSigma)#1/rgamma(3820,2,)
  NewThetas <- MuestrearTheta(25,Personas$Dominio,Personas$IngresoTotal,NewDeltas,NewTau,NewMu)
  NewSigma <- MuestrearSigma(rep("1",3820),3820,NewDeltas,0.486)
  NewMu <- MuestrearMus(NewThetas,NewTau)
  NewTau <- MuestrearTaus(NewThetas,NewMu)
  LogVero <- sum(dnorm(x = Personas$IngresoTotal, mean = rep(NewThetas, Resumen$Tamanios), sd = sqrt(NewDeltas), log = T))
  LogVeroDT <- sum(dt.scaled(x = Personas$IngresoTotal,df=3 ,mean = rep(NewThetas,Resumen$Tamanios), sd = sqrt(NewSigma), log = T))
  MatrizModelo5[i,] <- c(NewThetas,NewSigma,NewMu,NewTau,LogVero,LogVeroDT)
  
}

#Guardar matriz en disco duro
write.csv(MatrizModelo5[1001:11000,],"MuestrasModelo5")



###########################Modelo 6######################################

#Valores iniciales

set.seed(1729)

NewDeltas <- rep(1,3820)
NewThetas <- Resumen$Medias
NewSigma <- Resumen$Varianzas
NewMu <- rnorm(1,13.495,sqrt(11.382))
NewTau <- 1/rgamma(1,0.5,0.5*1.182)
NewBeta <- rgamma(1,0.5,1.182*0.5)

#Creación de matriz para almacenar las muestras

MatrizModelo6 <- matrix(data = NA, nrow = 11000, ncol = 55)

#Implementación Muestreador de Gibbs


set.seed(1729)
for(i in 1:11000){
  NewDeltas <- MuestrearDeltas(Personas$IngresoTotal,rep(NewThetas,Resumen$Tamanios),NewSigma)
  NewThetas <- MuestrearTheta(25,Personas$Dominio,Personas$IngresoTotal,NewDeltas,NewTau,NewMu)
  NewSigma <- MuestrearSigma(Personas$Dominio,Resumen$Tamanios,NewDeltas,NewBeta)
  NewMu <- MuestrearMus(NewThetas,NewTau)
  NewTau <- MuestrearTaus(NewThetas,NewMu)
  NewBeta <- MuestrearBetas(NewSigma)
  LogVero <- sum(dnorm(x = Personas$IngresoTotal, mean = rep(NewThetas, Resumen$Tamanios), sd = sqrt(NewDeltas), log = T))
  LogVeroDT <- sum(dt.scaled(x = Personas$IngresoTotal,df=3 ,mean = rep(NewThetas,Resumen$Tamanios), sd = sqrt(rep(NewSigma,Resumen$Tamanios)), log = T))
  MatrizModelo6[i,] <- c(NewThetas,NewSigma,NewMu,NewTau,NewBeta,LogVero,LogVeroDT)
  
}

#Guardar matriz en disco duro
write.csv(MatrizModelo6[1001:11000,],"MuestrasModelo6")


###########################Importación matrices resultantes######################


MuestrasModelo1<-read.csv("MuestrasModelo1")
names(MuestrasModelo1)<-c("indice","Theta","Sigma","LogVerosimilitud")

MuestrasModelo2<-read.csv("MuestrasModelo2")
names(MuestrasModelo2)<-c("indice",paste("Theta",names(table(Personas$Dominio)),sep="-"),"Sigma","Mu","Tau2","LogVerosimilitud")

MuestrasModelo3<-read.csv("MuestrasModelo3")
names(MuestrasModelo3)<-c("indice",paste("Theta",names(table(Personas$Dominio)),sep="-"),paste("Sigma",names(table(Personas$Dominio)),sep="-"),"Mu","Tau2","SigmaSup","LogVerosimilitud")

MuestrasModelo4<-read.csv("MuestrasModelo4")
names(MuestrasModelo4)<-c("indice","Theta","Sigma","LogVerosimilitud","LogVerosimilitudReal")

MuestrasModelo5<-read.csv("MuestrasModelo5")
names(MuestrasModelo5)<-c("indice",paste("Theta",names(table(Personas$Dominio)),sep="-"),"Sigma","Mu","Tau2","LogVerosimilitud","LogVerosimilitudReal")

MuestrasModelo6<-read.csv("MuestrasModelo6")
names(MuestrasModelo6)<-c("indice",paste("Theta",names(table(Personas$Dominio)),sep="-"),paste("Sigma",names(table(Personas$Dominio)),sep="-"),"Mu","Tau2","Beta","LogVerosimilitud","LogVerosimilitudReal")



#################Tamaños efectivos de muestra

coda::effectiveSize(MuestrasModelo1[,2:3])
#Me da que la varianza tiene poquitos


coda::effectiveSize(MuestrasModelo2[,2:29])
#Me da que la varianza tiene poquitos


coda::effectiveSize(MuestrasModelo3[,2:54])
#Me da que la varianza tiene poquitos



coda::effectiveSize(MuestrasModelo4[,2:3])
#Me da que la varianza tiene poquitos


coda::effectiveSize(MuestrasModelo5[,2:29])
#Me da que la varianza tiene poquitos


coda::effectiveSize(MuestrasModelo6[,2:54])
#Me da que la varianza tiene poquitos

###########################Punto 2#########################

#Log verosimilitudes reales superpuestas



#Gráficos de las logverosimilitudes bien bonito xd

opar <- par(no.readonly = TRUE)

# Cambiar los márgenes del gráfico (el cuarto es el margen derecho)
par(mar = c(5, 5, 4, 6))

plot(MuestrasModelo4$LogVerosimilitudReal,type = "p", pch = ".", cex = 2,add=0 , col = "#00C5CD" , xlim = c(0,10000) , ylim = c(-5750,-5550) , xlab = "Iteración", ylab = "Log-Verosimilitud", main = "Log-Verosimilitudes")
points(MuestrasModelo6$LogVerosimilitudReal,col="red",type="p",pch= ".", cex = 2)
points(MuestrasModelo5$LogVerosimilitudReal,col="blue",type="p",pch= ".", cex = 1.5)
points(MuestrasModelo1$LogVerosimilitud,col="yellow",type="p",pch= ".", cex = 2)
points(MuestrasModelo2$LogVerosimilitud,col="green",type="p",pch= ".", cex = 2)
points(MuestrasModelo3$LogVerosimilitud,col="purple",type="p",pch= ".", cex = 2)
grid()
legend("right",legend=paste("Modelo ",1:6,sep=""),fill=c("yellow","green","purple","#00C5CD","blue","red"),bty = "n",cex = 0.9, inset = c(-0.2, 0),xpd = TRUE)

on.exit(par(opar))

###########################Punto 3#############################


#######Criterios de Información

#Modelo1

LP1 <- as.numeric(MuestrasModelo1$LogVerosimilitud)
theta_hat  <- mean(MuestrasModelo1$Theta)
sigma2_hat <- mean(MuestrasModelo1$Sigma)
lpyth_m1   <- sum(dnorm(x = Personas$IngresoTotal, mean = theta_hat, sd = sqrt(sigma2_hat), log = T))
pDIC_m1    <- 2*(lpyth_m1 - mean(LP1))
dic_m1     <- -2*lpyth_m1 + 2*pDIC_m1

#Modelo 2

LP2 <- as.numeric(MuestrasModelo2$LogVerosimilitud)
theta_hat  <- colMeans(MuestrasModelo2[,2:26])
sigma2_hat <- mean(MuestrasModelo2$Sigma)
lpyth_m2   <- sum(dnorm(x = Personas$IngresoTotal, mean = rep(theta_hat, Resumen$Tamanios), sd = sqrt(sigma2_hat), log = T))
pDIC_m2    <- 2*(lpyth_m2 - mean(LP2))
dic_m2     <- -2*lpyth_m2 + 2*pDIC_m2


#Modelo 3


LP3 <- as.numeric(MuestrasModelo3$LogVerosimilitud)
theta_hat  <- colMeans(MuestrasModelo3[,2:26])
sigma2_hat <- colMeans(MuestrasModelo3[,27:51])
lpyth_m3   <- sum(dnorm(x = Personas$IngresoTotal, mean = rep(theta_hat, Resumen$Tamanios), sd = sqrt(rep(sigma2_hat, Resumen$Tamanios)), log = T))
pDIC_m3    <- 2*(lpyth_m3 - mean(LP3))
dic_m3     <- -2*lpyth_m3 + 2*pDIC_m3



#Modelo 4

LP4 <- as.numeric(MuestrasModelo4$LogVerosimilitudReal)
theta_hat  <- mean(MuestrasModelo4$Theta)
sigma2_hat <- mean(MuestrasModelo4$Sigma)
lpyth_m4   <- sum(dt.scaled(x = Personas$IngresoTotal,3, mean = theta_hat, sd = sqrt(sigma2_hat), log = T))
pDIC_m4    <- 2*(lpyth_m4 - mean(LP4))
dic_m4     <- -2*lpyth_m4 + 2*pDIC_m4

#Modelo 5

LP5 <- as.numeric(MuestrasModelo5$LogVerosimilitudReal)
theta_hat  <- colMeans(MuestrasModelo5[,2:26])
sigma2_hat <- mean(MuestrasModelo5$Sigma)
lpyth_m5   <- sum(dt.scaled(x = Personas$IngresoTotal,3, mean = rep(theta_hat, Resumen$Tamanios), sd = sqrt(sigma2_hat), log = T))
pDIC_m5    <- 2*(lpyth_m5 - mean(LP5))
dic_m5     <- -2*lpyth_m5 + 2*pDIC_m5


#Modelo 6


LP6 <- as.numeric(MuestrasModelo6$LogVerosimilitudReal)
theta_hat  <- colMeans(MuestrasModelo6[,2:26])
sigma2_hat <- colMeans(MuestrasModelo6[,27:51])
lpyth_m6   <- sum(dt.scaled(x = Personas$IngresoTotal,3, mean = rep(theta_hat, Resumen$Tamanios), sd = sqrt(rep(sigma2_hat, Resumen$Tamanios)), log = T))
pDIC_m6    <- 2*(lpyth_m6 - mean(LP6))
dic_m6     <- -2*lpyth_m6 + 2*pDIC_m6


#Tabla con los Dic y Pdic

cbind(c("","DIC","p_DIC"),rbind(c(paste("Modelo",1:6,sep=" ")),c(dic_m1,dic_m2,dic_m3,dic_m4,dic_m5,dic_m6),c(pDIC_m1,pDIC_m2,pDIC_m3,pDIC_m4,pDIC_m5,pDIC_m6)))

###########################Punto 4##########################

ThetasBogota<-MuestrasModelo6$`Theta-BOGOTA`
SigmasBogota<-MuestrasModelo6$`Sigma-BOGOTA`
MatrizPPPm6<-matrix(data = NA, nrow = 10000, ncol = 6)
set.seed(1729)
for(j in 1:10000){
  Simulacion <- rt.scaled(n=165,df=3,mean=ThetasBogota[j],sd=sqrt(SigmasBogota[j]))
  MatrizPPPm6[j,]<- c(mean(Simulacion),median(Simulacion),sd(Simulacion),sd(Simulacion)/mean(Simulacion),max(Simulacion)-min(Simulacion),IQR(Simulacion))
}
colnames(MatrizPPPm6)<-c("Media","Mediana","Desviacion","CoeficientedeVariacion","Rango","RangoIntercuartilico")


#Comparación de Histograma de Medias con Media real

hist(x = MatrizPPPm6[,1] , freq = F, col = "gray90", border = "gray90", xlim= c(13,15) ,xlab = "Media", ylab = "Densidad", main = "Bondad de ajuste del modelo 6 con media como estadístico de prueba para\nlos ingresos en escala logarítmica de Bogotá en el año 2021")
lines(density(MatrizPPPm6[,1]), col = "#00FF00", lwd = 2)
abline(v = 13.8, col = "red", lwd = 2, lty = 1)
grid()
legend("right", legend = c("Distribución de las medias", "Media observada"), col = c("#00FF00", "red"), lty = 1, lwd = 2, bty = "n",cex=1)


#Comparación de Histograma de Medianas con Mediana real

hist(x = MatrizPPPm6[,2] , freq = F, col = "gray90", border = "gray90", xlim= c(13,15) ,xlab = "Mediana", ylab = "Densidad", main = "Bondad de ajuste del modelo 6 con mediana como estadístico de prueba para\nlos ingresos en escala logarítmica de Bogotá en el año 2021")
lines(density(MatrizPPPm6[,2]), col = "#00FF00", lwd = 2)
abline(v = median(Personas[348:512,2]), col = "red", lwd = 2, lty = 1)
grid()
legend("right", legend = c("Distribución de las medianas", "Mediana observada"), col = c("#00FF00", "red"), lty = 1, lwd = 2, bty = "n",cex=1)


#Comparación de Histograma de Desviación con Desviación real

hist(x = MatrizPPPm6[,3] , freq = F,breaks=250, col = "gray90", border = "gray90", xlim= c(0.6,2),ylim=c(0,2.5) ,xlab = "Desviación", ylab = "Densidad", main = "Bondad de ajuste del modelo 6 con Desviación como estadístico de prueba para\nlos ingresos en escala logarítmica de Bogotá en el año 2021")
lines(density(MatrizPPPm6[,3]), col = "#00FF00", lwd = 2)
abline(v = sd(Personas[348:512,2]), col = "red", lwd = 2, lty = 1)
grid()
legend("right", legend = c("Distribución de las desviaciones", "Desviación observada"), col = c("#00FF00", "red"), lty = 1, lwd = 2, bty = "n",cex=1)



#Comparación de Histograma de Coeficiente de Variación con CV real

hist(x = MatrizPPPm6[,4] , freq = F, col = "gray90", border = "gray90", breaks=250,xlim= c(0.04,0.20),ylim=c(0,35) ,xlab = "Coeficiente de Variación", ylab = "Densidad", main = "Bondad de ajuste del modelo 6 con Coeficiente de Variación como estadístico\nde prueba para los ingresos en escala logarítmica de Bogotá en el año 2021")
grid()
lines(density(MatrizPPPm6[,4]), col = "#00FF00", lwd = 2)
abline(v = sd(Personas[348:512,2])/mean(Personas[348:512,2]), col = "red", lwd = 2, lty = 1)
legend("right", legend = c("Distribución de los Coef. de Variación", "Coef. de variación observado"), col = c("#00FF00", "red"), lty = 1, lwd = 2, bty = "n",cex=1)



#Comparación de Histograma del Rango con Rango real

hist(x = MatrizPPPm6[,5] , freq = F, col = "gray90", border = "gray90",breaks=250 ,xlim= c(0,35),xlab = "Rango", ylab = "Densidad", main = "Bondad de ajuste del modelo 6 con Rango como estadístico de prueba para\nlos ingresos en escala logarítmica de Bogotá en el año 2021")
grid()
lines(density(MatrizPPPm6[,5]), col = "#00FF00", lwd = 2)
abline(v = max(Personas[348:512,2])-min(Personas[348:512,2]), col = "red", lwd = 2, lty = 1)
legend("right", legend = c("Distribución de los Rangos", "Rango observado"), col = c("#00FF00", "red"), lty = 1, lwd = 2, bty = "n",cex=1)




#Comparación de Histograma del Rango Intercuartilico con Rango Intercuartilico real

hist(x = MatrizPPPm6[,6] , freq = F, col = "gray90", border = "gray90",breaks=100 ,xlim= c(0.6,1.8),ylim=c(0,3.5),xlab = "IQR", ylab = "Densidad", main = "Bondad de ajuste del modelo 6 con IQR como estadístico de prueba para\nlos ingresos en escala logarítmica de Bogotá en el año 2021")
grid()
lines(density(MatrizPPPm6[,6]), col = "#00FF00", lwd = 2)
abline(v = IQR(Personas[348:512,2]), col = "red", lwd = 2, lty = 1)
legend("right", legend = c("Distribución de los IQR", "IQR observado"), col = c("#00FF00", "red"), lty = 1, lwd = 2, bty = "n",cex=1)



PPPs <- list(Media=sum(MatrizPPPm6[,1]>(Resumen$Medias[3]))/10000,Mediana=sum(MatrizPPPm6[,2]>(median(Personas[348:512,2])))/10000,Desviacion=sum(MatrizPPPm6[,3]>(sd(Personas[348:512,2])))/10000,CoefVar=sum(MatrizPPPm6[,4]>(sd(Personas[348:512,2])/mean(Personas[348:512,2])))/10000,Rango=sum(MatrizPPPm6[,5]>(max(Personas[348:512,2])-min(Personas[348:512,2])))/10000,IQR=sum(MatrizPPPm6[,6]>(IQR(Personas[348:512,2])))/10000)

###########################Punto 5################################


# Imprimir el resultad
library(Hmisc)
THETA <- MuestrasModelo6
ids2 <- c("Armenia","Barranquilla","Bogotá","Bucaramanga","Cali","Cartagena","Cúcuta","Florencia","Ibagué","Manizales","Medellín","Montería","Neiva","Pasto","Pereira","Popayán","Quibdó","Resto Urbano","Riohacha","Rural","Santa Marta","Sincelejo","Tunja","Valledupar","Villavicencio")
that  <- colMeans(THETA[,2:26])
ic1   <- apply(X = THETA[,2:26], MARGIN = 2, FUN = function(x) quantile(x, c(0.025,0.975)))
ranking <- order(that) 
ids2 <- ids2[ ranking]
that <- that[ ranking]
ic1  <- ic1 [,ranking]
colo <- rep(2,25)
colo[which(ic1[1,]>13.830)] <- 1
colo[which(ic1[2,]<13.830)] <- 3
colo <- c("#008B00","black","#CD3333")[colo]
dev.new()
par(mar = c(5, 7, 2, 4))
plot(NA, NA, xlab = "Salarios en escala logarítmica", ylab = "", main = "Ranking Bayesiano Modelo 6", xlim = c(12.83,14.2), ylim = c(1,25), cex.axis = 0.75, yaxt = "n")
axis(side = 2, at = 1:25, labels = ids2, las = 1)
abline(v = 13.830,  col = "#97FFFF", lwd = 3)
abline(h = 1:25, col = "lightgray", lwd = 1,lty=2)
for (j in 1:25) {
  segments(x0 = ic1[1,j], y0 = j, x1 = ic1[2,j], y1 = j, col = colo[j])
  lines(x = that[j], y = j, type = "p", pch = 16, cex = 0.8, col = colo[j])
}
legend("bottomright", legend = c("SMLMV 2022"), col = c("#97FFFF"), lty = 1, lwd = 2, bty = "n",cex=0.7)



###########################Punto 6##################################

#Medellín

#Estimación media(Valor esperado)
exp(mean(MuestrasModelo6$`Theta-MEDELLIN`))
#Estimación desviación estándar
exp(mean(sqrt(3*MuestrasModelo6$`Sigma-MEDELLIN`)))
#Estimación coeficiente de variación
mean(sqrt(3*MuestrasModelo6$`Sigma-MEDELLIN`)/MuestrasModelo6$`Theta-MEDELLIN`)


#Manizales 

#Estimación media(Valor esperado)
exp(mean(MuestrasModelo6$`Theta-MANIZALES`))
#Estimación desviación estándar
exp(mean(sqrt(3*MuestrasModelo6$`Sigma-MANIZALES`)))
#Estimación coeficiente de variación
mean(sqrt(3*MuestrasModelo6$`Sigma-MANIZALES`)/MuestrasModelo6$`Theta-MANIZALES`)



#Bogotá

#Estimación media(Valor esperado)
exp(mean(MuestrasModelo6$`Theta-BOGOTA`))
#Estimación desviación estándar
exp(mean(sqrt(3*MuestrasModelo6$`Sigma-BOGOTA`)))
#Estimación coeficiente de variación
mean(sqrt(3*MuestrasModelo6$`Sigma-BOGOTA`)/MuestrasModelo6$`Theta-BOGOTA`)


#Tunja

#Estimación media(Valor esperado)
exp(mean(MuestrasModelo6$`Theta-TUNJA`))
#Estimación desviación estándar
exp(mean(sqrt(3*MuestrasModelo6$`Sigma-TUNJA`)))
#Estimación coeficiente de variación
mean(sqrt(3*MuestrasModelo6$`Sigma-TUNJA`)/MuestrasModelo6$`Theta-TUNJA`)


#Bucaramanchester

#Estimación media(Valor esperado)
exp(mean(MuestrasModelo6$`Theta-BUCARAMANGA`))
#Estimación desviación estándar
exp(mean(sqrt(3*MuestrasModelo6$`Sigma-BUCARAMANGA`)))
#Estimación coeficiente de variación
mean(sqrt(3*MuestrasModelo6$`Sigma-BUCARAMANGA`)/MuestrasModelo6$`Theta-BUCARAMANGA`)



###########################Punto 7############################

MatrizNoCentrada <-cbind(MuestrasModelo6[,2:26],sqrt(MuestrasModelo6[,27:51]))

EstimPunt <- matrix(data=NA,nrow=25,ncol=2)
for(a in 1:25){
  EstimPunt[a,] <- c(mean(MatrizNoCentrada[,a]),mean(MatrizNoCentrada[,25+a]))
}
rownames(EstimPunt)<- ids2
colnames(EstimPunt)<-c("Theta","Sigma")

#Prueba final de k means

set.seed(1729)

# Aplicar clustering con k-means y 4 grupos
resultado <- kmeans(EstimPunt, centers = 4)

# Obtener las asignaciones de grupos
asignaciones <- resultado$cluster

# Imprimir las asignaciones de grupos
print(asignaciones)


#Dendograma


# Generar datos de ejemplo
set.seed(1729)

# Calcular la matriz de distancias
distancias <- dist(EstimPunt)

# Aplicar clustering jerárquico
resultado <- hclust(distancias, method = "complete")

# Realizar el corte en 4 grupos
grupos <- cutree(resultado, k = 4)

# Visualizar el dendrograma
plot(resultado,main="Dendograma de segmentación en 4 grupos",xlab = "Dominios",ylab="Altura",ylim=c(0,1))
rect.hclust(resultado, k = 4,border=c("green","purple","blue","red"))
legend("topright", legend = c(paste0("Grupo ",1:4)), col = c("blue","purple" ,"red","green"), lty = 1, lwd = 2, bty = "n",cex=0.7)

