#########################CASO DE ESTUDIO NÚMERO 3
#Universidad Nacional de Colombia
#2023-1
#Juan David Duitama Correa
#Daniel Hoyos Mateus


################Directorio, librerías utilizadas e importación de datos################
setwd("C:/Users/juand/Desktop/Estadística/Semestres/Semestre 7/EstadisticaBayesiana/CasosDeEstudio/Directorio")
library(ggplot2)
library(pscl)
library(mnormt)
library(VGAM)
library(miscTools)
library(nlme)
library(coda)
source("regression_gprior.R")#Importo las funciones creadas por Hoff 


DiabetesTrain<-read.csv("EntrenamientoCaso3")
DiabetesTest<-read.csv("TesteoCaso3")

#########Matriz X y Vector respuestas

YTrain<-DiabetesTrain[,2]
XTrain<-as.matrix(DiabetesTrain[,3:12])

YTest<-DiabetesTest[,2]
XTest<-as.matrix(DiabetesTest[,3:12])

#OLS
YTY <- sum(YTrain^2)
XTY <- t(as.matrix(XTrain))%*%YTrain 
XTX <- t(as.matrix(XTrain))%*%as.matrix(XTrain)
Hat <- as.matrix(XTrain)%*%solve(XTX)%*%t(as.matrix(XTrain))
VarOls <- as.numeric((t(YTrain)%*%(diag(1,342,342)-Hat)%*%YTrain)/(342-10))

#Funciones auxiliares realizadas por el profesor

Traza <- function(X) sum(diag(X))

rmvnorm <- function(n,mu,Sigma)#Sigma es una matriz 
{
  p<-length(mu)#mu es un vector
  res<-matrix(0,nrow=n,ncol=p)#matriz de ceros con n filas y p columnas
  if( n>0 & p>0 ) {
    E<-matrix(rnorm(n*p),n,p)#LLenado de una matriz con np números provenientes de una normal estándar
    res<-t(  t(E%*%chol(Sigma)) +c(mu))#chol es descomposición de Cholesky
  }
  res
}

#########################Modelo 1: Ridge#########################


#Hiperparámetros

Nu_0 <- 1
Sigma2_0 <- VarOls
a_Lambda <- 1
b_Lambda <- 2


# Muestreador de Gibbs con Distribuciones condicionales completas conocidas

#############Valores iniciales(Previa difusa independiente)

set.seed(1729)
Betas <- rep(0,10)
Sigma <- 10
Lambda <- rgamma(1,a_Lambda,b_Lambda)

#Muestreador

S <- 10000 
MatrizParam1 <- matrix(NA, S, 13) 
set.seed(1729)
for(s in 1:S) {
  
  #Muestrear Betas
  VarBetas <- Sigma*solve(diag(Lambda,10,10)+XTX)
  MediaBetas <- VarBetas%*%(XTY/Sigma)
  Betas <- c(mvtnorm::rmvnorm(1, MediaBetas, VarBetas))
  MatrizParam1[s,1:10] <- Betas
  
  #Muestrear Sigma
  SigmaParam <- sum((YTrain-as.matrix(XTrain)%*%Betas)^2) + Lambda*sum(Betas^2)+ Sigma2_0
  Sigma <- 1/rgamma(1, 353/2, as.numeric(SigmaParam/2))
  MatrizParam1[s,11] <- Sigma
  
  #Muestrear Lambda
  LambdaParam <- sum(Betas^2)/(2*Sigma) + b_Lambda
  Lambda <- as.numeric(rgamma(1,a_Lambda+5,as.numeric(LambdaParam)))
  MatrizParam1[s,12] <- Lambda
  
  #Log-Verosimilitud
  LogVero <- mvtnorm::dmvnorm(x=YTrain, mean=as.matrix(XTrain)%*%Betas, sigma=Sigma*diag(1,342,342), log=T)
  MatrizParam1[s,13] <- LogVero
}

#Tiempo de ejecución: 4 minutos aproximadamente

write.csv(MatrizParam1,"MuestrasModeloRidge")

#########################Modelo 2: Lasso#########################

#Hiperparámetros

Tau_0 <- 5
Nu_0 <- 1
Sigma2_0 <- VarOls

#Valores iniciales

Betas <- rep(0,10)
Sigma2 <- 100
Phis <- rep(1,10)

#Muestreador de Gibbs

S <- 10000
MatrizParam2 <- matrix(NA,S,12)
count <- 0
set.seed(1729)
for(s in 1:S){
  #Anuncio avance de porcentaje de avance
  if (s %% 100 == 0) {
    count<-count+1
    cat('Avance:', count, "%")
  }
  #Muestrear Betas
  VarBetas <- solve(XTX/Sigma2+diag(1/Phis,10,10))
  MediaBetas <- VarBetas%*%XTY/Sigma2
  Betas <- c(mvtnorm::rmvnorm(1,mean=MediaBetas,sigma = VarBetas))
  MatrizParam2[s,1:10]<-Betas
  
  #Muestrear Sigma
  ParamSigma <- Nu_0*Sigma2_0+sum((YTrain-XTrain%*%Betas)^2)
  Sigma2 <- 1/rgamma(1,343/2,ParamSigma/2)
  MatrizParam2[s,11]<-Sigma2
  
  #Muestrear Phis
  Phis <- rep(0, 10)
  for (i in seq(10)) {
    Phis[i] <- rinv.gaussian(1, mu = 1/(abs(Betas[i])*Tau_0),lambda =  1/(Betas[i]^2))
  }
  
  #Almacenar Logverosimilitud
  LogVero <- LogVero <- mvtnorm::dmvnorm(x=YTrain, mean=XTrain%*%Betas, sigma=Sigma2*diag(1,342,342), log=T)
  MatrizParam2[s,12] <- LogVero
}


#Tiempo de ejecución: 3 minutos y 49 segundos aprox

write.csv(MatrizParam2,"MuestrasModeloLasso")


#########################Modelo 3: Errores Correlacionados#########################

#Matriz de los exponentes de rho 
DY <-abs(outer( (1:342),(1:342) ,"-"))

#Hiperparámetros del modelo

Tau_0 <- diag(1/50,10,10)
Nu_0 <- 1
Sigma2_0 <- VarOls
a_rho <- 0
b_rho <- 1

#Valores Iniciales
Betas <- rep(0,10)
Sigma2<- 100
Phi <- acf(lm(YTrain ~ -1+XTrain)$res,plot=FALSE)$acf[2]

#Muestreador de Gibbs

set.seed(1729)
S <- 50000 # numero de iteraciones
odens <-S/10000  # informacion,el denominador es lo que al final deseo obtener
MatrizParam3 <- matrix(NA,10000,13)    # almacenamiento
ac    <-0       # tasa de aceptacion
count <- 0

#50000 iteraciones, de las cuales se guardará el 20%, es decir 10000

# cadena
for (s in 1:S) {
  # simular beta
  Cor    <- Phi^DY   
  iCor   <- solve(Cor)
  V.beta <- solve( t(XTrain)%*%iCor%*%XTrain/Sigma2 + Tau_0)
  E.beta <- V.beta%*%( t(XTrain)%*%iCor%*%YTrain/Sigma2)
  Betas   <- t(rmvnorm(1,E.beta,V.beta))
  # simular sigma^2
  XBetas <- XTrain%*%Betas
  Sigma2 <- 1/rgamma(1,171.5,(Sigma2_0+t(YTrain)%*%iCor%*%YTrain+t(XBetas)%*%iCor%*%(-2*YTrain+XBetas))/2 )
  # simular rho (metropolis)
  # 1. propuesta
  phi.p <- abs(runif(1,Phi-.15,Phi+.15))#Aquí el delta debe cambiarse por uno que de una buena tasa de aceptación cercana al 50%
  phi.p <- min(phi.p, 2-phi.p)
  # 2. tasa de aceptacion
  Cor1 <- phi.p^DY
  Cor2 <- Phi^DY
  lr <- -.5*( determinant(Cor1,log=TRUE)$mod - determinant(Cor2,log=TRUE)$mod + 
                Traza( (YTrain-XBetas)%*%t(YTrain-XBetas)%*%(solve(Cor1) - solve(Cor2)) )/Sigma2 )
  # 3. actualizar valor
  if( log(runif(1)) < lr ) { 
    Phi <-phi.p
    ac<-ac+1 
  }
  # progreso & almacenar,voy guardando cada 5 muestreos
  if(s%%odens==0) {
    #Solo calculo la LogVerosimilitud de los que se van a guardar
    
    LogVero <- mvtnorm::dmvnorm(x=YTrain, mean=XTrain%*%Betas, sigma=Sigma2*Phi^DY, log=T)
    Wardar<-s/odens
    MatrizParam3[Wardar,] <- c(Betas,Sigma2,Phi,LogVero)
  }
  
  #Anuncio avance de porcentaje de avance
  if (s %% 5000 == 0) {
    count<-count+10
    cat('\nAvance:', count, "%")
  }
}

ac

write.csv(MatrizParam3,"MuestrasModeloErrorCor")

#########################Modelo 4: Selección automática de covariables#########################

#Código

p<-dim(XTrain)[2]
S<-10000
count <- 0
MatrizParam4<-matrix(NA,S,12)
z<-rep(1,dim(XTrain)[2] )
lpy.c<-lpy.X(YTrain,XTrain[,z==1,drop=FALSE])
set.seed(1729)
for(s in 1:S){

  for(j in sample(1:p))
  {
    zp<-z ; zp[j]<-1-zp[j]
    lpy.p<-lpy.X(YTrain,XTrain[,zp==1,drop=FALSE])
    r<- (lpy.p - lpy.c)*(-1)^(zp[j]==0)
    z[j]<-rbinom(1,1,1/(1+exp(-r)))
    if(z[j]==zp[j]) {lpy.c<-lpy.p}
  }
    
  beta<-z
  if(sum(z)>0){
    resultado<-lm.gprior(YTrain,XTrain[,z==1,drop=FALSE],S=1)
    beta[z==1]<-resultado$beta }
  #Z[s,]<-z  No voy a almacenar los z
  MatrizParam4[s,1:10]<-beta
  MatrizParam4[s,11]<-resultado$s2
    
  #Calculo de LogVerosimilitud
  LogVero <- mvtnorm::dmvnorm(x=YTrain, mean=as.matrix(XTrain)%*%beta, sigma=resultado$s2*diag(1,342,342), log=T)
  MatrizParam4[s,12] <- LogVero
    
  #Anuncio avance de porcentaje de avance
  if (s %% 100 == 0) {
    count<-count+1
    cat('\nAvance:', count, "%")
  }
    #if(s%%10==0)
    #{ 
      #bpm<-apply(Betas[1:s,],2,mean) ; plot(bpm)
      #cat(s,mean(z), mean( (y.te-X.te%*%bpm)^2),"\n")
      #Zcp<- apply(Z[1:s,,drop=FALSE],2,cumsum)/(1:s)
      #plot(c(1,s),range(Zcp),type="n") ; apply(Zcp,2,lines)
    #}
  }
  
write.csv(MatrizParam4,"MuestrasModeloSelecAuto")

#########################Importación matrices resultantes#########################

RegresionRidge <- read.csv("MuestrasModeloRidge")
names(RegresionRidge) <- c("Indice",paste("Beta",1:10,sep=""),"Sigma","Lambda","LogVerosimilitud")


RegresionLasso <- read.csv("MuestrasModeloLasso")
names(RegresionLasso) <- c("Indice",paste("Beta",1:10,sep=""),"Sigma","LogVerosimilitud")

RegresionErrorCor <- read.csv("MuestrasModeloErrorCor")
names(RegresionErrorCor) <- c("Indice",paste("Beta",1:10,sep=""),"Sigma","Phi","LogVerosimilitud")

RegresionSelecAuto <- read.csv("MuestrasModeloSelecAuto")
names(RegresionSelecAuto) <- c("Indice",paste("Beta",1:10,sep=""),"Sigma","LogVerosimilitud")


#########################Inferencia y tratamiento de las muestras resultantes######################

#########################Graficación de LogVerosimilitudes
dev.new()
layout(matrix(c(1, 2, 3, 4), nrow = 2, ncol = 2))
#Regresión Ridge
plot(RegresionRidge$LogVerosimilitud,type = "p", pch = ".", cex = 2,add=0 , col = "blue" , xlim = c(0,10000) , ylim = c(-385,-360) , xlab = "Iteración", ylab = "Log-Verosimilitud", main = "Log-Verosimilitud Regresión Ridge")
grid()
#Regresión Lasso
plot(RegresionLasso$LogVerosimilitud,type = "p", pch = ".", cex = 2,add=0 , col = "red" , xlim = c(0,10000) , ylim = c(-385,-360) , xlab = "Iteración", ylab = "Log-Verosimilitud", main = "Log-Verosimilitud Regresión Lasso")
grid()
#Regresión Errores correlacionados
plot(RegresionErrorCor$LogVerosimilitud,type = "p", pch = ".", cex = 2,add=0 , col = "green" , xlim = c(0,10000) , ylim = c(-385,-360) , xlab = "Iteración", ylab = "Log-Verosimilitud", main = "Log-Verosimilitud Regresión con\nErrores Correlacionados")
grid()
#Regresión Selección automática de variables
plot(RegresionSelecAuto$LogVerosimilitud,type = "p", pch = ".", cex = 2,add=0 , col = "purple" , xlim = c(0,10000) , ylim = c(-385,-360) , xlab = "Iteración", ylab = "Log-Verosimilitud", main = "Log-Verosimilitud Regresión con\nSelección automática de covariables")
grid()


#En conjunto
plot(RegresionRidge$LogVerosimilitud,type = "p", pch = ".", cex = 2,add=0 , col = "blue" , xlim = c(0,10000) , ylim = c(-385,-360) , xlab = "Iteración", ylab = "Log-Verosimilitud", main = "Log-Verosimilitudes")
grid()
points(RegresionLasso$LogVerosimilitud,col="red",type="p",pch= ".", cex = 2)
points(RegresionErrorCor$LogVerosimilitud,col="green",type="p",pch= ".", cex = 2)
points(RegresionLasso$LogVerosimilitud,col="purple",type="p",pch= ".", cex = 2)


########################Tamaños efectivos de muestra
#Modelo Ridge
apply(RegresionRidge[,2:12],2,effectiveSize)
#Modelo Lasso
apply(RegresionLasso[,2:12],2,effectiveSize)
#Modelo Errores Correlacionados
apply(RegresionErrorCor[,2:12],2,effectiveSize)
#Modelo selección automática de covariables
apply(RegresionSelecAuto[,2:12],2,effectiveSize)

#######################Gráficos de autocorrelación


#Ridge
dev.new(width = 15, height = 6)
par(mfrow = c(2, 5))
layout(matrix(1:10, nrow = 2, ncol = 5))
for(i in 2:11){
  acf(RegresionRidge[,i],ci.col="gray",xlab="lag",main=paste("Modelo Ridge: ","Beta",i-1))
}
acf(RegresionRidge[,12],ci.col="gray",xlab="lag",main="Modelo Ridge: Sigma")

#Lasso
dev.new(width = 15, height = 6)
par(mfrow = c(2, 5))
layout(matrix(1:10, nrow = 2, ncol = 5))
for(i in 2:11){
  acf(RegresionLasso[,i],ci.col="gray",xlab="lag",main=paste("Modelo Lasso: ","Beta",i-1))
}
acf(RegresionLasso[,12],ci.col="gray",xlab="lag",main="Modelo Lasso: Sigma")

#Errores correlacionados
dev.new(width = 15, height = 6)
par(mfrow = c(2, 5))
layout(matrix(1:10, nrow = 2, ncol = 5))
for(i in 2:11){
  acf(RegresionErrorCor[,i],ci.col="gray",xlab="lag",main=paste("Modelo Errores\ncorrelacionados: ","Beta",i-1))
}
acf(RegresionErrorCor[,12],ci.col="gray",xlab="lag",main="Modelo Errores correlacionados: Sigma")

#Selección automática de covariables
dev.new(width = 15, height = 6)
par(mfrow = c(2, 5))
layout(matrix(1:10, nrow = 2, ncol = 5))
for(i in 2:11){
  acf(RegresionSelecAuto[,i],ci.col="gray",xlab="lag",main=paste("Modelo Selección automática\nde covariables: ","Beta",i-1))
}
acf(RegresionSelecAuto[,12],ci.col="gray",xlab="lag",main="Modelo Selección automática\nde covariables: Sigma")

#En este modelo los betas 5,6,7,8,9 tienen mal comportamiento

for(i in 2:11){
  #revisión de cuantas muestras son 0 en cada columna
  cat("\nBeta",i-1," tiene",sum(RegresionSelecAuto[,i]==0),"ceros")
}



#########################Estimaciones de Beta y Sigma en cada modelo
ModFrecu <- lm(YTrain ~ -1 + XTrain)
EstimFrecu <- ModFrecu$coefficients
EstimRidge <- colMeans(RegresionRidge[,2:12])
EstimLasso <- colMeans(RegresionLasso[,2:12])
EstimError <- colMeans(RegresionErrorCor[,2:12])
EstimSelec <- colMeans(RegresionSelecAuto[,2:12])

round(rbind(EstimFrecu,EstimRidge,EstimLasso,EstimError,EstimSelec),3)


#####################Intervalos de credibilidad e histogramas

#Ridge
apply(RegresionRidge[,2:11],2,function(x) round(quantile(x,c(0.025,0.975)),3))
#Intervalos de Beta 1, 5, 6, 7, 8, 10 contienen al 0

#Lasso
apply(RegresionLasso[,2:11],2,function(x) round(quantile(x,c(0.025,0.975)),3))
#Intervalos de Beta 1, 6, 7, 8, 10 contienen al 0

#Errores Correlacionados
apply(RegresionErrorCor[,2:11],2,function(x) round(quantile(x,c(0.025,0.975)),3))
#Intervalos de Beta 1, 6, 7, 8, 10 contienen al 0

#Selección automática de covariables
apply(RegresionSelecAuto[,2:11],2,function(x) round(quantile(x,c(0.025,0.975)),3))
#Intervalos de Beta 1, 2, 5, 6, 7, 8, 10 contienen al 0

#histogramas de betas y sigma

#Ridge
dev.new(width = 15, height = 6)
par(mfrow = c(2, 5))
layout(matrix(1:10, nrow = 2, ncol = 5))
for(i in 2:11){
  hist(RegresionRidge[,i],main=paste("Distribución de Beta",i-1," |--",sep=""),breaks=100,freq=F,xlab="",ylab = "")
}

hist(RegresionRidge[,12],main="Distribución de Sigma^2 | -",breaks=100,freq=F,xlab="",ylab = "")


#Lasso
dev.new(width = 15, height = 6)
par(mfrow = c(2, 5))
layout(matrix(1:10, nrow = 2, ncol = 5))
for(i in 2:11){
  hist(RegresionLasso[,i],main=paste("Distribución de Beta",i-1," |--",sep=""),breaks=100,freq=F,xlab="",ylab = "")
}

hist(RegresionLasso[,12],main="Distribución de Sigma^2 | -",breaks=100,freq=F,xlab="",ylab = "")


#Errores Correlacionados
dev.new(width = 15, height = 6)
par(mfrow = c(2, 5))
layout(matrix(1:10, nrow = 2, ncol = 5))
for(i in 2:11){
  hist(RegresionErrorCor[,i],main=paste("Distribución de Beta",i-1," |--",sep=""),breaks=100,freq=F,xlab="",ylab = "")
}

hist(RegresionErrorCor[,12],main="Distribución de Sigma^2 | -",breaks=100,freq=F,xlab="",ylab = "")

hist(RegresionErrorCor[,13],main="Distribución de Rho | -",breaks=100,freq=F,xlab="",ylab = "")



#Selección automática de variables
dev.new(width = 15, height = 6)
par(mfrow = c(2, 5))
layout(matrix(1:10, nrow = 2, ncol = 5))
for(i in 2:11){
  hist(RegresionSelecAuto[,i],main=paste("Distribución de Beta",i-1," |--",sep=""),breaks=100,freq=F,xlab="",ylab = "")
}

hist(RegresionSelecAuto[,12],main="Distribución de Sigma^2 | -",breaks=100,freq=F,xlab="",ylab = "")


##############Evaluación de los modelos con los datos de Testeo

#Modelo Ridge
YRidge <- XTest%*%EstimRidge[1:10]#Estimaciones
mean(abs(YTest-YRidge))#Error absoluto medio
#Modelo Lasso
YLasso <- XTest%*%EstimLasso[1:10]#Estimaciones
mean(abs(YTest-YLasso))#Error absoluto medio
#Modelo Errores correlacionados
YErrCor <- XTest%*%EstimError[1:10]#Estimaciones
mean(abs(YTest-YErrCor))#Error absoluto medio
#Modelo Selección automática de covariables
YSelec <- XTest%*%EstimSelec[1:10]#Estimaciones
mean(abs(YTest-YSelec))#Error absoluto medio

#Gráficos de las estimaciones versus valores reales

dev.new()

par(mar=c(4,3,2.5,3),mgp=c(1.7,.7,0))

layout(matrix(c(1, 2, 3, 4), nrow = 2, ncol = 2))
plot(YTest,YRidge,xlab=expression(italic(y)[test]),
     ylab=expression(hat(italic(y))[test]),main="Modelo Ridge",col = "blue", pch = 21, bg = "red",ylim=c(-1.5,1.5)) ; abline(0,1)
grid()
legend("bottomright",legend=paste("Error absoluto medio:",round(mean(abs(YTest-YRidge)),3)),bty = "n",cex = 0.8)
plot(YTest,YLasso,xlab=expression(italic(y)[test]),
     ylab=expression(hat(italic(y))[test]),main="Modelo Lasso",col = "blue", pch = 21, bg = "red",ylim=c(-1.5,1.5)) ; abline(0,1)
grid()
legend("bottomright",legend=paste("Error absoluto medio:",round(mean(abs(YTest-YLasso)),3)),bty = "n",cex = 0.8)
plot(YTest,YErrCor,xlab=expression(italic(y)[test]),
     ylab=expression(hat(italic(y))[test]),main="Modelo Errores\ncorrelacionados",col = "blue", pch = 21, bg = "red",ylim=c(-1.5,1.5)) ; abline(0,1)
grid()
legend("bottomright",legend=paste("Error absoluto medio:",round(mean(abs(YTest-YErrCor)),3)),bty = "n",cex = 0.8)
plot(YTest,YSelec,xlab=expression(italic(y)[test]),
     ylab=expression(hat(italic(y))[test]),main="Modelo Selección automática\nde covariables",col = "blue", pch = 21, bg = "red",ylim=c(-1.5,1.5)) ; abline(0,1)
grid()
legend("bottomright",legend=paste("Error absoluto medio:",round(mean(abs(YTest-YSelec)),3)),bty = "n",cex = 0.8)


#################Bondad de ajuste

###Utilizando los datos de Entrenamiento


DY <-abs(outer( (1:342),(1:342) ,"-"))
MedDesv<-matrix(NA,10000,8)

set.seed(1729)
count<-0
for(i in 1:10000){  
  #Ridge
  MR<-mvtnorm::rmvnorm(1, mean = XTrain%*%t(RegresionRidge[i,2:11]), sigma = diag(RegresionRidge[i,12],342,342))
  #Lasso
  ML<-mvtnorm::rmvnorm(1, mean = XTrain%*%t(RegresionLasso[i,2:11]), sigma = diag(RegresionLasso[i,12],342,342))
  #Errores correlacionados
  ME<-mvtnorm::rmvnorm(1, mean = XTrain%*%t(RegresionErrorCor[i,2:11]), sigma = RegresionErrorCor[i,12]*(RegresionErrorCor[i,13]^DY))
  #Selección automática de covariables
  MS<-mvtnorm::rmvnorm(1, mean = XTrain%*%t(RegresionSelecAuto[i,2:11]), sigma = diag(RegresionSelecAuto[i,12],342,342))
  MedDesv[i,]<-c(mean(MR),sd(MR),mean(ML),sd(ML),mean(ME),sd(ME),mean(MS),sd(MS))
  
  #Anuncio avance de porcentaje de avance
  if (i %% 100 == 0) {
    count<-count+1
    cat('\nAvance:', count, "%")
  }
}

write.csv(MedDesv,"BondadRealX")
BondadAjuste<-read.csv("BondadRealX")
names(BondadAjuste)<-c("Indice","MediaRidge","DesvRidge","MediaLasso","DesvLasso","MediaErrCor","DesvErrCor","MediaSelec","DesvSelec")


#Media real
MediaTrain<-mean(YTrain)
#Desviación real
DesvTrain<-sd(YTrain)

#########################PPP
#Ridge
round(mean(BondadAjuste$MediaRidge>MediaTrain),3);round(mean(BondadAjuste$DesvRidge>DesvTrain),3)
#Lasso
round(mean(BondadAjuste$MediaLasso>MediaTrain),3);round(mean(BondadAjuste$DesvLasso>DesvTrain),3)
#Errores Correlacionados
round(mean(BondadAjuste$MediaErrCor>MediaTrain),3);round(mean(BondadAjuste$DesvErrCor>DesvTrain),3)
#Selección automática
round(mean(BondadAjuste$MediaSelec>MediaTrain),3);round(mean(BondadAjuste$DesvSelec>DesvTrain),3)


#Ahora comparando la bondad resultante con la media real y desviacion de los datos de testeo

#Media real
MediaTest<-mean(YTest)
#Desviación real
DesvTest<-sd(YTest)

#Ridge
round(mean(BondadAjuste$MediaRidge>MediaTest),3);round(mean(BondadAjuste$DesvRidge>DesvTest),3)
#Lasso
round(mean(BondadAjuste$MediaLasso>MediaTest),3);round(mean(BondadAjuste$DesvLasso>DesvTest),3)
#Errores Correlacionados
round(mean(BondadAjuste$MediaErrCor>MediaTest),3);round(mean(BondadAjuste$DesvErrCor>DesvTest),3)
#Selección automática
round(mean(BondadAjuste$MediaSelec>MediaTest),3);round(mean(BondadAjuste$DesvSelec>DesvTest),3)






#Gráfico de las medias y desviación estándar
dev.new()
par(mar=c(4,3,2.5,3),mgp=c(1.7,.7,0))
layout(matrix(c(1, 2, 3, 4), nrow = 2, ncol = 2))
#Ridge
plot(BondadAjuste$MediaRidge,BondadAjuste$DesvRidge,col = "blue", pch = 21, cex = 1, bg = "red", alpha = 0.1, main="Modelo Ridge",ylab="Desviación estándar",xlab="Media",ylim=c(0.8,1.25),xlim=c(-0.15,0.15))
grid()
#Lineas entrenamiento
abline(h = sd(YTrain), col = "black", lwd = 2,lty = 2)  
abline(v = mean(YTrain), col = "black", lwd = 2, lty = 2)
points(x=mean(YTrain),y=sd(YTrain),col="black",pch=15)
#Líneas testeo
abline(h = DesvTest, col = "green", lwd = 2,lty = 2)  
abline(v = MediaTest, col = "green", lwd = 2, lty = 2)
points(x=MediaTest,y=DesvTest,col="green",pch=15)
#Leyenda
legend("bottomright",legend=c(paste("ppp Media Train:",round(mean(BondadAjuste$MediaRidge>mean(YTrain)),3)),paste("ppp Media Test:",round(mean(BondadAjuste$MediaRidge>MediaTest),3))),bty = "n",cex = 0.7,bg="white")
legend("topleft",legend=c(paste("ppp Desviación Train:",round(mean(BondadAjuste$DesvRidge>sd(YTrain)),3)),paste("ppp Desviación Test:",round(mean(BondadAjuste$DesvRidge>DesvTest),3))),bty = "n",cex = 0.7,bg="white")

#Leyenda de colores para las líneas

legend(x = "bottomleft",
       legend = c("Test","Entrenamiento"),
       bty = "n",
       fill = c("green", "black"),
       cex = 0.6,
       inset = c(0.02, 0.02))


#Lasso
plot(BondadAjuste$MediaLasso,BondadAjuste$DesvLasso,col = "blue", pch = 21, cex = 1, bg = "red", alpha = 0.1, main="Modelo Lasso",ylab="Desviación estándar",xlab="Media",ylim=c(0.8,1.25),xlim=c(-0.15,0.15))
grid()
abline(h = sd(YTrain), col = "black", lwd = 2,lty = 2)  # Línea horizontal desviación
abline(v = mean(YTrain), col = "black", lwd = 2, lty = 2)
points(x=mean(YTrain),y=sd(YTrain),col="black",pch=15)
#Líneas testeo
abline(h = DesvTest, col = "green", lwd = 2,lty = 2)  
abline(v = MediaTest, col = "green", lwd = 2, lty = 2)
points(x=MediaTest,y=DesvTest,col="green",pch=15)
#Leyenda
legend("bottomright",legend=c(paste("ppp Media Train:",round(mean(BondadAjuste$MediaLasso>mean(YTrain)),3)),paste("ppp Media Test:",round(mean(BondadAjuste$MediaLasso>MediaTest),3))),bty = "n",cex = 0.7)
legend("topleft",legend=c(paste("ppp Desviación Train:",round(mean(BondadAjuste$DesvLasso>sd(YTrain)),3)),paste("ppp Desviación Test:",round(mean(BondadAjuste$DesvLasso>DesvTest),3))),bty = "n",cex = 0.7)


legend(x = "bottomleft",
       legend = c("Test","Entrenamiento"),
       bty = "n",
       fill = c("green", "black"),
       cex = 0.6,
       inset = c(0.02, 0.02))
#Errores Correlacionados
plot(BondadAjuste$MediaErrCor,BondadAjuste$DesvErrCor,col = "blue", pch = 21, cex = 1, bg = "red", alpha = 0.1, main="Modelo Errores Correlacionados",ylab="Desviación estándar",xlab="Media",ylim=c(0.8,1.25),xlim=c(-0.15,0.15))
grid()
abline(h = sd(YTrain), col = "black", lwd = 2,lty = 2)  # Línea horizontal desviación
abline(v = mean(YTrain), col = "black", lwd = 2, lty = 2)
points(x=mean(YTrain),y=sd(YTrain),col="black",pch=15)
#Líneas testeo
abline(h = DesvTest, col = "green", lwd = 2,lty = 2)  
abline(v = MediaTest, col = "green", lwd = 2, lty = 2)
points(x=MediaTest,y=DesvTest,col="green",pch=15)
#Leyenda
legend("bottomright",legend=c(paste("ppp Media Train:",round(mean(BondadAjuste$MediaErrCor>mean(YTrain)),3)),paste("ppp Media Test:",round(mean(BondadAjuste$MediaErrCor>MediaTest),3))),bty = "n",cex = 0.7)
legend("topleft",legend=c(paste("ppp Desviación Train:",round(mean(BondadAjuste$DesvErrCor>sd(YTrain)),3)),paste("ppp Desviación Test:",round(mean(BondadAjuste$DesvErrCor>DesvTest),3))),bty = "n",cex = 0.7)


legend(x = "bottomleft",
       legend = c("Test","Entrenamiento"),
       bty = "n",
       fill = c("green", "black"),
       cex = 0.6,
       inset = c(0.02, 0.02))
#Selección automática
plot(BondadAjuste$MediaSelec,BondadAjuste$DesvSelec,col = "blue", pch = 21, cex = 1, bg = "red", alpha = 0.1, main="Modelo Selección automática\nde covariables",ylab="Desviación estándar",xlab="Media",ylim=c(0.8,1.25),xlim=c(-0.15,0.15))
grid()
abline(h = sd(YTrain), col = "black", lwd = 2,lty = 2)  # Línea horizontal desviación
abline(v = mean(YTrain), col = "black", lwd = 2, lty = 2)
points(x=mean(YTrain),y=sd(YTrain),col="black",pch=15)
#Líneas testeo
abline(h = DesvTest, col = "green", lwd = 2,lty = 2)  
abline(v = MediaTest, col = "green", lwd = 2, lty = 2)
points(x=MediaTest,y=DesvTest,col="green",pch=15)
#Leyenda
legend("bottomright",legend=c(paste("ppp Media Train:",round(mean(BondadAjuste$MediaSelec>mean(YTrain)),3)),paste("ppp Media Test:",round(mean(BondadAjuste$MediaSelec>MediaTest),3))),bty = "n",cex = 0.7)
legend("topleft",legend=c(paste("ppp Desviación Train:",round(mean(BondadAjuste$DesvSelec>sd(YTrain)),3)),paste("ppp Desviación Test:",round(mean(BondadAjuste$DesvSelec>DesvTest),3))),bty = "n",cex = 0.7)
legend(x = "bottomleft",
       legend = c("Test","Entrenamiento"),
       bty = "n",
       fill = c("green", "black"),
       cex = 0.6,
       inset = c(0.02, 0.02))



############################DIC

#Ridge

LP1 <- as.numeric(RegresionRidge$LogVerosimilitud)
theta_hat  <- colMeans(RegresionRidge[,2:11])
sigma2_hat <- mean(RegresionRidge[,12])
lpyth_m1   <- mvtnorm::dmvnorm(x = YTrain, mean = XTrain%*%theta_hat , sigma = diag(sigma2_hat,342,342), log = T)
pDIC_m1    <- 2*(lpyth_m1 - mean(LP1))
dic_m1     <- -2*lpyth_m1 + 2*pDIC_m1

#Lasso

LP2 <- as.numeric(RegresionLasso$LogVerosimilitud)
theta_hat  <- colMeans(RegresionLasso[,2:11])
sigma2_hat <- mean(RegresionLasso[,12])
lpyth_m2   <- mvtnorm::dmvnorm(x = YTrain, mean = XTrain%*%theta_hat , sigma = diag(sigma2_hat,342,342), log = T)
pDIC_m2    <- 2*(lpyth_m2 - mean(LP2))
dic_m2     <- -2*lpyth_m2 + 2*pDIC_m2


#Errores correlacionados

LP3 <- as.numeric(RegresionErrorCor$LogVerosimilitud)
theta_hat  <- colMeans(RegresionErrorCor[,2:11])
sigma2_hat <- mean(RegresionErrorCor[,12])
rho_hat    <- mean(RegresionErrorCor[,13])
lpyth_m3   <- mvtnorm::dmvnorm(x = YTrain, mean = XTrain%*%theta_hat , sigma = sigma2_hat*(rho_hat^DY), log = T)
pDIC_m3    <- 2*(lpyth_m3 - mean(LP3))
dic_m3     <- -2*lpyth_m3 + 2*pDIC_m3


#Selección automática de covariables

LP4 <- as.numeric(RegresionSelecAuto$LogVerosimilitud)
theta_hat  <- colMeans(RegresionSelecAuto[,2:11])
sigma2_hat <- mean(RegresionSelecAuto[,12])
lpyth_m4   <- mvtnorm::dmvnorm(x = YTrain, mean = XTrain%*%theta_hat , sigma = diag(sigma2_hat,342,342), log = T)
pDIC_m4    <- 2*(lpyth_m4 - mean(LP4))
dic_m4     <- -2*lpyth_m4 + 2*pDIC_m4



