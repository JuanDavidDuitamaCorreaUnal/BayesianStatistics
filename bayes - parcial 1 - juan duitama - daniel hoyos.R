##########################Script Caso de estudio 1 Estadística Bayesiana#################################

######################################LIBRERÍAS, DIRECTORIOS Y SEMILLA ###################################

#######Librerías utilizadas
library(readxl)
library(readr)
library(stringr)
library(sqldf)
library(dplyr)
library(ggplot2)

######################Directorios de trabajo

#Juan David
setwd("C:/Users/juand/Desktop/Estadística/Semestres/Semestre 7/EstadisticaBayesiana/CasosDeEstudio/Directorio")

#Daniel Hoyos

setwd("C:/Users/Daniel/Desktop/Semestre 2023-1S/Bayesiana/Caso estudio 1")



#Finalmente se selecciona una semilla para el desarrollo de los apartados
set.seed(1729)

##########################################IMPORTACIÓN,LIMPIEZA###################################################################

#No ejecutar esta parte, ya se hizo y se guardó en csv que luego se importarán

####################Importación de datos

Fiscalia <- read_csv("Conteo_de_Victimas.csv",col_names=TRUE,
                     col_types=cols_only("HECHO"="c",
                                         "ESTADO_NOTICIA"="c",
                                         "ANIO_DENUNCIA"="i",
                                         "ANIO_ENTRADA"="i",
                                         "ANIO_HECHO"="i",
                                         "DEPARTAMENTO"="c",
                                         "GRUPO_DELITO"="c",
                                         "PAIS_NACIMIENTO"="c",
                                         "PAIS"="c",
                                         "GRUPO_EDAD_VICTIMA"="c",
                                         "SEXO_VICTIMA"="c",
                                         "TOTAL_VICTIMAS"="i"))

#No fue necesaria una limpieza de datos en esta base, ya fueron realizadas las respectivas revisiones en cada variable

####################REALIZACION DE LAS TABLAS DE CADA PUNTO (no correr, más adelante importar csv guardado)##################


#########PUNTO 1
Filtro2022<-sqldf("select SEXO_VICTIMA, TOTAL_VICTIMAS
                   from Fiscalia
                   where HECHO='SI' and ESTADO_NOTICIA='ACTIVO' and ANIO_DENUNCIA='2022' and ANIO_ENTRADA='2022' and ANIO_HECHO='2022' and DEPARTAMENTO='BOGOTÁ, D. C.' and GRUPO_DELITO='DELITOS SEXUALES' and PAIS_NACIMIENTO='Colombia' and GRUPO_EDAD_VICTIMA in ('ADOLESCENTE 14 - 17','PRE-ADOLESCENTE 12 - 13','INFANCIA 6 - 11','PRIMERA INFANCIA 0 - 5')")

quantile(Filtro2022$TOTAL_VICTIMAS,0.75)+3*IQR(Filtro2022$TOTAL_VICTIMAS)

Filtro2022<-sqldf("select *
                   from Filtro2022
                   where SEXO_VICTIMA is not NULL and TOTAL_VICTIMAS<=9")

write_csv(Filtro2022,"Anio2022")

####PUNTO 3


#Mas tarde las hago, pienso hacerlo con un for, pero creo que quemaría mi cerebro xddd

#Año 2012

Filtro2012<-sqldf("select SEXO_VICTIMA, TOTAL_VICTIMAS
                   from Fiscalia
                   where HECHO='SI' and ESTADO_NOTICIA='ACTIVO' and ANIO_HECHO=2012 and ANIO_DENUNCIA=2012 and ANIO_ENTRADA=2012 and DEPARTAMENTO='BOGOTÁ, D. C.' and GRUPO_DELITO='DELITOS SEXUALES' and PAIS_NACIMIENTO='Colombia' and GRUPO_EDAD_VICTIMA in ('ADOLESCENTE 14 - 17','PRE-ADOLESCENTE 12 - 13','INFANCIA 6 - 11','PRIMERA INFANCIA 0 - 5')")


quantile(Filtro2012$TOTAL_VICTIMAS,c(0.25,0.75))+c(-3,3)*IQR(Filtro2012$TOTAL_VICTIMAS)

Filtro2012<-sqldf("select *
                   from Filtro2012
                   where SEXO_VICTIMA is not NULL and TOTAL_VICTIMAS<=5")

write_csv(Filtro2012,"Anio2012")


#Año 2013

Filtro2013<-sqldf("select SEXO_VICTIMA, TOTAL_VICTIMAS
                   from Fiscalia
                   where HECHO='SI' and ESTADO_NOTICIA='ACTIVO' and ANIO_HECHO=2013 and ANIO_DENUNCIA=2013 and ANIO_ENTRADA=2013 and DEPARTAMENTO='BOGOTÁ, D. C.' and GRUPO_DELITO='DELITOS SEXUALES' and PAIS_NACIMIENTO='Colombia' and GRUPO_EDAD_VICTIMA in ('ADOLESCENTE 14 - 17','PRE-ADOLESCENTE 12 - 13','INFANCIA 6 - 11','PRIMERA INFANCIA 0 - 5')")


quantile(Filtro2013$TOTAL_VICTIMAS,c(0.25,0.75))+c(-3,3)*IQR(Filtro2013$TOTAL_VICTIMAS)

Filtro2013<-sqldf("select *
                   from Filtro2013
                   where SEXO_VICTIMA is not NULL and TOTAL_VICTIMAS<=2")

write_csv(Filtro2013,"Anio2013")



#Año 2014

Filtro2014<-sqldf("select SEXO_VICTIMA, TOTAL_VICTIMAS
                   from Fiscalia
                   where HECHO='SI' and ESTADO_NOTICIA='ACTIVO' and ANIO_HECHO=2014 and ANIO_DENUNCIA=2014 and ANIO_ENTRADA=2014 and DEPARTAMENTO='BOGOTÁ, D. C.' and GRUPO_DELITO='DELITOS SEXUALES' and PAIS_NACIMIENTO='Colombia' and GRUPO_EDAD_VICTIMA in ('ADOLESCENTE 14 - 17','PRE-ADOLESCENTE 12 - 13','INFANCIA 6 - 11','PRIMERA INFANCIA 0 - 5')")


quantile(Filtro2014$TOTAL_VICTIMAS,c(0.25,0.75))+c(-3,3)*IQR(Filtro2014$TOTAL_VICTIMAS)

Filtro2014<-sqldf("select *
                   from Filtro2014
                   where SEXO_VICTIMA is not NULL and TOTAL_VICTIMAS<=5")

write_csv(Filtro2014,"Anio2014")


#Año 2015

Filtro2015<-sqldf("select SEXO_VICTIMA, TOTAL_VICTIMAS
                   from Fiscalia
                   where HECHO='SI' and ESTADO_NOTICIA='ACTIVO' and ANIO_HECHO=2015 and ANIO_DENUNCIA=2015 and ANIO_ENTRADA=2015 and DEPARTAMENTO='BOGOTÁ, D. C.' and GRUPO_DELITO='DELITOS SEXUALES' and PAIS_NACIMIENTO='Colombia' and GRUPO_EDAD_VICTIMA in ('ADOLESCENTE 14 - 17','PRE-ADOLESCENTE 12 - 13','INFANCIA 6 - 11','PRIMERA INFANCIA 0 - 5')")


quantile(Filtro2015$TOTAL_VICTIMAS,c(0.25,0.75))+c(-3,3)*IQR(Filtro2015$TOTAL_VICTIMAS)

Filtro2015<-sqldf("select *
                   from Filtro2015
                   where SEXO_VICTIMA is not NULL and TOTAL_VICTIMAS<=9")

write_csv(Filtro2015,"Anio2015")


#Año 2016

Filtro2016<-sqldf("select SEXO_VICTIMA, TOTAL_VICTIMAS
                   from Fiscalia
                   where HECHO='SI' and ESTADO_NOTICIA='ACTIVO' and ANIO_HECHO=2016 and ANIO_DENUNCIA=2016 and ANIO_ENTRADA=2016 and DEPARTAMENTO='BOGOTÁ, D. C.' and GRUPO_DELITO='DELITOS SEXUALES' and PAIS_NACIMIENTO='Colombia' and GRUPO_EDAD_VICTIMA in ('ADOLESCENTE 14 - 17','PRE-ADOLESCENTE 12 - 13','INFANCIA 6 - 11','PRIMERA INFANCIA 0 - 5')")


quantile(Filtro2016$TOTAL_VICTIMAS,c(0.25,0.75))+c(-3,3)*IQR(Filtro2016$TOTAL_VICTIMAS)

Filtro2016<-sqldf("select *
                   from Filtro2016
                   where SEXO_VICTIMA is not NULL and TOTAL_VICTIMAS<=5")

write_csv(Filtro2016,"Anio2016")



#Año 2017

Filtro2017<-sqldf("select SEXO_VICTIMA, TOTAL_VICTIMAS
                   from Fiscalia
                   where HECHO='SI' and ESTADO_NOTICIA='ACTIVO' and ANIO_HECHO=2017 and ANIO_DENUNCIA=2017 and ANIO_ENTRADA=2017 and DEPARTAMENTO='BOGOTÁ, D. C.' and GRUPO_DELITO='DELITOS SEXUALES' and PAIS_NACIMIENTO='Colombia' and GRUPO_EDAD_VICTIMA in ('ADOLESCENTE 14 - 17','PRE-ADOLESCENTE 12 - 13','INFANCIA 6 - 11','PRIMERA INFANCIA 0 - 5')")


quantile(Filtro2017$TOTAL_VICTIMAS,c(0.25,0.75))+c(-3,3)*IQR(Filtro2017$TOTAL_VICTIMAS)

Filtro2017<-sqldf("select *
                   from Filtro2017
                   where SEXO_VICTIMA is not NULL and TOTAL_VICTIMAS<=5")

write_csv(Filtro2017,"Anio2017")



#Año 2018

Filtro2018<-sqldf("select SEXO_VICTIMA, TOTAL_VICTIMAS
                   from Fiscalia
                   where HECHO='SI' and ESTADO_NOTICIA='ACTIVO' and ANIO_HECHO=2018 and ANIO_DENUNCIA=2018 and ANIO_ENTRADA=2018 and DEPARTAMENTO='BOGOTÁ, D. C.' and GRUPO_DELITO='DELITOS SEXUALES' and PAIS_NACIMIENTO='Colombia' and GRUPO_EDAD_VICTIMA in ('ADOLESCENTE 14 - 17','PRE-ADOLESCENTE 12 - 13','INFANCIA 6 - 11','PRIMERA INFANCIA 0 - 5')")


quantile(Filtro2018$TOTAL_VICTIMAS,c(0.25,0.75))+c(-3,3)*IQR(Filtro2018$TOTAL_VICTIMAS)

Filtro2018<-sqldf("select *
                   from Filtro2018
                   where SEXO_VICTIMA is not NULL and TOTAL_VICTIMAS<=9")

write_csv(Filtro2018,"Anio2018")


#Año 2019

Filtro2019<-sqldf("select SEXO_VICTIMA, TOTAL_VICTIMAS
                   from Fiscalia
                   where HECHO='SI' and ESTADO_NOTICIA='ACTIVO' and ANIO_HECHO=2019 and ANIO_DENUNCIA=2019 and ANIO_ENTRADA=2019 and DEPARTAMENTO='BOGOTÁ, D. C.' and GRUPO_DELITO='DELITOS SEXUALES' and PAIS_NACIMIENTO='Colombia' and GRUPO_EDAD_VICTIMA in ('ADOLESCENTE 14 - 17','PRE-ADOLESCENTE 12 - 13','INFANCIA 6 - 11','PRIMERA INFANCIA 0 - 5')")


quantile(Filtro2019$TOTAL_VICTIMAS,c(0.25,0.75))+c(-3,3)*IQR(Filtro2019$TOTAL_VICTIMAS)

Filtro2019<-sqldf("select *
                   from Filtro2019
                   where SEXO_VICTIMA is not NULL and TOTAL_VICTIMAS<=9")

write_csv(Filtro2019,"Anio2019")




#Año 2020

Filtro2020<-sqldf("select SEXO_VICTIMA, TOTAL_VICTIMAS
                   from Fiscalia
                   where HECHO='SI' and ESTADO_NOTICIA='ACTIVO' and ANIO_HECHO=2020 and ANIO_DENUNCIA=2020 and ANIO_ENTRADA=2020 and DEPARTAMENTO='BOGOTÁ, D. C.' and GRUPO_DELITO='DELITOS SEXUALES' and PAIS_NACIMIENTO='Colombia' and GRUPO_EDAD_VICTIMA in ('ADOLESCENTE 14 - 17','PRE-ADOLESCENTE 12 - 13','INFANCIA 6 - 11','PRIMERA INFANCIA 0 - 5')")


quantile(Filtro2020$TOTAL_VICTIMAS,c(0.25,0.75))+c(-3,3)*IQR(Filtro2020$TOTAL_VICTIMAS)

Filtro2020<-sqldf("select *
                   from Filtro2020
                   where SEXO_VICTIMA is not NULL and TOTAL_VICTIMAS<=9")

write_csv(Filtro2020,"Anio2020")


#Año 2021

Filtro2021<-sqldf("select SEXO_VICTIMA, TOTAL_VICTIMAS
                   from Fiscalia
                   where HECHO='SI' and ESTADO_NOTICIA='ACTIVO' and ANIO_HECHO=2021 and ANIO_DENUNCIA=2021 and ANIO_ENTRADA=2021 and DEPARTAMENTO='BOGOTÁ, D. C.' and GRUPO_DELITO='DELITOS SEXUALES' and PAIS_NACIMIENTO='Colombia' and GRUPO_EDAD_VICTIMA in ('ADOLESCENTE 14 - 17','PRE-ADOLESCENTE 12 - 13','INFANCIA 6 - 11','PRIMERA INFANCIA 0 - 5')")


quantile(Filtro2021$TOTAL_VICTIMAS,c(0.25,0.75))+c(-3,3)*IQR(Filtro2021$TOTAL_VICTIMAS)

Filtro2021<-sqldf("select *
                   from Filtro2021
                   where SEXO_VICTIMA is not NULL and TOTAL_VICTIMAS<=5")

write_csv(Filtro2021,"Anio2021")

#####################################DESARROLLO PUNTO 1#################################


DatosPunto1<-read.csv("Anio2022")



####################################################APARTADO A

#Hay que hacer tres gráficos, una distribución previa y dos posteriores, la masculina y femenina

alpha1<-0.01

#Tabla con estadísticos de prueba y cantidad de individuos

EstadPrueb1<-sqldf("select SEXO_VICTIMA as SexoVictima, sum(TOTAL_VICTIMAS) as Suma, count(*) as N
                    from DatosPunto1
                    group by SEXO_VICTIMA")

#Creación de gráficos
#l <- expression(paste("Distribución previa: ",theta,~"~ Gamma(0.01,0.01)"), paste("Distribución posterior de las víctimas Femeninas: ",theta[1],~"|y ~ Gamma(539.01,237.01)"),paste("Distribución posterior de las víctimas Masculinas: ",theta[2],~"|y ~ Gamma(208.01,115.01)"))
l<-c("Distribución previa","Distribución posterior \nde las víctimas Femeninas","Distribución posterior \nde las víctimas Masculinas")

set.seed(1729)

curve(dgamma(x,shape=alpha1+EstadPrueb1$Suma[1],rate=alpha1+EstadPrueb1$N[1]),from=0,to=3,add=0,ylab="Densidad",xlab=bquote(~theta),bty="o",lty="solid",col="red",lwd=2)
#text(locator(1),label=bquote(~"p("~theta[1]~"|y)"),col="red",cex=1.2)
curve(dgamma(x,shape=alpha1+EstadPrueb1$Suma[2],rate=alpha1+EstadPrueb1$N[2]),from=0,to=3,add=TRUE,ylab="Densidad",xlab=bquote(~theta),bty="o",lty="solid",col="green",lwd=2)
#text(locator(1),label=bquote(~"p("~theta[2]~"|y)"),col="green",cex=1.2)
curve(dgamma(x,shape=alpha1,rate=alpha1),from=0,to=3,add=TRUE,ylab="Densidad",xlab=bquote(~theta),bty="o",lty="solid",col="blue",lwd=2)
#text(locator(1),label=bquote(~"p("~theta~")"),col="blue",cex=1.2)
grid()
legend("left", legend = l, lty = c(1, 1, 1), bty = "n", col = c("blue", "red", "green"), inset = .05, y.intersp = .75,cex=0.9)#,box.col = "white",bg = rgb(1, 0, 0, alpha = 0.15)
title("Comparación entre la distribución previa y las distribuciones posteriores")




####################################################APARTADO B

set.seed(1729)

SimulMas1<-rgamma(100000,shape=208.01,rate=115.01) 
SimulFem1<-rgamma(100000,shape=539.01,rate=237.01)  
Niu1<-(SimulFem1-SimulMas1)/SimulMas1
1-sum(Niu>0)/100000

#Media, Coeficiente de variación, Intervalo de credibilidad

mean(Niu1) #0.2640523
sd(Niu1)/mean(Niu1)  #0.3922948
round(quantile(x = Niu1, probs = c(0.025, 0.975)), 3) #0.075 0.480


par(mar = c(3,3,1.4,1.4), mgp = c(1.75,.75,0))
hist(x = Niu1, freq = F, col = "gray90", border = "gray90", xlim = c(-0.25,1), ylim = c(0,4), xlab = expression(eta), ylab = "Densidad", main = bquote(~"Densidad para la variable aleatoria "~eta~"="~"("~theta[2]-theta[1]~")/"~theta[1]~" utilizando métodos de Monte Carlo"))
lines(density(Niu1), col = 4, lwd = 2)
abline(v = quantile(x = Niu1, probs = c(0.025, 0.975)), lty = 2, lwd = 2, col = "green")
abline(v = mean(Niu1), lty = 2, lwd = 2, col = "red")
grid()
legend("right", legend = c("Densidad", "IC 95%", "Media"), 
       col = c(4, "green", "red"), lty = 1, lwd = 2, bty = "n")





##################################################APARTADO C

#Análisis de sensitividad


set.seed(1729)

Alphas<-c(0.01,0.1,1,1,1,1)
Betas<-c(0.01,0.1,1,0.5,1/3,0.25)
MediasPriori<-Alphas/Betas
CoefVarPriori<-1/sqrt(Alphas)
Colores<-c("turquoise2","chartreuse1","#00FF7F","#1874CD","yellow","firebrick2")

#En cada caso calcular la media y el coeficiente de variación a priori, y repetir el númeral anterior. 
#Presentar los resultados visual y tabularmente. Interpretar los resultados obtenidos


for(i in 1:6){
  #Posterior de mujeres
  curve(dgamma(x,shape=Alphas[i]+EstadPrueb1$Suma[1],rate=Betas[i]+EstadPrueb1$N[1]),from=0,to=3,add=TRUE,ylab="Densidad",xlab=bquote(~theta),bty="o",lty= 5,col=Colores[i],lwd=2)
  #Posterior de hombres
  curve(dgamma(x,shape=Alphas[i]+EstadPrueb1$Suma[2],rate=Betas[i]+EstadPrueb1$N[2]),from=0,to=3,add=TRUE,ylab="Densidad",xlab=bquote(~theta),bty="o",lty= 3,col=Colores[i],lwd=2)
  #Previa, ambas poblaciones tienen las mismas previas
  curve(dgamma(x,shape=Alphas[i],rate=Alphas[i]),from=0,to=3,add=TRUE,ylab="Densidad",xlab=bquote(~theta),bty="o",lty="solid",col=Colores[i],lwd=2)
}
grid()
legend("topleft",legend = c("Distribución previa","Distribución posterior de \nvíctimas femeninas","Distribución posterior de \nvíctimas masculinas"),col="black",lty=c(1,2,3),lwd = 2, bty = "n")
title("Análisis de sensitividad de las distribuciones posteriores")



#Repetición de graficacion de las variables eta


set.seed(1729)


SimulMas<-rgamma(100000,shape=208+Alphas[1],rate=115+Betas[1]) 
SimulFem<-rgamma(100000,shape=539+Alphas[2],rate=237+Betas[2])  
Niu<-(SimulFem-SimulMas)/SimulMas
Resumen<-c(mean(Niu),sd(Niu)/mean(Niu),round(quantile(x = Niu, probs = c(0.025, 0.975)), 3))

par(mar = c(3,3,1.4,1.4), mgp = c(1.75,.75,0))
plot(density(Niu), lwd = 2, lty=1 , add=TRUE , col = Colores[1] , xlim = c(0,0.6) , ylim = c(0,4) , xlab = expression(eta), ylab = "Densidad", main = bquote(~"Análisis de sensitividad sobre la variable aleatoria "~eta~"="~"("~theta[2]-theta[1]~")/"~theta[1]~" utilizando métodos de Monte Carlo"))
abline(v = quantile(x = Niu, probs = c(0.025, 0.975)), lty = 5, lwd = 2, col = Colores[1])
abline(v = mean(Niu), lty = 3, lwd = 2, col = Colores[1])

for(i in 2:6){
#Media, Coeficiente de variación, Intervalo de credibilidad
Resumen<-rbind(Resumen,c(mean(Niu),sd(Niu)/mean(Niu),round(quantile(x = Niu, probs = c(0.025, 0.975)), 3)))

SimulMas<-rgamma(100000,shape=208+Alphas[i],rate=115+Betas[i]) 
SimulFem<-rgamma(100000,shape=539+Alphas[i],rate=237+Betas[i])  

Niu<-(SimulFem-SimulMas)/SimulMas
#hist(x = Niu, freq = F, col = "white", border = "gray90", xlim = c(-0.25,1), ylim = c(0,4), xlab = expression(eta), ylab = "Densidad", main = bquote(~"Densidad para la variable aleatoria "~eta~"="~"("~theta[2]-theta[1]~")/"~theta[1]~" utilizando métodos de Monte Carlo"))
lines(density(Niu), col = Colores[i], lwd = 2)
abline(v = quantile(x = Niu, probs = c(0.025, 0.975)), lty = 5, lwd = 2, col = Colores[i])
abline(v = mean(Niu), lty = 3, lwd = 2, col = Colores[i])
}

grid()
legend(locator(1),legend = c("Densidad", "IC 95%", "Media"), 
       col = "black", lty = c(1,5,3), lwd = 2, bty = "n")

Resumen

################################APARTADO D

#Bondad de ajuste utilizando la media y la desviación estándar

vectorMasculinos2022<-sqldf("select TOTAL_VICTIMAS
                    from       DatosPunto1
                    where      SEXO_VICTIMA = 'MASCULINO'   ")

vectorFemeninos2022<-sqldf("select  TOTAL_VICTIMAS
                    from       DatosPunto1
                    where      SEXO_VICTIMA = 'FEMENINO'   ")

set.seed(1729)

ThetaPostMas<-rgamma(10000,shape=0.01+EstadPrueb1$Suma[2],rate=0.01+EstadPrueb1$N[2])
ThetaPostFem<-rgamma(10000,shape=0.01+EstadPrueb1$Suma[1],rate=0.01+EstadPrueb1$N[1])

PredMedsMas<-NULL
PredMedsFem<-NULL
PredSDMas<-NULL
PredSDFem<-NULL

for(i in 1:10000){
  PrimerMas<-rpois(115,ThetaPostMas[i])
  PrimerFem<-rpois(237,ThetaPostFem[i])
  PredMedsMas[i]<-mean(PrimerMas)
  PredMedsFem[i]<-mean(PrimerFem)
  PredSDMas[i]<-sd(PrimerMas)
  PredSDFem[i]<-sd(PrimerFem)
}

summary(PredMedsFem)
mean(PredMedsFem)
sd(VectoresFemeninos[,1])
mean(PredSDFem)

mean(PredMedsMas)
sd(VectoresMasculinos[,1])
mean(PredSDMas)

#par(mfrow = c(2,2), mar = c(3,3,1.4,1.4), mgp = c(1.75,0.75,0))
#Bondad de ajuste de las mujeres

hist(x = PredMedsFem , freq = F, col = "gray90", border = "gray90", xlab = "Media", ylab = "Densidad", main = "Bondad de ajuste del modelo con media como estadístico de prueba para la población de víctimas Femeninas de Bogotá en 2022")
lines(density(PredMedsFem), col = "#00FF00", lwd = 2)
abline(v = EstadPrueb1$Suma[1]/EstadPrueb1$N[1], col = 1, lwd = 2, lty = 1)
abline(v = quantile(x = PredMedsFem, probs = c(0.025, 0.975)), lty = 1, lwd = 2, col = "red")
grid()
legend("left", legend = c("Posterior", "Media observada", "IC 95%"), col = c("#00FF00", 1, "red"), lty = 1, lwd = 2, bty = "n")


hist(x = PredSDFem , freq = F, col = "gray90", border = "gray90", xlim= c(1,2.2),xlab = "Desviación estándar", ylab = "Densidad", main = "Bondad de ajuste del modelo con desviación estándar como estadístico de prueba para la población de víctimas Femeninas de Bogotá en 2022")
lines(density(PredSDFem), col = "#00FF00", lwd = 2)
abline(v = sd(vectorFemeninos2022[,1]), col = 1, lwd = 2, lty = 1)
abline(v = quantile(x = PredSDFem, probs = c(0.025, 0.975)), lty = 1, lwd = 2, col = "red")
grid()
legend("left", legend = c("Posterior", "Desviación estándar\nobservada", "IC 95%"), col = c("#00FF00", 1, "red"), lty = 1, lwd = 2, bty = "n")


#Bondad de ajuste de los hombres

hist(x = PredMedsMas , freq = F, col = "gray90", border = "gray90", xlim= c(1,3) ,xlab = "Media", ylab = "Densidad", main = "Bondad de ajuste del modelo con media como estadístico de prueba para la población de víctimas Masculinas de Bogotá en 2022")
lines(density(PredMedsMas), col = "#00FF00", lwd = 2)
abline(v = EstadPrueb1$Suma[2]/EstadPrueb1$N[2], col = 1, lwd = 2, lty = 1)
abline(v = quantile(x = PredMedsMas, probs = c(0.025, 0.975)), lty = 1, lwd = 2, col = "red")
grid()
legend("left", legend = c("Posterior", "Media\nobservada", "IC 95%"), col = c("#00FF00", 1, "red"), lty = 1, lwd = 2, bty = "n")


hist(x = PredSDMas , freq = F, col = "gray90", border = "gray90", xlim= c(0.8,2),xlab = "Desviación estándar", ylab = "Densidad", main = "Bondad de ajuste del modelo con desviación estándar como estadístico de prueba para la población de víctimas Masculinas de Bogotá en 2022")
lines(density(PredSDMas), col = "#00FF00", lwd = 2)
abline(v = sd(vectorMasculinos2022[,1]), col = 1, lwd = 2, lty = 1)
abline(v = quantile(x = PredSDMas, probs = c(0.025, 0.975)), lty = 1, lwd = 2, col = "red")
grid()
legend("left", legend = c("Posterior", "Desviación estándar\nobservada", "IC 95%"), col = c("#00FF00", 1, "red"), lty = 1, lwd = 2, bty = "n")




#P-valores predictivos posteriores 

#Media mujeres
sum(PredMedsFem>(539/237))/10000
#Desviación estándar mujeres
sum(PredSDFem>sd(vectorFemeninos2022[,1]))/10000
#Media hombres
sum(PredMedsMas>(208/115))/10000
#Desviación Hombres
sum(PredSDMas>sd(vectorMasculinos2022[,1]))/10000

#####################################DESARROLLO PUNTO 2 Apartado 1#######################################

###Punto 1

set.seed(1729)

etahats<-NULL

for(i in 1:2000){
  thetahat1<-mean(sample(vectorMasculinos2022[,1],size=115,replace=TRUE))
  thetahat2<-mean(sample(vectorFemeninos2022[,1],237,replace=TRUE))
  etahat<-(thetahat2-thetahat1)/thetahat1
  etahats[i]<-etahat
  
}




summary(etahats)
cv<-sd(etahats)/mean(etahats);cv
round(quantile(etahats, probs = c(0.025,0.975)),3)

hist(etahats)

par(mar = c(3,3,1.4,1.4), mgp = c(1.75,.75,0))
hist(x = etahats, freq = F, col = "gray90", border = "gray90", xlim = c(-0.25,1), ylim = c(0,4), xlab = expression(eta), ylab = "Densidad", main = bquote(~"Densidad para la variable aleatoria "~eta~"="~"("~theta[2]-theta[1]~")/"~theta[1]~" utilizando Bootstrap paramétrico"))
lines(density(etahats), col = 4, lwd = 2)
abline(v = quantile(x = etahats, probs = c(0.025, 0.975)), lty = 2, lwd = 2, col = "green")
abline(v = mean(etahats), lty = 2, lwd = 2, col = "red")
grid()
legend("right", legend = c("Densidad", "IC 95%", "Media"), 
       col = c(4, "green", "red"), lty = 1, lwd = 2, bty = "n")

##############################################PUNTO3#####################################################


#Hay que replicar la modelación de eta tanto bayesiano como frecuentista

set.seed(1729)

InfoPedidaBayes<-NULL
InfoPedidaBoots<-NULL

for(year in 2012:2022){
  #Importación de los datos
  Datos<-read.csv(paste("Anio",as.character(year),sep = ""))
  EstadSuf<-sqldf("select SEXO_VICTIMA as SexoVictima, sum(TOTAL_VICTIMAS) as Suma, count(*) as N
                    from Datos
                    group by SEXO_VICTIMA")
  
  
  SimulMasculinos<-rgamma(100000,shape=0.01+EstadSuf$Suma[2],rate=0.01+EstadSuf$N[2]) 
  SimulFemeninos<-rgamma(100000,shape=0.01+EstadSuf$Suma[1],rate=0.01+EstadSuf$N[1])  
  EtasBayesianos<-(SimulFemeninos-SimulMasculinos)/SimulMasculinos
  
  #Información Bayesiana de Eta; Media, Intervalo de credibilidad 95% y 99%
  
  InfoPedidaBayes<-rbind(InfoPedidaBayes,c(year,mean(EtasBayesianos),round(quantile(x = EtasBayesianos, probs = c(0.025, 0.975)), 3),round(quantile(x = EtasBayesianos, probs = c(0.005, 0.995)), 3)))

  
  #Proceso de Bootstrap
  
  VectoresMasculinos<-sqldf("select TOTAL_VICTIMAS
                    from       Datos
                    where      SEXO_VICTIMA = 'MASCULINO'   ")
  
  VectoresFemeninos<-sqldf("select  TOTAL_VICTIMAS
                    from       Datos
                    where      SEXO_VICTIMA = 'FEMENINO'   ")
  
  EtasHats<-NULL
  
  for(i in 1:2000){
    ThetaHat1<-mean(sample(VectoresMasculinos[,1],size=EstadSuf$N[2],replace=TRUE))
    ThetaHat2<-mean(sample(VectoresFemeninos[,1],size=EstadSuf$N[1],replace=TRUE))
    EtaHat<-(ThetaHat2-ThetaHat1)/ThetaHat1
    EtasHats[i]<-EtaHat
    
  }
  
  #Información Frecuentista de Eta: Media, IC 95% y IC 99%
  
  InfoPedidaBoots<-rbind(InfoPedidaBoots,c(year,mean(EtasHats),round(quantile(EtasHats, probs = c(0.025,0.975)),3),round(quantile(EtasHats, probs = c(0.005,0.995)),3)))
  
  
  #Este es el gráfico de Bayes
  plot(density(EtasBayesianos), lwd = 2, lty=1 , add=0 , col = "green" , xlim = c(-0.3,0.8) , ylim = c(0,5) , xlab = expression(eta), ylab = "Densidad", main = paste("Densidad de eta en el año",as.character(year),"utilizando métodos de MonteCarlo y Bootstrap"))
  # abline(v = quantile(x = EtasBayesianos, probs = c(0.025, 0.975)), lty = 2, lwd = 2, col = "green")
  # abline(v = quantile(x = EtasBayesianos, probs = c(0.005, 0.995)), lty = 3, lwd = 2, col = "green")
  # abline(v = mean(EtasBayesianos), lty = 1, lwd = 1, col = "green")
  
  #Este es el gráfico de Bootstrap
  lines(density(EtasHats), col = "red", lwd = 2, lty = 1)
  # abline(v = quantile(x = EtasHats, probs = c(0.025, 0.975)), lty = 2, lwd = 3, col = "red")
  # abline(v = quantile(x = EtasHats, probs = c(0.005, 0.995)), lty = 3, lwd = 2, col = "red")
  #abline(v = mean(EtasHats), lty = 1, lwd = 1, col = "red")
  grid()
  
  #legend("right", legend = "Media", 
  #       lty=1, col="black", bty = "n",cex=0.8)
  legend("topright",legend=c("Densidad por Montecarlo","Densidad por Bootstrap"),fill=c("green","red"),bty="n",cex = 1)
  
}


InfoPedidaBayes
InfoPedidaBoots
#bquote(~"Densidad de la variable "~eta~"="~"("~theta[2]-theta[1]~")/"~theta[1]~" utilizando métodos de Monte Carlo y Bootstrap paramétrico en el año "
#####################################DESARROLLO PUNTO 2 Apartado 2 (punto más largo)#######################################

#Mejorar código en el futuro para reducir el tiempo de ejecución, aproximado de 12 horas

set.seed(1729)


y1<-mean(vectorMasculinos2022[,1]);y1
y2<-mean(vectorFemeninos2022[,1]);y2
etaReal<-y2/y1;etaReal

#Aquí evito restar 1 para no restar 1 en los siguientes vectores y así ahorrarme un poco de operaciones

### Escenarios

n1<-c(10,20,50,100)
ProporcionesEtaBayes<-NULL
ProporcionesEtaBoots<-NULL

#Experimental
for(n in 1:4){
  contadoretaBoots1<-0
  contadoretaBayes1<-0
  
  for (i in 1:100000) {
    
    poblMasculina<-rpois(n1[n], y1)
    poblFemenina<-rpois(n1[n], y2)
    estasufMas<-sum(poblMasculina)
    estasufFem<-sum(poblFemenina)
    
    #Procedimiento Bayesiano
    
    etasBayes1<-rgamma(1000,0.01+estasufFem, 0.01+n1[n])/rgamma(1000,0.01+estasufMas, 0.01+n1[n])
    if(etaReal>= quantile(etasBayes1, 0.025) & etaReal<= quantile(etasBayes1, 0.975)){
      contadoretaBayes1<-contadoretaBayes1+1}
    
    #Procedimiento Frecuentista
    
    etahat1<-NULL
    for (j in 1:1000) {
      etasBoots1<-mean(sample(poblFemenina, n1[n], replace = TRUE))/mean(sample(poblMasculina, n1[n], replace = TRUE))
      etahat1[j]<-etasBoots1
    }
    if(etaReal>= quantile(etahat1, 0.025) & etaReal<= quantile(etahat1, 0.975)){
      contadoretaBoots1<-contadoretaBoots1+1}
    
    
  }
  cat("El",25*n,"% del algoritmo se ha completado")
  ProporcionesEtaBayes[n]<-contadoretaBayes1
  ProporcionesEtaBoots[n]<-contadoretaBoots1
}


ProporcionesEtaBayes
ProporcionesEtaBoots


InfoFinal<-rbind(ProporcionesEtaBayes/100000,ProporcionesEtaBoots/100000)
colnames(InfoFinal)<-c("n=10","n=20","n=50","n=100")
InfoFinal<-cbind(c("EnfoqueBayesiano","EnfoqueFrecuentista"),InfoFinal)
InfoFinal
write.csv(InfoFinal,"TablaPorcentajesDef")
