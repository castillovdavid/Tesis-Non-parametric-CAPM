rm(list=ls())
#install.packages("dplyr")
library(dplyr)
#install.packages("psych")
library(psych)
#install.packages("lmtest")
library(lmtest)
#install.packages("PerformanceAnalytics")
library(PerformanceAnalytics)
#install.packages("tseries")
library(tseries)
#install.packages("car")
library(car)
#install.packages("readr")
library(readr)
#install.packages("ggplot2")
library(ggplot2)
#install.packages("KernSmooth")
library(KernSmooth)
#install.packages("stargazer")
library(stargazer)
#install.packages("openxlsx")
library(openxlsx)
#install.packages("sm")
library(sm)
#install.packages("np")
library(np)
#install.packages("fANCOVA")
library(fANCOVA)
#install.packages("Deriv")
library(Deriv)
#install.packages("locfit")
library(locfit)
#install.packages("cowplot")
library(cowplot)





##### CARGA DE DATOS / marzo 2012 - marzo 2022 #####
Bonos <- read_csv("C:/Users/casti/Desktop/Proyecto de Título/Scripts/Datos/Chile 1 año Datos Históricos Rendimiento de Bonos.csv")
BLUMAR <- read_csv("C:/Users/casti/Desktop/Proyecto de Título/Scripts/Datos/Datos históricos BLUMAR.csv")
CAMANCHACA <- read_csv("C:/Users/casti/Desktop/Proyecto de Título/Scripts/Datos/Datos históricos CAMANCHACA.csv")
IGPA <- read_csv("C:/Users/casti/Desktop/Proyecto de Título/Scripts/Datos/Datos históricos S&P CLX IGPA.csv")
IPSA <- read_csv("C:/Users/casti/Desktop/Proyecto de Título/Scripts/Datos/Datos históricos S&P CLX IPSA.csv")

#hola
Bonos$Último <- as.numeric(gsub("," , ".",Bonos$Último))

# RECODIFICACIÓN DE FECHA
fecha<-as.Date(IGPA$Fecha,format = "%d.%m.%Y")
T_Bonos<- Bonos[,2];T_Bonos
T_IGPA<- IGPA[,2]*1000; T_IGPA
T_IPSA <- IPSA[,2]*1000; T_IPSA
P_BLUMAR<- BLUMAR[,2]/100; P_BLUMAR
P_CAMANCHACA<- CAMANCHACA[,2]/100; P_CAMANCHACA

#CREACIÓN DE BASE DE DATOS CON PRECIOS DE ACCIONES DE LAS EMPRESAS  además de la tasa del bono soberano###
precios <- data.frame(fecha,T_Bonos,T_IGPA,P_BLUMAR,P_CAMANCHACA,T_IPSA)
names(precios)<-c("fechas","Bono","Igpa","Blumar","Camanchaca","Ipsa")

#Orden por fechas
precios <- precios %>% arrange(fechas)
View(precios)
#### CALCULO DE RETORNO LOGARITMICO O RENTABILIDAD LOGARITMICA ####

#  OPCION 1 

r_ipsa <- diff(log(precios$Ipsa))*100
r_igpa <- diff(log(precios$Igpa))*100
r_blumar <- diff(log(precios$Blumar))*100
r_cam <- diff(log(precios$Camanchaca))*100
rf <- precios$Bono[2:121]

retornos <- data.frame(fecha = precios$fechas[2:121],r_ipsa,r_igpa,r_blumar,r_cam,rf)

# EXPORTAR LOS DATOS
write.xlsx(retornos,"C:/Users/casti/Desktop/retornos.xlsx")


#  OPCION 2 

#SE CALCULAN LOS RETORNOS Y SE AÑADEN A LA MISMA BASE DE DATOS 
df <- precios %>% mutate ( r_ipsa = log(Ipsa/lag(Ipsa))*100,
                         r_igpa = log(Igpa/lag(Igpa))*100,
                         r_blum = log(Blumar/lag(Blumar))*100,
                         r_Cam  = log(Camanchaca/lag(Camanchaca))*100) 

write.xlsx(df,"C:/Users/casti/Desktop/df.xlsx")
# GRAFICAMOS EL INDICE GENERAL DE PRECIOS DE ACCIONES ( IGPA )
df
g1<-ggplot(data = df, aes(x = fechas, y = Igpa))+
  geom_line(color = "#00AFBB", size = 1) +  
  ggtitle("IGPA periodo 2012-2022")+
  xlab("")+
  ylab("CLP")
g1


#ÍNDICE DE PRECIOS SELECTIVO DE ACCIONES ( IPSA )

g2<-ggplot(data = df, aes(x = fechas, y = Ipsa))+
  geom_line(color = "#00AFBB", size = 1) +  
  ggtitle("IPSA periodo 2012-2022")+
  xlab("")+
  ylab("CLP")
g2



# GRAFICANDO LA TASA LIBRE DE RIESGO ( BONOS DEL BANCO CENTRAL )
g3<-ggplot(data = df, aes(x = fechas, y = Bono))+
  geom_line(color = "#00AFBB", size = 1) +  
  ggtitle("Bonos del Banco Central a 1 año periodo 2012-2022")+
  xlab("tiempo") + ylab("%")
g3

# GRAFICO DE LOS PRECIOS DE LAS ACCIONES
g4<-ggplot(df, aes(fechas)) +
  geom_line(aes(y = Blumar, colour = "Blumar")) +
  geom_line(aes(y = Camanchaca, colour = "Camanchaca")) +
  scale_colour_hue("")+
  ggtitle("Precio de acciones")+
  xlab("") + ylab("Valor")
g4

g5<- ggplot(df,aes(x=fechas,y=Blumar))+
     geom_line(color="#00AFBB")+
     ggtitle("Precio / Acción Blumar Periodo 2012-2022")+
     xlab("")+
     ylab("CLP")
g5


g6<- ggplot(df,aes(x=fechas,y=Camanchaca))+
  geom_line(color="#00AFBB")+
  ggtitle("Precio / Acción Camanchaca Periodo 2012-2022")+
  xlab("")+
  ylab("CLP")
g6

# GRAFICO DE LOS RETORNOS DE ÍNDICES DE MERCADO
g7<- ggplot(retornos,aes(fecha))+
  geom_line(aes(y=r_ipsa, colour = "IPSA"))+
  geom_line(aes(y=r_igpa, colour = "IGPA"))+
  scale_colour_hue("")+
  ggtitle("Tasa de retorno índices de mercado")+
  xlab("")+
  ylab("Tasa (%)")
g7
  
# GRÁFICO DE RETORNOS DE LOS EMPRESAS

g8<- ggplot(retornos, aes(fecha))+
     geom_line(aes(y=r_cam, colour="Camanchaca"),size=0.8)+
     geom_line(aes(y=r_blumar, colour="Blumar"),size=0.8)+
     scale_colour_hue("")+
     ggtitle("Retorno de Acciones")+
     xlab("Tasa (%)")+
     ylab("")
g8

# SI QUEREMOS ESTABLECER UNA RELACIÓN LINEAL ENTRE EL RETORNO DEL MERCADO
# Y EL RETORNO DE CADA EMPRESA ENTONCES IDEALMENTE PODEMOS VER LOS GRAFICOS DE DISPERSIÓN

g9<- ggplot(retornos, aes(r_igpa-rf,r_blumar-rf))+geom_point()+
     xlab("IGPA - rf ")+
     ylab("Blumar - rf")+
     geom_smooth(method="lm",col="red")+
     ggtitle("Relación retornos IGPA-BLUMAR")
g9

df
g10<- ggplot(retornos, aes(r_igpa-rf,r_cam-rf))+geom_point()+
  xlab("IGPA-rf")+
  ylab("Camanchaca-rf")+
  geom_smooth(method="lm",col="red")+
  ggtitle("Relación retornos IGPA VS CAMANCHACA")
g10

retornos <- retornos %>% mutate(rm_rf = r_igpa-rf, 
                                blumar_rf = r_blumar-rf,
                                cam_rf = r_cam-rf)


#### CALCULO BETA POR DEFINICIÓN Y REGRESIÓN LINEAL SIMPLE ####

### BLUMAR ###

#REGRESIÓN
lm1<- lm(formula = blumar_rf ~ rm_rf, data=retornos)
summary(lm1)

#DEFINICIÓN
cov(retornos$rm_rf,retornos$blumar_rf)/var(retornos$rm_rf)

### CAMANCHACA ###

#REGRESIÓN
lm2 <- lm(formula = cam_rf~rm_rf,data=retornos)

summary(lm2)


#DEFINICIÓN


cov(retornos$cam_rf,retornos$rm_rf)/var(retornos$rm_rf)

#SE PUEDE APRECIAR QUE EL CALCULO Y LA ESTIMACIÓN ES IDÉNTICA

attach(retornos)

#### FUNCIONES NÚCLEO MANUALMENTE ####

manual_loess <- function (x,y,h){
  W <- matrix(0, nrow = length(x), ncol = length(x))
  for (i in 1:length(x)) {
    w<- 1/sqrt(2*pi)*exp(-0.5 * ((x - x[i]) / h)^2)
    W[i, i] <- w[i] / sum(w)
  }
  X<-cbind(1,x)
  Y<-y
  beta <- solve(t(X) %*% W %*% X) %*% t(X) %*% W %*% Y
  return(beta)
}

manual_loess(retornos$rm_rf,retornos$r_cam,0.4)
result<-c()
result2<-c()

m_hat <- function(x,y,h){
  for (i in 1:length(x)) {
    s <- 0 
    for (j in 1:length(x)) {
      s <- s + 1/sqrt(2*pi)*exp(-0.5 * ((x[i]- x[j]) / h)^2)*y[j]
      
    }
    result[i] <- s
  }
  for (i in 1:length(x)) {
    f <- 0 
    for (j in 1:length(x)) {
      f <- f + 1/sqrt(2*pi)*exp(-0.5 * ((x[i]- x[j]) / h)^2)
      
    }
    result2[i] <- f
  }
  return(result/result2)
}

##### MODELOS NO PARAMÉTRICOS #####

##### BLUMAR ####
x <- retornos$rm_rf
y <- retornos$blumar_rf


#CONSIDEREMOS DISTINTOS ALPHA, CON EL OBJETIVO DE VER EL CAMBIO EN EL AJUSTE.
#EN ESTE CASO SERÁ DESDE 0.65 A 0.9 CON DISTANCIA DE 0.05

lambda04<-loess(y~x,degree = 1,family = "gaussian",span = 0.4)
lambda05<-loess(y~x,degree = 1,family = "gaussian",span = 0.5)
lambda06<-loess(y~x,degree = 1,family = "gaussian",span = 0.6)
lambda07<-loess(y~x,degree = 1,family = "gaussian",span = 0.7)
lambda08<-loess(y~x,degree = 1,family = "gaussian",span = 0.8)
lambda09<-loess(y~x,degree = 1,family = "gaussian",span = 0.9)

lambda_blumar <- data.frame(rm_rf = x,blumar_rf = y)
lambda_blumar <- lambda_blumar %>%  mutate(l4 =lambda04$fitted, l5 = lambda05$fitted, l6= lambda06$fitted,
                         l7 =lambda07$fitted, l8 = lambda08$fitted,l9 =lambda09$fitted)
head(lambda_blumar)



View(lambda_blumar)
g11<-ggplot(lambda_blumar, aes(rm_rf, blumar_rf)) +
     geom_point() +
     geom_line(aes(rm_rf, l4),col="red", size = 1) +
     labs(title = "Alpha = 0.4",
          x = "Rm - Rf",
          y = "R_Blumar - Rf",
          color = "",
          linetype = "") 

g12<-ggplot(lambda_blumar, aes(rm_rf, blumar_rf)) +
     geom_point() +
     geom_line(aes(rm_rf, l5),col="red", size = 1) +
     labs(title = "Alpha = 0.5",
          x = "Rm - Rf",
          y = "R_Blumar - Rf",
          color = "",
          linetype = "") 

g13<-ggplot(lambda_blumar, aes(rm_rf, blumar_rf)) +
  geom_point() +
  geom_line(aes(rm_rf, l6),col="red", size = 1) +
  labs(title = "Alpha = 0.6",
       x = "Rm - Rf",
       y = "R_Blumar - Rf",
       color = "",
       linetype = "") 
g14<-ggplot(lambda_blumar, aes(rm_rf, blumar_rf)) +
     geom_point() +
     geom_line(aes(rm_rf, l7),col="red", size = 1) +
     labs(title = "Alpha = 0.7",
          x = "Rm - Rf",
          y = "R_Blumar - Rf",
          color = "",
          linetype = "")
g15<-ggplot(lambda_blumar, aes(rm_rf, blumar_rf)) +
     geom_point() +
     geom_line(aes(rm_rf, l8),col="red", size = 1) +
     labs(title = "Alpha = 0.8",
          x = "Rm - Rf",
          y = "R_Blumar - Rf",
          color = "",
          linetype = "") 
g16<-ggplot(lambda_blumar, aes(rm_rf, blumar_rf)) +
     geom_point() +
     geom_line(aes(rm_rf, l9),col="red", size = 1) +
     labs(title = " Alpha = 0.9",
          x = "Rm - Rf",
          y = "R_Blumar - Rf",
          color = "",
          linetype = "") 

require(gridExtra)
grid.arrange(g11,g12,g13,g14,g15,g16,nrow=2)



#### MODELO NO PARAMETRICO ADECUADO PARA BLUMAR ####

loess.as(x,y,degree=1,criterion = "aicc",family = "gaussian")
np_blumar <- loess.as(x,y,degree = 1,criterion = "gcv",family = "gaussian")
summary(np_blumar)

#### BETA NO PARAMÉTRICO BLUMAR #### 
manual_loess(x,y,0.8642)

np_blu<-loess(y~x,degree = 1,family = "gaussian",span = 0.8642)

#AGREGAMOS LOS RESULTADOS DE LOS MODELOS LINEALES Y NO LINEALES A LA BASE RETORNOS
retornos <- retornos %>% mutate(blunp = np_blu$fitted)
retornos <- retornos %>% mutate(blulm = lm1$fitted.values)

ggplot(retornos, aes(rm_rf, blumar_rf)) +
  geom_point() +
  geom_line(aes(rm_rf, blunp, color = "red"), size = 1) +
  #geom_ribbon(aes(ymin = blunp - 1.96 * sd(blunp),
  #               ymax = blunp + 1.96 * sd(blunp)),
  #          fill = "indianred", alpha = 0.2) +
  geom_smooth(aes(linetype = "dashed"), formula = y ~ x, method = "lm", size = 1, se = FALSE,color="blue") +
  labs(title = "",
       x = "Rm - Rf",
       y = "R_Blumar - Rf",
       color = "",
       linetype = "") +
  scale_color_manual(values = c("red","blue"), labels = c("Regresión Kernel alpha 0.86 ", "Regresión Lineal")) +
  scale_linetype_manual(values = c("dashed"), labels = "Regresión Lineal") +
  theme(legend.position = "top",  
        legend.key.size = unit(1.5, "lines"))+
  ggtitle("")


R_2 <- function(y, fitted) {
  SSE <- sum((y - fitted)^2)
  SST <- sum((y - mean(y))^2)
  R2 <- 1 - (SSE / SST)
  return(R2)
}


#### CAMANCHACA ####
x<-retornos$rm_rf
y<-retornos$cam_rf

lambda04<-loess(y~x,degree = 1,family = "gaussian",span = 0.4)
lambda05<-loess(y~x,degree = 1,family = "gaussian",span = 0.5)
lambda06<-loess(y~x,degree = 1,family = "gaussian",span = 0.6)
lambda07<-loess(y~x,degree = 1,family = "gaussian",span = 0.7)
lambda08<-loess(y~x,degree = 1,family = "gaussian",span = 0.8)
lambda09<-loess(y~x,degree = 1,family = "gaussian",span = 0.9)

lambda_cam <- data.frame(rm_rf = x,cam_rf = y)
lambda_cam <- lambda_cam %>%  mutate(l4 =lambda04$fitted, l5 = lambda05$fitted, l6= lambda06$fitted,
                                           l7 =lambda07$fitted, l8 = lambda08$fitted,l9 =lambda09$fitted)
head(lambda_cam)



g17<-ggplot(lambda_cam, aes(rm_rf, cam_rf)) +
  geom_point() +
  geom_line(aes(rm_rf, l4),col="red", size = 1) +
  labs(title = "Alpha = 0.4",
       x = "Rm - Rf",
       y = "R_Camanchaca - Rf",
       color = "",
       linetype = "") 

g18<-ggplot(lambda_cam, aes(rm_rf, cam_rf)) +
  geom_point() +
  geom_line(aes(rm_rf, l5),col="red", size = 1) +
  labs(title = "Alpha = 0.5",
       x = "Rm - Rf",
       y = "R_Camanchaca - Rf",
       color = "",
       linetype = "") 

g19<-ggplot(lambda_cam, aes(rm_rf, cam_rf)) +
  geom_point() +
  geom_line(aes(rm_rf, l6),col="red", size = 1) +
  labs(title = "Alpha = 0.6",
       x = "Rm - Rf",
       y = "R_Camanchaca - Rf",
       color = "",
       linetype = "") 
g20<-ggplot(lambda_cam, aes(rm_rf, cam_rf)) +
  geom_point() +
  geom_line(aes(rm_rf, l7),col="red", size = 1) +
  labs(title = "Alpha = 0.7",
       x = "Rm - Rf",
       y = "R_Camanchaca - Rf",
       color = "",
       linetype = "")
g21<-ggplot(lambda_cam, aes(rm_rf, cam_rf)) +
  geom_point() +
  geom_line(aes(rm_rf, l8),col="red", size = 1) +
  labs(title = "Alpha = 0.8",
       x = "Rm - Rf",
       y = "R_Camanchaca - Rf",
       color = "",
       linetype = "") 
g22<-ggplot(lambda_cam, aes(rm_rf, cam_rf)) +
  geom_point() +
  geom_line(aes(rm_rf, l9),col="red", size = 1) +
  labs(title = " Alpha = 0.9",
       x = "Rm - Rf",
       y = "R_Camanchaca - Rf",
       color = "",
       linetype = "") 

require(gridExtra)
grid.arrange(g17,g18,g19,g20,g21,g22,nrow=2)



#### MODELO NO PARAMETRICO ADECUADO PARA CAMANCHACA ####

np_cam <- loess.as(x,y,degree = 1,criterion = "gcv",family = "gaussian")
summary(np_cam)

#### BETA NO PARAMÉTRICO CAMANCHACA #### 
manual_loess(x,y,0.8927921)

np_cam<-loess(y~x,degree = 1,family = "gaussian",span = 0.8927921)

#AGREGAMOS LOS RESULTADOS DE LOS MODELOS LINEALES Y NO LINEALES A LA BASE RETORNOS
retornos <- retornos %>% mutate(camnp = np_cam$fitted)
retornos <- retornos %>% mutate(camlm = lm2$fitted.values)

ggplot(retornos, aes(rm_rf, cam_rf)) +
  geom_point() +
  geom_line(aes(rm_rf, camnp, color = "red"), size = 1) +
  geom_smooth(aes(linetype = "dashed"), formula = y ~ x, method = "lm", size = 1, se = FALSE,color="blue") +
  labs(title = "",
       x = "Rm - Rf",
       y = "R_Camanchaca - Rf",
       color = "",
       linetype = "") +
  scale_color_manual(values = c("red","blue"), labels = c("Regresión Kernel alpha 0.89 ", "Regresión Lineal")) +
  scale_linetype_manual(values = c("dashed"), labels = "Regresión Lineal") +
  theme(legend.position = "top",  
        legend.key.size = unit(1.5, "lines"))+
  ggtitle("")
