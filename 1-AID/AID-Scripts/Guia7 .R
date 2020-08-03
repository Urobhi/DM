library(MASS)
library(ggplot2)
library(lattice)
library(grid)
library(DMwR)
library(readxl)
library(reshape)
library(GGally)
library(ca)
library(FactoMineR)
library(factoextra)
library (anacor) 
library(ade4)
library(data.table)
library(stats)
library(reshape2)
library(car)
library(nortest)
library(MASS)
library(pgirmess)
GrandParent_folder <- dirname(dirname(rstudioapi::getSourceEditorContext()$path))
Parent_folder <- dirname(rstudioapi::getSourceEditorContext()$path)
source(paste(dirname(GrandParent_folder),"/Funciones.R", sep= ""))
source(paste(dirname(Parent_folder),"/ggbiplot.r", sep= ""))
source(paste(dirname(Parent_folder),"/ggscreeplot.r", sep= ""))



##### Guia 7 ########

A=data.frame(matrix ( c (0.17,0.21,0.18,0.20,0.26,0.22,0.33,0.30,0.19,0.28,0.23,0.32,0.34,0.25,0.16,0.20,0.52,0.90,0.19,0.19,0.33,0.33,0.30,0.22,0.23,0.22,0.21,0.27,0.20,0.17,0.20,0.24,0.18,0.39,0.16,0.29,0.22,0.27,0.21,0.27) , nrow=20 , ncol =2 , byrow=T))
colnames(A) = c("Tiempo1","Tiempo2")
A = melt(A)                             
# Normalidad --> Shapiro
ggplot(data = A, mapping = aes(x = variable, y = value, colour = variable)) + 
  geom_boxplot() + theme_bw() + theme(legend.position = "none")
shapiro.test(A$value)
qqnorm(A$value)
qqline(A$value)
B <- boxcox(value~variable,data=A,plotit = TRUE)
A$value <- (A$value)^(-1)
shapiro.test(A$value)
qqnorm(A$value)
qqline(A$value)

# Test no parametrico para probar si provienen de la misma poblacion

kruskal.test(value~variable, data=A )

kruskalmc(value~variable, data=A)


### 2

Marcas = data.frame(matrix(c(24.4,10.2,19.2,17.4,13.4,21.3,22.6,12.1,19.4,18.1,15,20.2,23.8,10.3,19.8,16.7,14.1,20.7,22.0,10.2,19.0,18.3,13.1,20.8,24.5,9.9,19.6,17.6,14.9,20.1,22.3,11.2,18.3,17.5,15.0,18.8,25.0,12.0,20.0,18.0,13.4,21.1,24.5,9.5,19.4,16.4,14.8,20.3),nrow=8,ncol=6,byrow=T))
colnames(Marcas) = c("Marca1","Marca2","Marca3","Marca4","Marca5","Marca6")                    
Marcas = melt(Marcas)


ggplot(data = Marcas, mapping = aes(x = value, colour = variable)) + 
  geom_histogram() + theme_bw() + facet_grid(. ~ variable) + 
  theme(legend.position = "none")# + stat_bin(binwidth=30)


aggregate( value ~  variable,Marcas, FUN=mean)
aggregate( value ~  variable,Marcas, FUN=var)

Marcas.anova <- aov(value ~  variable, data=Marcas)
summary(Marcas.anova)  # Rechazamos igualdad de medias con toda segudirdad

qqnorm(resid(Marcas.anova))
qqline(resid(Marcas.anova))

shapiro.test(residuals(Marcas.anova)) # normalidad de los residuos, vamos bien

ggplot(data = Marcas, mapping = aes(x = variable, y = value, colour = variable)) + 
  geom_boxplot() + theme_bw() + theme(legend.position = "none")

leveneTest(value ~  variable, data=Marcas) # no rechaza heterocedasticidad, vamos bien

# como se cumplen los requisitos para que el anova sea valido, rechazamos.


#### 3

Suplementos = data.frame(matrix(c(3.3,4.6,6.7,6.3,4.4,4.5,5.8,6,4.9,5.0,5.0,6.7,4.9,4.0,4.8,5.5,3.9,4.5,5.3,6.6,4.2,5.2,6.2,6.1,4.7,4.9,5.0,5.3,5.1,5.5,6.4,6.5,4.6,4.8,5.9,6.3,4.5,5.3,5.4,6.8),nrow=10,ncol = 4,byrow=T))
colnames(Suplementos) = c("S1","S2","S3","S4")
Suplementos= melt(Suplementos)

ggplot(data = Suplementos, mapping = aes(x = value, colour = variable)) + 
  geom_histogram() + theme_bw() + facet_grid(. ~ variable) + 
  theme(legend.position = "none")# + stat_bin(binwidth=30)

ggplot(data = Suplementos, mapping = aes(x = variable, y = value, colour = variable)) + 
  geom_boxplot() + theme_bw() + theme(legend.position = "none")

Suplementos.anova <- aov(value ~  variable, data=Suplementos)
summary(Suplementos.anova)

qqnorm(resid(Suplementos.anova))
qqline(resid(Suplementos.anova))

shapiro.test(residuals(Suplementos.anova))
leveneTest(value ~  variable, data=Suplementos) # no rechaza heterocedasticidad, vamos bien

TukeyHSD(Suplementos.anova,conf.level=0.95)


####4
Placebo= rnorm(10,mean = 2.5,sd = 0.13)
Aspirina= rnorm(10,mean = 2.82,sd = 0.20)
Droga = rnorm(10,mean = 3.2,sd=0.17)
t.test(Placebo,Aspirina, conf.level = 0.05/3)
t.test(Placebo,Droga, conf.level = 0.05/3)
t.test(Aspirina,Droga, conf.level = 0.05/3)
