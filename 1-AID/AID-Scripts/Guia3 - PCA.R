library(MASS)
library(ggplot2)
library(lattice)
library(grid)
library(DMwR)
library(readxl)
library(reshape)
library(GGally)
GrandParent_folder <- dirname(dirname(rstudioapi::getSourceEditorContext()$path))
Parent_folder <- dirname(rstudioapi::getSourceEditorContext()$path)
source(paste(dirname(GrandParent_folder),"/Funciones.R", sep= ""))
source(paste(dirname(Parent_folder),"/ggbiplot.r", sep= ""))
source(paste(dirname(Parent_folder),"/ggscreeplot.r", sep= ""))



##### Guia 3 ########

A=matrix ( c (3,1,1,1,3,1,1,1,5) , nrow=3 , ncol =3 , byrow=T)

# Autovalores
A_AvalAvec = eigen(A)
A_Cov = cov(A)
A_Cov
A_PCA = prcomp(A,center = TRUE, scale. = FALSE)

carga1=data.frame (cbind (tramo =1:nrow(A_PCA$rotation) , primcarga=data.frame ( A_PCA$rotation) [,1]))
carga2=data.frame(cbind(carga1,seccarga=data.frame ( A_PCA$rotation) [,2]))
carga2=data.frame(cbind(carga2,terccarga=data.frame ( A_PCA$rotation) [,3]))

ggscreeplot(A_PCA, type = c('pev','cev')) +
  xlab('Componente principal') +
  ylab('Proporcion de la variabilidad explicada')+
  geom_line(colour='royalblue')+
  geom_point(colour='royalblue')

vector <- c(2,2,1) 
valor4 <- vector %*% as.matrix(carga2[,2:3])
valor4 


###### 2
chalets = read.csv(file = paste(dirname(Parent_folder),"/AID-Datasets/3_2 - chalets.csv",sep=""), dec = ",")
chalets = chalets[,2:4]
boxplot(chalets$Duracion.hipoteca..anos.)
boxplot(chalets$Precio.medio..millones.euros.)
boxplot(chalets$Superficie..cocina..m2.)

pairs(chalets)

chalets.means = apply(chalets, 2, mean)
chalets.cor = cor(chalets)
chalets.cor
chalets.cov = cov(chalets)
chalets.cov

#Componentes princip
chalets.PCA = prcomp(chalets,center = TRUE, scale. = TRUE)

#  Primera carga
chalets.carga1 = data.frame(cbind(variable=1:3, PC1=data.frame(chalets.PCA$rotation)[,1]))

ggplot(chalets.carga1, aes(variable,PC1)) +
    geom_bar(stat="identity", position="dodge", fill = "royalblue", width = 0.5)+
    xlab('Tramo') + ylab("primera_carga")

ggscreeplot(chalets.PCA, type = c('pev','cev')) +
  xlab('Componente principal') +
  ylab('Proporcion de la variabilidad explicada')+
  geom_line(colour='royalblue')+
  geom_point(colour='royalblue')


#####3

A=matrix ( c (3,6,5,6,10,12) , nrow=3 , ncol =2 , byrow=T)

# Autovalores
A_Cov = cov(A)
A_Cov
A.AvalAvec = eigen(A_Cov)
A.AvalAvec
A = scale(A, center=TRUE, scale=TRUE)
A.PCA = prcomp(A)
A=data.frame(A)

A.rotated = data.frame(as.matrix(A) %*% as.matrix(A.PCA$rotation))

ggplot(A,aes( x = A[,1], y=A[,2])) +
  geom_point()+w
  xlim(-16,16) + ylim(-16,16)


ggplot(A,aes( x = A.rotated[,1], y=A.rotated[,2])) +
  geom_point()+
  xlim(-16,16) + ylim(-16,16)

#####5

gorriones = read_excel( paste(dirname(Parent_folder),"/AID-Datasets/gorriones.xlsx",sep=""))
gorriones.scale = scale(gorriones,center = TRUE, scale = TRUE)
gorriones.scale.cov = cov(gorriones.scale)
gorriones.scale.cov

gorriones.PCA = prcomp(gorriones.scale)
  
ggscreeplot(gorriones.PCA, type = c('pev','cev')) +
xlab('Componente principal') +
ylab('Proporcion de la variabilidad explicada')+
geom_line(colour='royalblue')+
geom_point(colour='royalblue')




# 
A = data.frame(matrix(c(81,103,147,359,326,277),nrow = 2,ncol = ,byrow = T))
Test = chisq.test(A)




