library(MASS)
library(ggplot2)
library(lattice)
library(grid)
library(DMwR)
library(readxl)
library(reshape)
library(GGally)
Parent_folder <- dirname(dirname(rstudioapi::getSourceEditorContext()$path))
source(paste(dirname(Parent_folder),"/Funciones.R", sep= ""))





##### Guia 2 ########

#####Ej 2.2.1

# Levanto datos y saco ID

Data <- read_excel(paste(Parent_folder,"/AID-Datasets/2_2 - internet.xlsx",sep=""))
Data <- subset(Data, select = -c(ID))

# Defino categoricas  
Cols_categorical <- c("Nacionalidad","Sexo","Interés")
Data[Cols_categorical] <- lapply(Data[Cols_categorical], as.factor)

nums <- unlist(lapply(Data, is.numeric))  
ggparcoord(Data[,nums],
           title = "Parallel Coordinates of numerical values",
           alphaLines = 0.3
           )  + 
  scale_color_discrete("Sexo",labels = levels(Data$Sexo))



#### Ej 2.2.2
barplot(table(Data$Sexo ), xlab="Sexo", ylab = "Frecuencia")
ftable(Data$Sexo)
## Ej 2.2.3
barplot(table(Data$Edad ), xlab="Edad", ylab = "Frecuencia")
# Ej 2.2.4
barplot(table(Data$Interés ), xlab="Edad", ylab = "Frecuencia")
#Ej 2.2.5 
barplot(table(Data$Temperatura ), xlab="Edad", ylab = "Frecuencia")
barplot(table(Data$Autos ), xlab="Edad", ylab = "Frecuencia")
barplot(table(Data$Cigarrillos ), xlab="Edad", ylab = "Frecuencia")

#Ej 2.2.6
quants = c(0.25, 0.5, 0.75)
Quantiles <- sapply(Data[nums], quantile, probs = quants)
Intercuartil <-sapply(Data[nums], IQR)
Data=Data[Data$Sexo!=0 & Data$Edad> 0 & Data$Edad< (Quantiles[3,.3  "Edad"]+1.5*Intercuartil["Edad"]) & Data$Autos<1000,]

#Ej 2.2.7
ggplot(stack(Data[nums]), aes(x=ind, y = values)) +
  geom_boxplot()

#Ej 2.2.8
Detect_outliers(Data$Edad)


### Ejercicio 2.3
Parent_folder <- dirname(dirname(rstudioapi::getSourceEditorContext()$path))
