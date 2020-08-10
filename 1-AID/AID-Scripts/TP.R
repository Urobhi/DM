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

sampleo <-function(dataset,split)
  {
    dni = 31234567
    n = round(split* nrow(dataset))
    set.seed(dni)
    mask = sample(1:nrow(dataset),size=n,replace=FALSE)
    dataset = dataset[mask,]
    return(dataset)
  }

DataTypes <- function(dataset)
  {
    # elimino el id
    dataset = dataset[,-1] 
    # Seteo como factores
    dataset$CAPSULE = as.factor(dataset$CAPSULE)
    dataset$RACE = as.factor(dataset$RACE)
    dataset$DPROS = as.factor(dataset$DPROS)
    dataset$DCAPS = as.factor(dataset$DCAPS)
    
    # Seteo como numeros
    dataset$PSA = as.numeric(dataset$PSA)
    dataset$VOL = as.numeric(dataset$VOL)
    return(dataset)
  }
  

##### Levanto datos y sampleo ######
prostata = read.csv(file = paste(dirname(Parent_folder),"/AID-Datasets/Prostata.csv",sep=""), dec = ",")
prostata = sampleo(prostata,0.8)
prostata2 = read.csv(file = paste(dirname(Parent_folder),"/AID-Datasets/Prostata2.csv",sep=""), dec = ",")
prostata2 = sampleo(prostata,0.8)
seguros = read.csv(file = paste(dirname(Parent_folder),"/AID-Datasets/Seguros.csv",sep=""), dec = ",")
seguros = sampleo(seguros,0.75)

prostata <- DataTypes(prostata)



#### Clasificación supervisada ####




ggplot(data = prostata, mapping = aes(x = CAPSULE, y = PSA, colour = CAPSULE)) + 
  geom_boxplot() + theme_bw() + theme(legend.position = "none")
  

ggplot(data = prostata, mapping = aes(x = CAPSULE, y = VOL, colour = CAPSULE)) + 
  geom_boxplot() + theme_bw() + theme(legend.position = "none")

ggplot(data = prostata, mapping = aes(x = CAPSULE, y = GLEASON, colour = CAPSULE)) + 
  geom_boxplot() + theme_bw() + theme(legend.position = "none")

ggplot(data = prostata, mapping = aes(x = CAPSULE, y = AGE, colour = CAPSULE)) + 
  geom_boxplot() + theme_bw() + theme(legend.position = "none")

xtabs(~CAPSULE+RACE, data=prostata)
xtabs(~CAPSULE+DPROS, data=prostata)
xtabs(~CAPSULE+DCAPS, data=prostata)


