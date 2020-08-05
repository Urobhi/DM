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
  

##### Levanto datos y sampleo ######
prostata = read.csv(file = paste(dirname(Parent_folder),"/AID-Datasets/Prostata.csv",sep=""), dec = ",")
prostata = sampleo(prostata,0.8)
seguros = read.csv(file = paste(dirname(Parent_folder),"/AID-Datasets/Seguros.csv",sep=""), dec = ",")
seguros = sampleo(seguros,0.75)



#### Clasificación supervisada ####

prostata = prostata[,-1]

