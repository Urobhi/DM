library(MASS)
library(ggplot2)
library(lattice)
library(grid)
library(DMwR)
library(readxl)
library(reshape)
library(GGally)
Parent_folder <- dirname(dirname(rstudioapi::getSourceEditorContext()$path))
source(paste(dirname(Parent_folder),"/Funciones.R"))


Data <- read_excel(paste(Parent_folder,"/AID-Datasets/Datos_Alumnos_2020.xlsx",sep=""))
Data <- data.frame(Data$Comision,Data$Titulo2,Data$Universidad2,Data$Genero,Data$Edad)

hist(x=Data$Data.Edad, breaks = 10)
boxplot(x = Data$Data.Edad)




# Punto 4.1
Freq_Table <- sort(table(Data$Data.Titulo2))
Titulo_max_freq <- names(Freq_Table[which.max(Freq_Table)])

# Punto 4.2  

Freq_Table <- sort(table(Data$Data.Universidad2)) 
Univ_max_freq <- names(Freq_Table[which.max(Freq_Table)])

# Punto 5
