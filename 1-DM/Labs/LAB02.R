library(readr)
library(corrplot)
library(sqldf)
Parent_folder <- dirname(dirname(rstudioapi::getSourceEditorContext()$path))
library(ggplot2)
library(data.table)
library(infotheo)

source(paste(Parent_folder,"/Funciones.R", sep= ""))
MPI_Subnational <- read_csv("https://raw.githubusercontent.com/dmuba/dmuba.github.io/master/Practicos/LAB01/MPI_subnational.csv")
MPI_National <- read_csv("https://raw.githubusercontent.com/dmuba/dmuba.github.io/master/Practicos/LAB02_2020/MPI_national.csv")

# Releve características de los atributos

str(MPI_National)
summary(MPI_National)
str(MPI_Subnational)
summary(MPI_Subnational)


# Integración de datos 

# Usando SQLDF

Query = "
  SELECT 
    SN.*,N.'MPI Urban',N.'MPI Rural',N.'Headcount Ratio Urban', N.'Headcount Ratio Rural', N.'Intensity of Deprivation Rural',N.'Intensity of Deprivation Urban'
  FROM 
    MPI_Subnational  SN
  LEFT JOIN MPI_National N
    on SN.'ISO country code'= N.ISO"

Integrated_DF = sqldf(Query)


# Verificación de datos redundantes


# Verifico cual es la distribución de los datos

nums <- unlist(lapply(Integrated_DF, is.numeric))  
Numericos <- colnames(Integrated_DF[nums])
Integrated_DF_numericos <- Integrated_DF[Numericos]
boxplot(Integrated_DF_numericos[,!(colnames(Integrated_DF_numericos) %like% "MPI")])
boxplot(Integrated_DF[,colnames(Integrated_DF) %like% "MPI"])  


# Elijko : Head count ratio Regional
plot(sort(Integrated_DF$`Headcount Ratio Regional`) , type = "l", col="red", ylab = "HC Ratio Regional", xlab = "Observaciones", main = "Dato original vs suavizado")
for (i in 2:10)
{
  bin_eq_freq <- discretize(Integrated_DF$`Headcount Ratio Regional`,"equalfreq",i)
  for(bin in 1:i)
  {
    bin_eq_freq$suavizado[ bin_eq_freq$X==bin] = mean(bin_eq_freq$Sepal.Width[ bin_eq_freq$X==bin])
  }
}

#Elijo MPI Regional