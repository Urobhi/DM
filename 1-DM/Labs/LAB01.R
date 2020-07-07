
library(readr)
library(corrplot)
library(ggplot2)
Parent_folder <- dirname(dirname(rstudioapi::getSourceEditorContext()$path))
source(paste(Parent_folder,"/Funciones.R", sep= ""))
Data <- read_csv("https://raw.githubusercontent.com/dmuba/dmuba.github.io/master/Practicos/LAB01/MPI_subnational.csv")


# Releve características de los atributos
str(Data)
summary(Data)


# Refresento gráficamente la cantidad de ciudades agrupadas por region
barplot(table(Data$`World region`), xlab = "Región", ylab = "Frecuencia", main = "Cantidad por especie", las=2)


# Medidas de posicion para atributos numéricos y agrupe de a cuerdo a región

nums <- unlist(lapply(Data, is.numeric))  
Numericos <- colnames(Data[nums])

.data <- Data
attach(.data)
media <- aggregate( data.frame(`MPI National`,`MPI Regional`,`Headcount Ratio Regional`,`Intensity of deprivation Regional`), by = list(`World region`), FUN = mean)
colnames(media)[2:length(colnames(media))] <- paste('Mean_',colnames(media)[2:length(colnames(media))])
moda <-  aggregate( data.frame(`MPI National`,`MPI Regional`,`Headcount Ratio Regional`,`Intensity of deprivation Regional`), by = list(`World region`), FUN = mode)
colnames(moda)[2:length(colnames(moda))] <- paste('Mode_',colnames(moda)[2:length(colnames(moda))])
mediana  <- aggregate( data.frame(`MPI National`,`MPI Regional`,`Headcount Ratio Regional`,`Intensity of deprivation Regional`), by = list(`World region`), FUN = median)
colnames(mediana)[2:length(colnames(mediana))] <- paste('Median_',colnames(mediana)[2:length(colnames(mediana))])

Agregados <-cbind(media,moda[2:length(moda)],mediana[2:length(mediana)])

# Medidas de dispersión

MPI= Medidas_Dispersión(Data$`MPI National`, Data$`World region`)
MPI_Reg = Medidas_Dispersión(Data$`MPI Regional`, Data$`World region`)
Hc_ratio = Medidas_Dispersión(Data$`Headcount Ratio Regional`,Data$`World region`)
Intensity = Medidas_Dispersión(Data$`Intensity of deprivation Regional`,Data$`World region`)

# Scatter Plot
unique(Data$`World region`)
pairs(Data[Numericos],  labels = names(Data[Numericos]))

# Correlación
Data= Data[complete.cases(Data),]
Correlacion= cor(Data[Numericos],use= 'complete.obs' )
corrplot(cor(Data[Numericos]))
heatmap(Data[,Numericos])
