library(readr)
library(corrplot)
library(sqldf)
Parent_folder <- dirname(dirname(rstudioapi::getSourceEditorContext()$path))
library(ggplot2)
library(data.table)
library(infotheo)

source(paste(Parent_folder,"/Funciones.R", sep= ""))


Ruidoso <- read_csv("https://raw.githubusercontent.com/dmuba/dmuba.github.io/master/Practicos/LAB02/ruidoso.txt")

# relevo de características y relaciones
str(Ruidoso)
summary(Ruidoso)

plot(Ruidoso$Road_55dB)
plot(Ruidoso$Road_60dB)
plot(Ruidoso$Railways_65dB)
plot(Ruidoso$Industry_65dB)
boxplot((scale((Ruidoso[,-1]),center = TRUE, scale = TRUE)),xlab = "Dataset", ylab = "Valor normalizado", main = "Boxplot de exploración", las=2)

# Correlación

Correlacion= cor(Ruidoso[,-1],use= 'complete.obs' )
corrplot(Correlacion)

Var = Ruidoso$Road_60dB
boxplot(Var)$stats

c(mean(Var),min(Var),max(Var))

Detect_outliers(Var)

Var_filtrada = Filter_outliers(Var)
boxplot(Var_filtrada)
plot(Var_filtrada)
