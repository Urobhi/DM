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
GrandParent_folder <- dirname(dirname(rstudioapi::getSourceEditorContext()$path))
Parent_folder <- dirname(rstudioapi::getSourceEditorContext()$path)
source(paste(dirname(Parent_folder),"/ggbiplot.r", sep= ""))
source(paste(dirname(Parent_folder),"/ggscreeplot.r", sep= ""))



##### Guia 5 ########

A=data.frame(matrix ( c (4,2,3,2,4,3,7,4,25,10,12,4,18,24,33,13,10,6,7,2) , nrow=5 , ncol =4 , byrow=T))
colnames(A) <- c("No fuma","Poco","Moderado","Mucho")
rownames(A) <- c("Gerente Senior","Gerente Junior", "Empleado Senior", "Empleado Junior", "Secretaria")

A.condicionales <- t(apply(A, 1, function(i) i/sum(i)))

A.ac = CA(A,graph=TRUE)
get_ca_row(A.ac)
get_ca_col(A.ac)
fviz_contrib(A.ac,choice="row", axes=1, fill="royalblue", color = "black")
fviz_contrib(A.ac,choice="col", axes=1, fill="royalblue", color = "black")

fviz_ca_row(A.ac,repel=TRUE,col.row="royalblue")
A.ac$row

 chisq.test(A)$statistic / sum(colSums(A)) 
A.ac$eig

#####2
autos <- read_excel(path =  paste(dirname(Parent_folder),"/AID-Datasets/autos.xlsx",sep=""))
autos[,] <- lapply(autos, factor )
autos <- autos[,-1]
autos <- drop_na(autos)
colnames(autos)[6] <- "tamano"


autos.disyuntiva <- acm.disjonctif(data.table::data.table(autos))
autos.burt <- (data.table(autos))

autos.dim <- autos[,c('Origen','Tipo','tamano')]
autos.AC = MCA(autos.dim,graph = F, quali.sup=1)
fviz_mca_var(autos.AC,repel=TRUE,col.var="royalblue")+  theme_gray()
fviz_contrib(autos.AC,choice="var")


#####3
ingleses <- read_excel(path =  paste(dirname(Parent_folder),"/AID-Datasets/ingleses.xlsx",sep=""))
ingleses.perfilfila <- cbind(ingleses[,1], t(apply(ingleses[,-1], 1, function(i) i*100/sum(i))))


#####4

proteinas <- read_excel(path =  paste(dirname(Parent_folder),"/AID-Datasets/proteinas.xlsx",sep=""))
paises = proteinas$País
proteinas[,] <- lapply(proteinas, function(x) as.numeric(gsub(",", ".", x)))
proteinas <- proteinas[,-1]
rownames(proteinas) <- paises
proteinas.AC = CA(proteinas,graph=TRUE)
                             
                             
                             
                             


