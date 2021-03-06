library(dplyr)
library(cluster)
library(pracma)
library(caret)
library(MASS)
library(ggplot2)
library(lattice)
library(grid)
library(DMwR)
library(readxl)
library(reshape)
library(GGally)
library(magrittr)
library(tibble)
library(dendextend)
library(cluster)
library(factoextra)
library(ca)
library(FactoMineR)
library(nortest)
library(e1071)
library(pracma)
library(NbClust)
GrandParent_folder <- dirname(dirname(rstudioapi::getSourceEditorContext()$path))
Parent_folder <- dirname(rstudioapi::getSourceEditorContext()$path)
#source(paste(dirname(GrandParent_folder),"/Funciones.R", sep= ""))
source(paste(dirname(Parent_folder),"/ggbiplot.r", sep= ""))
source(paste(dirname(Parent_folder),"/ggscreeplot.r", sep= ""))

sampleo <-function(dataset,split)
  {
    dni = 36637757
    n = round(split* nrow(dataset))
    set.seed(dni)
    mask = sample(1:nrow(dataset),size=n,replace=FALSE)
    dataset = dataset[mask,]
    return(dataset)
}

train_test <-function(dataset,split)
{
  dni = 36637757
  n = round(split* nrow(dataset))
  set.seed(dni)
  mask = sample(1:nrow(dataset),size=n,replace=FALSE)
  dataset_train = dataset[mask,]
  dataset_test = dataset[-mask,]
  return(list(dataset_train,dataset_test))
}

prostata_DataTypes <- function(dataset)
  {
    # elimino el id
    dataset = dataset[,-8]
    dataset = dataset[,-1] 
    # Seteo como factores
    dataset$CAPSULE = as.factor(dataset$CAPSULE)
    dataset$RACE = as.factor(dataset$RACE)
    dataset$DPROS = as.factor(dataset$DPROS)
    dataset$DCAPS = as.factor(dataset$DCAPS)
    
    # Seteo como numeros
    dataset$PSA = as.numeric(dataset$PSA)
    
    # Elimino filas con NA en gleason dado que hay errores en otros datos
    dataset = dataset[!is.na(dataset$GLEASON),]
    return(dataset)
}

seguros_Datatypes <- function(dataset)
{
  dataset$edad = as.numeric(dataset$edad)
  dataset$BMI= as.numeric(dataset$BMI)
  dataset$hijos= as.numeric(dataset$hijos)
  dataset$fuma= as.numeric(dataset$fuma)
  dataset$cargos= as.numeric(dataset$cargos)
  dataset$primadelseguro= as.numeric(dataset$primadelseguro)
  
  dataset$sexo = as.factor(dataset$sexo)
  dataset$region = as.factor(dataset$region)
  
  return(dataset)
  
}


determinark = function(datA_esc,kmax,f,metrica) {
  
  sil = array()
  #sil_2 = array()
  sse = array()
  bet = array()
  
  datA_dist= dist(datA_esc,method = "euclidean", diag = FALSE, upper = FALSE, p = 2)
  for ( i in  2:kmax) {
    if (strcmp(f,"kmeans")==TRUE) {   
      CL  = kmeans(datA_esc,centers=i,nstart=50,iter.max = kmax)
      sse[i]  = CL$tot.withinss 
      bet[i] = CL$betweenss
      CL_sil = silhouette(CL$cluster, datA_dist)
      sil[i]  = summary(CL_sil)$avg.width
    }
    if (strcmp(f,"pam")==TRUE){       
      CL = pam(x=datA_esc, k=i, diss = F, metric = metrica)
      sse[i]  = CL$objective[1] 
      sil[i]  = CL$silinfo$avg.width
    }
  }
  sse
  sil
  return(data.frame(sse,sil,bet))
}


##### Levanto datos y sampleo ######
prostata = read.csv(file = paste(dirname(Parent_folder),"/AID-Datasets/Prostata.csv",sep=""), dec = ",")
prostata = sampleo(prostata,0.8)
prostata <- DataTypes(prostata) # Corrijo tipos de datos, elimino nulos,etc

nums <- unlist(lapply(prostata, is.numeric))
prostata_nums = prostata[,nums]
prostata_factor = prostata[,!nums]

cor(prostata_nums)

#### Visualizacion de algunos datos ####

par(mfcol = c(1,length(prostata_nums)))
for (k in 1:length(prostata_nums))
  {
    hist(prostata_nums[,k],proba = T,main = names(prostata_nums[,k]),10)
    x0 <- seq(min(prostata_nums[, k]), max(prostata_nums[, k]), le = 50) 
    lines(x0, dnorm(x0, mean(prostata_nums[,k]), sd(prostata_nums[,k])), col = "red", lwd = 2) 
    grid()
}

pval = list() 
par(mfcol = c(1,length(prostata_nums)))
for (k in 1:length(prostata_nums)){
  qqnorm(prostata_nums[,k],main = names(prostata_nums[k]))
  qqline(prostata_nums[,k],col="red") 
  pval[k] = ad.test(prostata_nums[,k])$p.value
  grid()
}
pval # Rechazo normalidad por en todas



ggplot(data = prostata, mapping = aes(x = CAPSULE, y = PSA, colour = CAPSULE)) + 
  geom_boxplot() + theme_bw() + theme(legend.position =  "none")
ggplot(data = prostata, mapping = aes(x = CAPSULE, y = VOL, colour = CAPSULE)) + 
  geom_boxplot() + theme_bw() + theme(legend.position = "none")
ggplot(data = prostata, mapping = aes(x = CAPSULE, y = GLEASON, colour = CAPSULE)) + 
  geom_boxplot() + theme_bw() + theme(legend.position = "none")
ggplot(data = prostata, mapping = aes(x = CAPSULE, y = AGE, colour = CAPSULE)) + 
  geom_boxplot() + theme_bw() + theme(legend.position = "none")

xtabs(~CAPSULE+RACE, data=prostata)
xtabs(~CAPSULE+DPROS, data=prostata)  # Examen prostatico ( 1 no hay, 2 unico izq, 3 unico der ,4  ambos)
xtabs(~CAPSULE+DCAPS, data=prostata)  # deteccion de envoltura ( 1 si 2 no)


##### Clasificacion Supervisada ######




##### Separaci�n en train y test ######
prostata.train_test = train_test(prostata,split = 0.8)


##### Modelo 1 ####
prostata_glm = glm(data = prostata.train_test[[1]], CAPSULE~., family = "binomial" )
prostata_stepwise_glm <- prostata_glm %>%  stepAIC(trace=FALSE)
summary(prostata_glm)
summary(prostata_stepwise_glm)

prostata.pred_test<-predict(prostata_glm,newdata=prostata.train_test[[2]], type="response") 
prostata.pred_test = ifelse(test=prostata.pred_test > 0.5,yes=1,no=0)
confusion_matrix=confusionMatrix(prostata.train_test[[2]]$CAPSULE,as.factor(prostata.pred_test))
confusion_matrix

# No deseable tener falsos negativos
prostata.pred_test<-predict(prostata_glm,newdata=prostata.train_test[[2]], type="response") 
prostata.pred_test = ifelse(test=prostata.pred_test > 0.25,yes=1, no=0)
confusion_matrix=confusionMatrix(as.factor(prostata.pred_test),prostata.train_test[[2]]$CAPSULE)
table(prostata.train_test[[2]]$CAPSULE,prostata.pred_test)
confusion_matrix

prostata.pred_test<-predict(prostata_stepwise_glm,newdata=prostata.train_test[[2]], type="response") 
prostata.pred_test = ifelse(test=prostata.pred_test > 0.25,yes=1,no=0)
confusion_matrix=confusionMatrix(as.factor(prostata.pred_test),prostata.train_test[[2]]$CAPSULE)
confusion_matrix


##### Modelo 2 #####

prostata_svm=svm(CAPSULE~ .,
              data=prostata.train_test[[1]],
              method="C-classification",
              kernel="radial",
              cost=1,
              gamma=1/nrow(prostata.train_test[[1]]))
prostata.train_test[[2]]$svmpred=predict(prostata_svm, prostata.train_test[[2]])
confusion_matrix=confusionMatrix(as.factor(prostata.pred_test),prostata.train_test[[2]]$CAPSULE)
confusion_matrix


# PCA para vaiables
prostata.numeric= prostata[,nums]
prost.PCA = prcomp(prost,center = TRUE, scale. = TRUE)

ggscreeplot(prost.PCA, type = c('pev','cev')) +
  xlab('Componente principal') +
  ylab('Proporcion de la variabilidad explicada')+
  geom_line(colour='royalblue')+
  geom_point(colour='royalblue')


ggbiplot(prost.PCA,obs.scale = 1,var.scale = 1,alpha = 0.5, groups = interaction(prostata$CAPSULE))



# Clusters para variables num�ricas 
prostata.kmeans = kmeans(scale(prostata[,nums]),2)
prostata$kmeansclust = as.factor(prostata.kmeans$cluster)

ggbiplot(prost.PCA,obs.scale = 1,var.scale = 1,alpha = 0.5, groups = interaction(prostata$CAPSULE))
ggbiplot(prost.PCA,obs.scale = 1,var.scale = 1,alpha = 0.5, groups = interaction(prostata$kmeansclust))

#Clusters para analisis de correspondencias utilizando los clusters
catprost = prostata[,!nums] 
catprost.AC = MCA(catprost, graph=F)
fviz_mca_var(catprost.AC,repel=TRUE,col.var="royalblue")+  theme_gray()












##### Seguros ####
seguros = read.csv(file = paste(dirname(Parent_folder),"/AID-Datasets/Seguros.csv",sep=""), dec = ",")
seguros = sampleo(seguros,0.75)
seguros = seguros_Datatypes(seguros)
seguros = seguros[seguros$hijos < 300 & seguros$primadelseguro < 40000,]

nums <- unlist(lapply(seguros, is.numeric))
seguros.numeric = seguros[,nums]
seguros.factor = seguros[,!nums]


###### PLOTS ####
boxplot(scale(seguros.numeric))

ggplot(data = seguros, mapping = aes( y = edad)) + 
  geom_boxplot() + theme_bw() + theme(legend.position = "none")
ggplot(data = seguros, mapping = aes( y = BMI)) + 
  geom_boxplot() + theme_bw() + theme(legend.position = "none")
ggplot(data = seguros, mapping = aes( y = hijos)) + 
  geom_boxplot() + theme_bw() + theme(legend.position = "none")
ggplot(data = seguros, mapping = aes( y = fuma)) + 
  geom_boxplot() + theme_bw() + theme(legend.position = "none")
ggplot(data = seguros, mapping = aes( y = cargos)) + 
  geom_boxplot() + theme_bw() + theme(legend.position = "none")
ggplot(data = seguros, mapping = aes( y = primadelseguro)) + 
  geom_boxplot() + theme_bw() + theme(legend.position = "none")

ggplot(data = seguros, mapping = aes(x = region, y = edad, colour = region)) + 
  geom_boxplot() + theme_bw() + theme(legend.position = "none")
ggplot(data = seguros, mapping = aes(x = sexo, y = edad, colour = sexo)) + 
  geom_boxplot() + theme_bw() + theme(legend.position = "none")

ggplot(data = seguros, mapping = aes(x = sexo, y = BMI, colour = sexo)) + 
  geom_boxplot() + theme_bw() + theme(legend.position = "none")
ggplot(data = seguros, mapping = aes(x = region, y = BMI, colour = region)) + 
  geom_boxplot() + theme_bw() + theme(legend.position = "none")

ggplot(data = seguros, mapping = aes(x = sexo, y = hijos, colour = sexo)) + 
  geom_boxplot() + theme_bw() + theme(legend.position = "none")
ggplot(data = seguros, mapping = aes(x = region, y = hijos, colour = region)) + 
  geom_boxplot() + theme_bw() + theme(legend.position = "none")

ggplot(data = seguros, mapping = aes(x = sexo, y = fuma, colour = sexo)) + 
  geom_boxplot() + theme_bw() + theme(legend.position = "none")
ggplot(data = seguros, mapping = aes(x = region, y = fuma, colour = region)) + 
  geom_boxplot() + theme_bw() + theme(legend.position = "none")

ggplot(data = seguros, mapping = aes(x = sexo, y = cargos, colour = sexo)) + 
  geom_boxplot() + theme_bw() + theme(legend.position = "none")
ggplot(data = seguros, mapping = aes(x = region, y = cargos, colour = region)) + 
  geom_boxplot() + theme_bw() + theme(legend.position = "none")

ggplot(data = seguros, mapping = aes(x = sexo, y = primadelseguro, colour = sexo)) + 
  geom_boxplot() + theme_bw() + theme(legend.position = "none")
ggplot(data = seguros, mapping = aes(x = region, y = primadelseguro, colour = region)) + 
  geom_boxplot() + theme_bw() + theme(legend.position = "none")


round(cor(seguros.numeric),2)



##### No supervisada #####
seguros.pca = prcomp(seguros.numeric,scale = TRUE)

kmax = 7
m1   = determinark(scale(seguros.numeric),kmax,"pam",metrica = "manhattan")               #tipica de la normal

par(mfrow=c(3,1))
plot(2:kmax, m1$sil[2:kmax],col=1,type="b", pch = 19, frame = FALSE, 
     xlab="Numero de clusters",
     ylab="sil") 

plot(2:kmax, m1$sse[2:kmax],type="b", pch = 19, frame = FALSE, 
     xlab="Numero de clusters",
     ylab="SS within") 

plot(2:kmax, m1$bet[2:kmax],type="b", pch = 19, frame = FALSE, 
     xlab="Numero de clusters",
     ylab="SS between") 

par(mfrow=c(1,1))


fviz_nbclust(scale(seguros.numeric),hcut,method = c("silhouette"),nboot = 50 ,diss = dist(scale(seguros.numeric),method = "euclidean") ) # Elegimos 5 clusters por el critero de maximizar mean silhouette
a =fviz_nbclust(scale(seguros.numeric),hcut,method = c("silhouette"),nboot = 50 ,diss = dist(scale(seguros.numeric),method = "euclidean") ) # Elegimos 5 clusters por el critero de maximizar mean silhouette
a$data$y


# Matriz de distancias eucl�deas 
mat_dist <- dist(x = scale(seguros.numeric), method = "euclidean") 

# Dendrogramas (seg�n el tipo de segmentaci�n jer�rquica aplicada)  
hc_complete <- hclust(d = mat_dist, method = "complete") 
hc_average  <- hclust(d = mat_dist, method = "average")
hc_single   <- hclust(d = mat_dist, method = "single")
hc_ward     <- hclust(d = mat_dist, method = "ward.D2")
#calculo del coeficiente de correlacion cofenetico
cor(x = mat_dist, cophenetic(hc_complete))
cor(x = mat_dist, cophenetic(hc_average))
cor(x = mat_dist, cophenetic(hc_single))
cor(x = mat_dist, cophenetic(hc_ward))

plot(hc_complete )#no se ve bien si hay muchos datos
plot(hc_average )#no se ve bien si hay muchos datos
plot(hc_single )#no se ve bien si hay muchos datos
plot(hc_ward )#no se ve bien si hay muchos datos
rect.hclust(hc_complete, k=5, border="red")#con 3 grupos
seguros$hcluster<-cutree(hc_ward,k=5)#con 3 grupos

ggbiplot(seguros.pca, obs.scale=1 ,var.scale=1, alpha=0.5,groups = as.factor(seguros$hcluster) )


seguros.kmeans = kmeans(scale(seguros.numeric),nstart=50,6)
kmeans(scale(seguros.numeric),nstart=50,10)
seguros$kmeansclust = as.factor(seguros.kmeans$cluster)

ggbiplot(seguros.pca,obs.scale = 1,var.scale = 1,alpha = 0.5, groups = interaction(seguros$kmeansclust))
ggbiplot(seguros.pca,obs.scale = 1,var.scale = 1,alpha = 0.5, groups = interaction(seguros$region))

seguros.numeric %>% mutate(kmeansclust = seguros.kmeans$cluster) %>%  mutate(region = as.numeric(seguros$region) -1) %>%  mutate(sexo = as.numeric(seguros$sexo)-1)%>% 
  group_by(kmeansclust) %>%
  summarise_all(mean) 

