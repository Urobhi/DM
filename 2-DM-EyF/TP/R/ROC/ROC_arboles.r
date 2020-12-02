#limpio la mmoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")
require("rpart")
require("rpart.plot")
require("ROCR")

setwd("M:\\" )  #establezco la carpeta donde voy a trabajar
#cargo el dataset
dataset <- fread( ".\\datasets\\paquete_premium_201906_202001.txt.gz")

#creo la clase_binaria
dataset[  , clase_binaria := ifelse( clase_ternaria=="BAJA+2", 1L, 0L ) ]
dataset[  , clase_ternaria := NULL ]  #elimino la clase_ternaria, ya no la necesito

dtrain  <- dataset[ foto_mes== 201909, ]
dtest   <- dataset[ foto_mes== 201911, ]

#genero el modelo sobre los datos de TRAINING
modelo1  <- rpart("clase_binaria ~ .",
                  method= "class",
                  data = dtrain,
                  model= TRUE, #quiero que me devuelva el modelo
                  xval=0,
                  cp= 0,
                  maxdepth=   8,
                  minsplit=   30,
                  minbucket=   5
                 )  #por ahora dejar xval=0

#voy a dibujar la curva ROC en testing
prediccion1  <- predict( modelo1, dtest, type = "prob")
ganancia1    <- sum(  (prediccion1[,2]>0.025 )  *  ifelse( dtest$clase==1, 29250, -750 ) )


#imprimo la curva ROC y calculo la AUC
#El vector  prediccion1[,2]  tiene las probabilidades de ser positivo
pred1  <- prediction( prediccion1[,2], dtest$clase_binaria)

perf1  <- performance( pred1,"tpr", "fpr")
plot(perf1)
auc1  <- performance( pred1,"auc")@y.values[[1]]
auc1



#genero el segundo modelo
modelo2  <- rpart("clase_binaria ~ .",
                  method= "class",
                  data = dtrain, 
                  model= TRUE, #quiero que me devuelva el modelo
                  xval=0,
                  cp= 0,
                  maxdepth=   22,
                  minsplit=  100,
                  minbucket=   1
                 )  #por ahora dejar xval=0

#voy a dibujar la curva ROC en testing
prediccion2  <- predict( modelo2, dtest, type = "prob")
ganancia2   <- sum(  (prediccion2[,2]>0.025 )  *  ifelse( dtest$clase==1, 29250, -750 ) )

#imprimo la curva ROC y calculo la AUC
#El vector  prediccion1[,2]  tiene las probabilidades de ser positivo
pred2  <- prediction( prediccion2[,2], dtest$clase_binaria)

perf2  <- performance( pred2,"tpr", "fpr")
plot( perf2 )
auc2  <- performance( pred2,"auc")@y.values[[1]]
auc2


#Que caso mas interesante, la curva de mayor ROC tiene la menor ganancia
cat( "Curva1  ganancia=", ganancia1, "AUC=", auc1 , "\n")
cat( "Curva2  ganancia=", ganancia2, "AUC=", auc2 , "\n")


#Ahora los imprimo en el mismo grafico
plot( perf1, col="blue", lwd=3)
plot( perf2, col="red",  lwd=3, add=TRUE)

#dibujo la recta  ganancia1
POS  <- sum( dtest$clase_binaria )
NEG  <- sum( 1-dtest$clase_binaria )

slope <- (750/29250)*POS
intercept <-  (ganancia1/29250)/POS

abline( intercept, slope, col="darkgreen", lwd=4 )

