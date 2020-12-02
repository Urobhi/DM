#Se utiliza el algoritmo Random Forest, creado por Leo Breiman en el año 2001
#Una libreria que implementa Rando Forest se llama  ranger
#La libreria esta implementada en lenguaje C y corre en paralelo, utiliza TODOS los nucleos del procesador

#limpio la memoria
rm( list=ls() )
gc()

require("data.table")
require("ranger")
require("randomForest")  #solo se usa para imputar nulos

#Aqui comienza el programa
setwd("M:" )  #establezco la carpeta donde voy a trabajar
#cargo el dataset
dataset  <- fread( "\\datasets\\paquete_premium_201906_202001.txt.gz", stringsAsFactors= TRUE)

#imputo los nulos, ya que ranger no acepta nulos
#Leo Breiman, ¿por que le temias a los nulos?
dataset  <-  na.roughfix( dataset )


#creo la clase_binaria
dataset[  , clase_binaria := as.factor(ifelse( clase_ternaria=="BAJA+2", "POS", "NEG" )) ]
dataset[  , clase_ternaria := NULL ]  #elimino la clase_ternaria, ya no la necesito


#genero el modelo de Random Forest con la libreria ranger
params  <- list( "num.trees"=      500,  #cantidad de arboles
                 "mtry"=             3,  #cantidad de variables que evalua para hacer un split
                 "min.node.size"=    1,  #hoja mas chica
                 "max.depth"=        0   # 0 significa profundidad infinita
               )

modelo  <- ranger( formula= "clase_binaria ~ .",
                   data= dataset[foto_mes<=201909, ],  #aqui considero los 6 meses de 201906 a 201912
                   probability=   TRUE,  #para que devuelva las probabilidades
                   num.trees=     params$num.trees,
                   mtry=          params$mtry,
                   min.node.size= params$min.node.size,
                   max.depth=     params$max.depth
                 )

prediccion_test  <- predict(  modelo, dataset[foto_mes==201911, ] )

ganancia_test <-  sum( (prediccion_test$predictions[ ,"POS" ]>0.025) 
                         * dataset[ foto_mes==201911, ifelse( clase_binaria=="POS",29250,-750)])

cat( "Ranger Basico, ganancia test =  ", ganancia_test, "\n" )
