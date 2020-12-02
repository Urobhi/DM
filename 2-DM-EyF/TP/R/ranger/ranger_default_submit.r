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
setwd("/media/Shared/gustavo/cloud1/" )  #establezco la carpeta donde voy a trabajar
#cargo el dataset
dataset  <- fread( "./datasetsOri_ITBA/transmutado/paquete_premium_201906_202001.txt.gz", stringsAsFactors= TRUE)

#imputo los nulos, ya que ranger no acepta nulos
#Leo Breiman, ¿por que le temias a los nulos?
dataset  <-  na.roughfix( dataset )


#creo la clase_binaria
dataset[  , clase_binaria := as.factor(ifelse( clase_ternaria=="BAJA+2", "POS", "NEG" )) ]
dataset[  , clase_ternaria := NULL ]  #elimino la clase_ternaria, ya no la necesito


#genero el modelo de Random Forest con la libreria ranger
params  <- list( "num.trees"=      500, #cantidad de arboles
                 "mtry"=             3, 
                 "min.node.size"=    1, # hoja mas chica
                 "max.depth"=        0  # 0 significa profundidad infinita
               )

modelo  <- ranger( formula= "clase_binaria ~ .",
                   data= dataset[foto_mes<=201911, ],  #aqui considero los 6 meses de 201906 a 201912
                   probability=   TRUE,  #para que devuelva las probabilidades
                   num.trees=     params$num.trees,
                   mtry=          params$mtry,
                   min.node.size= params$min.node.size,
                   max.depth=     params$max.depth
                 )

#aplico el modelo a los datos sin clase, 202001
prediccion_202001  <- predict(  modelo, dataset[foto_mes==202001, ] )

#genero el dataset de entrega
entrega  <- as.data.table(cbind( "numero_de_cliente"= dataset[ foto_mes==202001, numero_de_cliente],
                                 "prob"=  prediccion_202001$predictions[ ,"POS"] ) )

entrega[  ,  estimulo :=  as.integer( prob > 0.025)]

#genero el archivo de salida
fwrite( entrega[ ,  c("numero_de_cliente", "estimulo"), 
        with=FALSE], sep=",",  
        file="./work/ranger_default_6meses.csv")
