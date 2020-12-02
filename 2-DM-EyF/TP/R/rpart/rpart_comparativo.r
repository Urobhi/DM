#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")
require("rpart")

setwd("M:" )  #establezco la carpeta donde voy a trabajar
#cargo el dataset
dataset <- fread( "\\datasets\\paquete_premium_201906_202001.txt.gz")


for( vmaxdepth  in c(3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25) )    
{

  #genero el modelo
  modelo   <-  rpart("clase_ternaria ~ .",  
                     data = dataset[foto_mes==201909,], 
                     model= TRUE, #quiero que me devuelva el modelo
                     xval= 0,
                     cp= 0,
                     minsplit= 5,
                     maxdepth=  vmaxdepth
                    )  #por ahora dejar xval=0

  prediccion_201909     <- predict( modelo, dataset[foto_mes==201909,], type = "prob")
  prediccion_201911     <- predict( modelo, dataset[foto_mes==201911,], type = "prob")

  entrega_201909 <-   as.data.table(cbind( "numero_de_cliente" = dataset[ foto_mes==201909, numero_de_cliente],
                                           "prob" =prediccion_201909[, "BAJA+2"],
                                           "clase_ternaria" = dataset[ foto_mes==201909, clase_ternaria] ))

  entrega_201911 <-   as.data.table(cbind( "numero_de_cliente" = dataset[ foto_mes==201911, numero_de_cliente],
                                           "prob" =prediccion_201911[, "BAJA+2"],
                                           "clase_ternaria" = dataset[ foto_mes==201911, clase_ternaria] ))

  entrega_201909[  ,  estimulo :=  as.integer( prob > 0.025)]
  entrega_201911[  ,  estimulo :=  as.integer( prob > 0.025)]

  ganancia_201909 <- sum(  entrega_201909[ estimulo==1, ifelse( clase_ternaria=="BAJA+2", 29250, -750) ])
  ganancia_201911 <- sum(  entrega_201911[ estimulo==1, ifelse( clase_ternaria=="BAJA+2", 29250, -750) ])
    
  cat( "maxdepth=", vmaxdepth,
       "ganancia_201909=",  ganancia_201909, 
       "ganancia_201911=",  ganancia_201911, "\n" )

}
