#limpio la mmoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")
require("rpart")
require("rpart.plot")

setwd("/media/Shared/gustavo/cloud1/" )  #establezco la carpeta donde voy a trabajar
#cargo el dataset
dataset <- fread( "./datasets/paquete_premium_201906_202001.txt.gz")

#Aqui voy a ir guardando la mejor ganancia
mejor_params <- list(  "ganancia"= -1,
                       "maxdepth"= -1,
                       "minsplit"= -1,
                       "minbucket"= -1)


# GRID SEARCH
for( vmaxdepth  in c( 20, 18, 16, 14, 12, 10, 8, 6) )    
{
for(vminsplit in  c( 1000, 750, 500, 400, 300, 250, 200, 150, 100, 80, 50, 30, 20, 5))
{
porcentajes <-  c( 0.005, 0.01, 0.02, 0.03, 0.05, 0.10, 0.15, 0.20, 0.25, 0.30, 0.4 )

buckets_inteligentes <-  setdiff( unique( c( 1, 2, as.integer(vminsplit*porcentajes) )), 0 )

for(vminbucket in  buckets_inteligentes )
{

  #genero el modelo
  modelo   <-  rpart("clase_ternaria ~ .",  
                     data = dataset[foto_mes==201908,],  # Aqui entreno en UN SOLO MES
                     model= TRUE, #quiero que me devuelva el modelo
                     xval=0,
                     cp= 0,  #PRESTAR ATENCION A ESTO NUEVO
                     maxdepth=   vmaxdepth,
                     minsplit=   vminsplit,
                     minbucket=  vminbucket
                    )  #por ahora dejar xval=0

  prediccion_201910     <- predict( modelo, dataset[foto_mes==201910,], type = "prob")

  entrega <-   as.data.table(cbind( "numero_de_cliente" = dataset[ foto_mes==201910, numero_de_cliente],
                                    "prob" =prediccion_201910[, "BAJA+2"],
                                    "clase_ternaria" = dataset[ foto_mes==201910, clase_ternaria]
                                  ) 
                          )

  entrega[  ,  estimulo :=  as.integer( prob > 0.025)]

  ganancia <- sum(  entrega[ estimulo==1, ifelse( clase_ternaria=="BAJA+2", 29250, -750) ])

  if( ganancia > mejor_params$ganancia )
  {
    mejor_params$ganancia  <- ganancia
    mejor_params$maxdepth  <- vmaxdepth
    mejor_params$minsplit  <- vminsplit
    mejor_params$minbucket <- vminbucket
  }

  cat( "maxdepth=", vmaxdepth,
       "minsplit=", vminsplit,
       "minbucket=", vminbucket,
       "ganancia=",  ganancia, "\n", 
       file="./work/austral_01.txt", append="TRUE")

}
}
}


#Aqui los mejores parametros ya estan en la lista  mejor_params


#genero el modelo entrenando sobre 201910, ATENCION
modelo_final   <-  rpart ("clase_ternaria ~ .",  
                          data = dataset[foto_mes <= 201910,], 
                          model= TRUE, #quiero que me devuelva el modelo
                          xval=0,
                          cp= 0,  #PRESTAR ATENCION A ESTO NUEVO
                          maxdepth=   mejor_params$maxdepth,
                          minsplit=   mejor_params$minsplit,
                          minbucket=  mejor_params$minbucket
                         )  #por ahora dejar xval=0


#aplico el modelo a los datos sin clase, 202001
prediccion_202001      <- predict( modelo_final, dataset[foto_mes==202001,], type = "prob")

#genero el dataset de entrega
entrega <-   as.data.table(cbind( "numero_de_cliente" = dataset[ foto_mes==202001, numero_de_cliente],  "prob" =prediccion_202001[, "BAJA+2"]) )
entrega[  ,  estimulo :=  as.integer( prob > 0.025)]

#genero el archivo de salida
fwrite( entrega[ ,  c("numero_de_cliente", "estimulo"), with=FALSE], sep=",",  file="./work/rpart_basico_entrega_gridseach_1mes.csv")

#finalmente, se debe subir manualmente el archivo anterior a Kaggle