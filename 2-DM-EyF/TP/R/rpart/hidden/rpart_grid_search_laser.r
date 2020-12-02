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
                       "minbucket"= -1,
                       "cp"= -1)


# GRID SEARCH
for( vmaxdepth  in c( 16, 15, 14, 13, 12) )    
{
for(vminsplit in  c(  600, 580, 560, 540, 520, 500, 480, 460, 440, 420))
{
for(vminbucket in  c(1) )
{
for(vcp  in  c( 1e-7, 1e-6,  1e-5, 1e-4, 1e-3, 1e-2) )
{

  #genero el modelo
  modelo   <-  rpart("clase_ternaria ~ .",  
                     data = dataset[foto_mes<=201909,],  # Aqui entreno en CUATRO  MESES
                     model= TRUE, #quiero que me devuelva el modelo
                     xval=0,
                     cp= vcp,
                     maxdepth=   vmaxdepth,
                     minsplit=   vminsplit,
                     minbucket=  vminbucket
                    )  #por ahora dejar xval=0

  prediccion_201911     <- predict( modelo, dataset[foto_mes==201911,], type = "prob")

  entrega <-   as.data.table(cbind( "numero_de_cliente" = dataset[ foto_mes==201911, numero_de_cliente],
                                    "prob" =prediccion_201911[, "BAJA+2"],
                                    "clase_ternaria" = dataset[ foto_mes==201911, clase_ternaria]
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
    mejor_params$cp        <- vcp
  }

  cat( "maxdepth=",  vmaxdepth,
       "minsplit=",  vminsplit,
       "minbucket=", vminbucket,
       "cp=",        vcp,
       "ganancia=",  ganancia, "\n", 
       file="./work/salida_grid_search_laser.txt", append="TRUE")

}
}
}
}


#Aqui los mejores parametros ya estan en la lista  mejor_params


#genero el modelo entrenando sobre 201911, ATENCION
modelo_final   <-  rpart ("clase_ternaria ~ .",  
                          data = dataset[foto_mes <= 201911,], 
                          model= TRUE, #quiero que me devuelva el modelo
                          xval=0,
                          cp= mejor_params$cp,
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
fwrite( entrega[ ,  c("numero_de_cliente", "estimulo"), with=FALSE], sep=",",  file="./work/rpart_basico_entrega_gridseach_laser.csv")

#finalmente, se debe subir manualmente el archivo anterior a Kaggle
