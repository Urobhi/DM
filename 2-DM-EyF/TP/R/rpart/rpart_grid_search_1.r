#limpio la mmoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")
require("rpart")
require("rpart.plot")

setwd("M:" )  #establezco la carpeta donde voy a trabajar
#cargo el dataset
dataset <- fread( "\\datasets\\paquete_premium_201906_202001.txt.gz")

# GRID SEARCH
for( vmaxdepth  in c( 12, 11, 10, 9, 8, 7, 6, 5) )    
{
for(vminsplit in  c( 5, 10, 15, 20, 30, 40, 50, 60))
{
porcentajes <-  c( 0.01, 0.02, 0.03, 0.05, 0.075, 0.1, 0.15, 0.20, 0.25, 0.30, 0.40 )

buckets_inteligentes <-  setdiff( unique( as.integer(vminsplit*porcentajes) ), 0 )

for(vminbucket in  buckets_inteligentes )
{

  #genero el modelo
  modelo   <-  rpart("clase_ternaria ~ .",  
                     data = dataset[foto_mes==201909,], 
                     model= TRUE, #quiero que me devuelva el modelo
                     xval=0,
                     cp= 0,  #PRESTAR ATENCION A ESTO NUEVO
                     maxdepth=  vmaxdepth,
                     minsplit=  vminsplit,
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
    
  cat( "maxdepth=", vmaxdepth,
       "minsplit=", vminsplit,
       "minbucket=", vminbucket,
       "ganancia=",  ganancia, "\n", 
       file=".\\work\\salida_grid_search_1.txt", append="TRUE")

}
}
}



#Aqui ya supongo que tengo los parametros ganadores
#deben ser cargados A MANO  a partir del archivo  salida_grid_search_1.txt

maxdepth_optimo <- 
minsplit_optimo <- 
minbucket_optimo <-



#genero el modelo entrenando sobre 201911, ATENCION
modelo_final   <-  rpart ("clase_ternaria ~ .",  
                          data = dataset[foto_mes <= 201911,], 
                          model= TRUE, #quiero que me devuelva el modelo
                          xval=0,
                          cp= 0,  #PRESTAR ATENCION A ESTO NUEVO
                          maxdepth=  maxdepth_optimo,
                          minsplit=  minsplit_optimo,
                          minbucket=  minbucket_optimo
                         )  #por ahora dejar xval=0


#aplico el modelo a los datos sin clase, 202001
prediccion_202001      <- predict( modelo_final, dataset[foto_mes==202001,], type = "prob")

#genero el dataset de entrega
entrega <-   as.data.table(cbind( "numero_de_cliente" = dataset[ foto_mes==202001, numero_de_cliente],  "prob" =prediccion_202001[, "BAJA+2"]) )
entrega[  ,  estimulo :=  as.integer( prob > 0.025)]

#genero el archivo de salida
fwrite( entrega[ ,  c("numero_de_cliente", "estimulo"), with=FALSE], sep=",",  file="rpart_basico_entrega_gridseach_1.csv")

#finalmente, se debe subir manualmente el archivo anterior a Kaggle
