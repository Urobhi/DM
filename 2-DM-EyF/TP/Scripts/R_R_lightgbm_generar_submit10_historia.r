#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")
require("lightgbm")

setwd("~/buckets/b1")

#cargo el dataset donde voy a entrenar
dataset <- fread("./datasets/paquete_premium_exthist_corregido_lag2_diff_todo_cpp.txt.gz")  #archivo CON HISTORIA

#paso la clase a binaria que tome valores {0,1}  enteros
dataset[ , clase01 :=  ifelse( clase_ternaria=="BAJA+2", 1L, 0L)  ]

#los campos que se van a utilizar
campos_buenos  <- setdiff(  colnames(dataset) ,  c("clase_ternaria","clase01") )

dtrain <-  dataset[ foto_mes>=201902 & foto_mes<=201911 , ]  #lo aplico a 10 meses

#dejo los datos en el formato que necesita LightGBM
dgeneracion  <- lgb.Dataset( data= data.matrix(  dtrain[ , campos_buenos, with=FALSE]),
                             label= dtrain$clase01,
                             free_raw_data= FALSE )

optimo  <- list( num_iterations= 750,
                 nrounds =451,
                 learning_rate=  0.0065734,
                 feature_fraction=  0.3466282,
                 min_gain_to_split= 2.292883,
                 num_leaves=  277,
                 lambda_l1= 1.827623,
                 lambda_l2=  0.1861661,
                 prob_corte = 0.02182582)

#genero el modelo
modelo  <- lgb.train( data= dgeneracion,
                      objective= "binary",
                      boost_from_average= TRUE,
                      max_bin= 31,
                      verbosity= -1,
                      verbose= -1,
                      num_iterations=    optimo$num_iterations,
                      nrounds=           optimo$nrounds,
                      learning_rate=     optimo$learning_rate,
                      feature_fraction=  optimo$feature_fraction,
                      min_gain_to_split= optimo$min_gain_to_split,
                      num_leaves=        optimo$num_leaves,
                      lambda_l1=         optimo$lambda_l1,
                      lambda_l2=         optimo$lambda_l2)

dapply <-  dataset[ foto_mes==202001 ]

#aplico el modelo a los datos sin clase, 202001
prediccion_202001  <- predict( modelo, 
                               data.matrix( dapply[, campos_buenos, with=FALSE ])                                 )

proba_array = c(0.02392582,0.02492582,0.02592582,0.02692582,0.02792582,0.02892582,0.02992582,0.03092582,0.03192582,0.03292582,0.03492582,0.03392582,0.03592582,0.03792582,0.03992582,0.04192582,0.04392582,0.04592582,0.048592582)

#genero el dataset de entrega
for (proba in proba_array)
{
  print(paste0("./work/lightgbm_optimo10_historia_entrega_lags2_todo_cpp",proba,".csv",sep=""))
  entrega <-   as.data.table(cbind( "numero_de_cliente"=dapply$numero_de_cliente,  "prob" =prediccion_202001) )
  entrega[  ,  estimulo :=  as.integer( prob > proba)]
  #genero el archivo de salida
  fwrite( entrega[ ,  c("numero_de_cliente", "estimulo"), with=FALSE], sep=",",  file= paste0("./work/lightgbm_optimo10_historia_entrega_lags_newhyper_cpp",proba,".csv",sep=""))
  
}
