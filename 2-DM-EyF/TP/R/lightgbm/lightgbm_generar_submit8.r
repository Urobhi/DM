#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")
require("lightgbm")

setwd("~/buckets/b1")

#cargo el dataset donde voy a entrenar
dataset <- fread("./datasetsOri/paquete_premium.txt.gz")

#paso la clase a binaria que tome valores {0,1}  enteros
dataset[ , clase01 :=  ifelse( clase_ternaria=="BAJA+2", 1L, 0L)  ]

#los campos que se van a utilizar
campos_buenos  <- setdiff(  colnames(dataset) ,  c("clase_ternaria","clase01") )

dtrain <-  dataset[ foto_mes>=201904 & foto_mes<=201911 , ]  #lo aplico a 80 meses

#dejo los datos en el formato que necesita LightGBM
dgeneracion  <- lgb.Dataset( data= data.matrix(  dtrain[ , campos_buenos, with=FALSE]),
                             label= dtrain$clase01,
                             free_raw_data= FALSE )

optimo  <- list( num_iterations= 1265,
                 learning_rate=  0.01020333,
                 feature_fraction=  0.2333709,
                 min_gain_to_split= 0.003569764,
                 num_leaves=  417,
                 lambda_l1= 4.553854,
                 lambda_l2=  7.115316
               )

#genero el modelo
modelo  <- lgb.train( data= dgeneracion,
                      objective= "binary",
                      boost_from_average= TRUE,
                      max_bin= 31,
                      num_iterations=    optimo$num_iterations,
                      learning_rate=     optimo$learning_rate,
                      feature_fraction=  optimo$feature_fraction,
                      min_gain_to_split= optimo$min_gain_to_split,
                      num_leaves=        optimo$num_leaves,
                      lambda_l1=         optimo$lambda_l1,
                      lambda_l2=         optimo$lambda_l2
                    )

dapply <-  dataset[ foto_mes==202001 ]

#aplico el modelo a los datos sin clase, 202001
prediccion_202001  <- predict( modelo, 
                               data.matrix( dapply[, campos_buenos, with=FALSE ])                                 )

#genero el dataset de entrega
entrega <-   as.data.table(cbind( "numero_de_cliente"=dapply$numero_de_cliente,  "prob" =prediccion_202001) )
entrega[  ,  estimulo :=  as.integer( prob > 0.025)]

#genero el archivo de salida
fwrite( entrega[ ,  c("numero_de_cliente", "estimulo"), with=FALSE], sep=",",  file="./work/lightgbm_optimo8_entrega.csv")


