#necesita de 128GB de memoria RAM
#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")
require("lightgbm")

#cargo el dataset donde voy a entrenar
dataset <- fread("~/buckets/b2/datasetsOri/paquete_premium_completo.txt.gz")

#creo los campos lags "el valor del mes anterior"
setorderv( dataset, c("numero_de_cliente","foto_mes") ) #ordeno
campos_lags  <- setdiff(  colnames(dataset) ,  c("clase_ternaria", "numero_de_cliente") )
dataset[,  paste0( campos_lags, "_lag1") :=shift(.SD, 1, NA, "lag"), by=numero_de_cliente, .SDcols= campos_lags]
dataset[,  paste0( campos_lags, "_lag2") :=shift(.SD, 2, NA, "lag"), by=numero_de_cliente, .SDcols= campos_lags]

for( vcol in campos_lags )
{
   dataset[,  paste0(vcol, "_delta1") := get( vcol)  - get(paste0( vcol, "_lag1")) ]
   dataset[,  paste0(vcol, "_delta2") := get( vcol)  - get(paste0( vcol, "_lag2")) ]
   dataset[,  paste0(vcol, "_delta3") := get(paste0( vcol, "_lag1"))  - get(paste0( vcol, "_lag2")) ]
}


#paso la clase a binaria que tome valores {0,1}  enteros
dataset[ , clase01 :=  ifelse( clase_ternaria=="BAJA+2", 1L, 0L)  ]

#TODOS los datos van a parar a la olla, 35 meses 
#No corrijo los valores dañados, el calor matara todos los bichos
dataset[ foto_mes<=201911, train:= 1L ]

#los campos que se van a utilizar
campos_buenos  <- setdiff(  colnames(dataset) ,  c("clase_ternaria","clase01","train") )

#dejo los datos en el formato que necesita LightGBM
dgeneracion  <- lgb.Dataset( data= data.matrix( dataset[ train==1, campos_buenos, with=FALSE]),
                             label= dataset[ train==1, clase01],
                             free_raw_data= FALSE )

#Estos valores optimos provienen de una Optimizacion Bayesiana
optimo  <- list( "min_data_in_leaf" =   ,#Don't be greedy
                 "num_iterations"   =   ,#More than enough help has been given !
                 "pcorte" =              )  #atencion, NO es 0.025

#genero el modelo usando solo dos hiperparametros
modelo  <- lgb.train(data= dgeneracion,
                     objective= "binary",
                     min_data_in_leaf=  optimo$min_data_in_leaf,
                     num_iterations=    optimo$num_iterations  )

#finalmente, aplico el modelo a los datos de enero-2020
prediccion_202001  <- predict( modelo,  data.matrix( dataset[ foto_mes==202001 , campos_buenos, with=FALSE]))

#genero el dataset de entrega probabilidad de corte 
entrega  <- as.data.table( list( "numero_de_cliente"=  dataset[ foto_mes==202001, numero_de_cliente],  
                                 "estimulo"=           (prediccion_202001> optimo$pcorte)  ) )

#genero el archivo de salida
fwrite( entrega, logical01=TRUE, sep=",",  file= "~/buckets/b2/work/simple_lag_is_beautiful_EPIC.csv" )
