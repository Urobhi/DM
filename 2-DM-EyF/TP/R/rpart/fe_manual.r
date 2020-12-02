#Feature Engineering Manual

#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")
require("rpart")



#Aqui comienza el programa
setwd("M:" )  #establezco la carpeta donde voy a trabajar
#cargo el dataset
dataset <- fread( "\\datasets\\paquete_premium_201906_202001.txt.gz")

#creo la clase_binaria
dataset[  , clase_binaria := ifelse( clase_ternaria=="BAJA+2", "POS", "NEG" ) ]
dataset[  , clase_ternaria := NULL ]  #elimino la clase_ternaria, ya no la necesito

#genero los datasets de training y testing
dtrain <-  dataset[  foto_mes==201909, ]
dtest  <-  dataset[  foto_mes==201911, ]

#genero modelo sobre training
modelo1 <-  rpart("clase_binaria ~ . ",
                   data= dtrain,
                   xval= 0, 
                   cp=         0.0002024526,
                   maxdepth=  17,
                   minsplit=  29,
                   minbucket=  4
                  )

#calculo la ganancia en los datos de testing
prediccion1  <- predict( modelo1, dtest, type = "prob")[,"POS"]
ganancia1    <- sum( (prediccion1> 0.025) * 
                     dtest[, ifelse( clase_binaria=="POS",29250,-750)])

cat( "ganancia1=",  ganancia1, "\n" )

#--------------------------------------
#Ahora  viene  el Feature Engineering
dataset[  , VM_status1 :=  ifelse( is.na(Visa_status), 90, Visa_status*10 ) +  
                           ifelse( is.na(Master_status),  9, Master_status )  ]

dataset[  , VM_status2 :=  ifelse( is.na(Visa_status), 9, Visa_status) ]
dataset[  , VM_status3 :=  ifelse( is.na(Master_status), 9, Master_status) ]


dataset[  ,  .N, c( "Visa_status", "Master_status", "VM_status1" ) ]

#vuelvo a generar  dtrain y dtest, ahora con el nuevo campo
dtrain <-  dataset[  foto_mes==201909, ]
dtest  <-  dataset[  foto_mes==201911, ]


#genero el modelo2
modelo2 <-  rpart("clase_binaria ~ . ",
                   data= dtrain,
                   xval= 0, 
                   cp=         0.0,
                   maxdepth=  17,
                   minsplit=  29,
                   minbucket=  4
                  )

#calculo la ganancia en los datos de testing
prediccion2  <- predict( modelo2, dtest, type = "prob")[,"POS"]
ganancia2    <- sum( (prediccion2> 0.025) * 
                     dtest[, ifelse( clase_binaria=="POS",29250,-750)])

cat( "ganancia1=",  ganancia1, "ganancia2=",  ganancia2,"\n" )

#Mineros, pasamos de una ganancia inicial de 6072750
#a una ganancia de 
#Los invito a que sean creativos  e inventen nuevas conbinaciones de  Visa_status y Master_status

