#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")
require("lightgbm")


#paquetes necesarios para la Bayesian Optimization
require("DiceKriging")
require("mlrMBO")


#------------------------------------------------------------------------------

estimar_lightgbm <- function( pcampo, pmes1, pmes2 )
{

  campos_buenos <-   c(  pcampo )

  dBO_train  <-   lgb.Dataset( data  = data.matrix(  dataset[ foto_mes==pmes1, campos_buenos, with=FALSE]),
                               label = dataset[ foto_mes==pmes1, clase01],
                               free_raw_data=TRUE
                             )

  dBO_test   <-   lgb.Dataset( data  = data.matrix(  dataset[ foto_mes==pmes2, campos_buenos, with=FALSE]),
                               label = dataset[ foto_mes==pmes2, clase01],
                               free_raw_data=TRUE
                             )

  modelo <-  lgb.train(data= dBO_train,
                       objective= "binary",  #la clase es binaria
                       eval= "auc",
                       valids= list( valid= dBO_test),
                       metric= "custom",  #ATENCION   tremendamente importante
                       boost_from_average= TRUE,
                       num_iterations=  999999,  #un numero muy grande
                       early_stopping_rounds=  50,
                       learning_rate= 0.1,
                       feature_fraction= 1.0,
                       min_gain_to_split=  0,
                       num_leaves=  8,
                       lambda_l1= 0,
                       lambda_l2= 0,
                       max_bin= 255,
                       num_threads= 1,
                       verbosity= -1,
                       verbose= -1
                      )

  AUC  <- unlist(modelo$record_evals$valid$auc$eval)[ modelo$best_iter ] 
  
  return(  list( "campo"= pcampo,
                 "mes1"=  pmes1,
                 "mes2"=  pmes2,
                 "auc"=  AUC)
        )
}
#------------------------------------------------------------------------------


#Aqui comienza el programa
setwd("~/buckets/b1")

#cargo el dataset donde voy a entrenar
dataset <- fread("./datasetsOri/paquete_premium.txt.gz")

#paso la clase a binaria que tome valores {0,1}  enteros
dataset[ , clase01 :=  ifelse( clase_ternaria=="BAJA+2", 1L, 0L)  ]

periodos <-  sort( unique( dataset$foto_mes ) ) 
columnas  <-  setdiff( colnames( dataset ) ,  c("clase_ternaria","clase01" ) )

tb_final <-  data.table(  "campo"=character(),
                          "mes1"= integer(),
                          "mes2"= integer(),
                          "auc"= numeric()
                        )
for( vcampo  in  columnas )
{
for( vmes1  in 25:34 )
{
for( vmes2  in 36:38 )
{
    res <- estimar_lightgbm( vcampo, periodos[vmes1], periodos[vmes2] )
    tb_final <-  rbind(  tb_final, res )

    fwrite(  tb_final, file="./work/super_tabla.txt", sep="\t" ) 
}
}
}

fwrite(  tb_final, file="./work/super_tabla.txt", sep="\t" ) 

#------------------------------------------------------------------------------

tb_final <- fread( file="./work/super_tabla.txt" )

tb_final <-  tb_final[ campo != "numero_de_cliente" & campo!="foto_mes" ]

a1 <-  tb_final[  , list( auc_min=min(auc), auc_max=max(auc) ), by=c("mes2", "campo" ) ]

a1[ tb_final, on=c( "auc_min"="auc", "mes2", "campo" ),  mes1 := mes1 ]


a1[ , delta:= auc_max - auc_min ]
setorder( a1, -delta )

a1[ 1:50]

a2 <-  a1[ delta > 0.1

tb_final[  campo=="mtarjeta_visa_descuentos" &  mes2==201912, ]

