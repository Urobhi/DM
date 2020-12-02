#UNDER CONSTRUCTION

#Optimizacion Bayesiana de hiperparametros
#algoritmo LightGBM
#libreria   lightgbm
#compara NO corregir  vs  corregir

#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")
require("lightgbm")


#paquetes necesarios para la Bayesian Optimization
require("DiceKriging")
require("mlrMBO")


kexperimento <-  "66"   #cambiar esto en cada corrida !

#en este archivos queda el resultado de las DOS bayesiana
ksalida     <-  paste0("./work/lightgbm_compara_", kexperimento, ".txt" )

kBO_iter    <-  100  #cantidad de iteraciones de la Optimizacion Bayesiana


#------------------------------------------------------------------------------
#esta es la funcion de ganancia, que se busca optimizar
#se usa internamente a LightGBM
fganancia_logistic_lightgbm   <- function(probs, data) 
{
   vlabels <- getinfo(data, "label")
  
   gan <-sum(   (probs > 0.025  ) * 
                 ifelse( vlabels== 1, +29250, -750 )   
            )

   return(  list( name = "ganancia", 
                  value =  ifelse(  is.na(gan) , 0, gan) ,
                  higher_better= TRUE 
                )
         )
}
#------------------------------------------------------------------------------
#funcion que va a optimizar la Bayesian Optimization

estimar_lightgbm <- function( x )
{
  set.seed( 102191 )  # para que siempre me de el mismo resultado

  modelo <-  lgb.train(data= dBO_train,
                       objective= "binary",  #la clase es binaria
                       eval= fganancia_logistic_lightgbm,  #esta es la fuciona optimizar
                       valids= list( valid= dBO_test),
                       metric= "custom",  #ATENCION   tremendamente importante
                       boost_from_average= TRUE,
                       num_iterations=  999999,  #un numero muy grande
                       early_stopping_rounds= as.integer(50 + 5/x$plearning_rate),
                       learning_rate= x$plearning_rate,
                       feature_fraction= x$pfeature_fraction,
                       min_gain_to_split=  x$pmin_gain_to_split,
                       num_leaves=  x$pnum_leaves,
                       lambda_l1= 0.0,
                       lambda_l2= 0.0,
                       max_bin= 31,
                       verbosity= -1,
                       verbose= -1
                      )

  nrounds_optimo <- modelo$best_iter
  ganancia       <- unlist(modelo$record_evals$valid$ganancia$eval)[ nrounds_optimo ] 
  
  attr(ganancia ,"extras" ) <- list("pnum_iterations"= modelo$best_iter)  #esta es la forma de devolver un parametro extra

  cat( ganancia, " " )

  return( ganancia )
}
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#Aqui comienza el programa


setwd("~/buckets/b2")

#cargo el dataset donde voy a entrenar
dataset <- fread("./datasetsOri/paquete_premium.txt.gz")

#paso la clase a binaria que tome valores {0,1}  enteros
dataset[ , clase01 :=  ifelse( clase_ternaria=="BAJA+2", 1L, 0L)  ]

#los campos que se van a utilizar
campos_buenos  <- setdiff(  colnames(dataset) ,  c("clase_ternaria","clase01") )


#CUATRO MESES
dataset[ foto_mes>=201906 & foto_mes<=201909 , BO_train:=1]  #entreno en 10 meses
dataset[ foto_mes==201911,  BO_test:= 1]  #testeo en la union de DOS meses


#primero SIN  corregir
#dejo los datos en el formato que necesita LightGBM
dBO_train  <-   lgb.Dataset( data=  data.matrix(  dataset[ BO_train==1, campos_buenos, with=FALSE]),
                             label= dataset[ BO_train==1, clase01],
                             free_raw_data= TRUE
                           )

dBO_test   <-   lgb.Dataset( data=  data.matrix(  dataset[ BO_test==1, campos_buenos, with=FALSE]),
                             label= dataset[ BO_test==1, clase01],
                             free_raw_data= TRUE
                           )

#Aqui comienza la configuracion de la Bayesian Optimization

kbayesiana  <-  paste0("./work/lightgbm_compara_sincorregir", kexperimento, ".RDATA" )
configureMlr(show.learner.output = FALSE)

funcion_optimizar <-  estimar_lightgbm  #esta funcion se debe construir

#configuro la busqueda bayesiana,  los hiperparametros que se van a optimizar
#por favor, no desesperarse por lo complejo
obj.fun <- makeSingleObjectiveFunction(
        name = "OptimBayesiana",  #un nombre que no tiene importancia
        fn   = funcion_optimizar,  #aqui va la funcion que quiero optimizar
        minimize= FALSE,  #quiero maximizar la ganancia 
        par.set = makeParamSet(
            makeIntegerParam("pnum_leaves",       lower=  8L   , upper= 1023L),
            makeNumericParam("pfeature_fraction", lower=  0.10 , upper=    1.0),
            makeNumericParam("pmin_gain_to_split",lower=  0.0  , upper=   20),
            makeNumericParam("plearning_rate",    lower=  0.01 , upper=    0.1)
        ),
        has.simple.signature = FALSE,  #porque le pase los paratros con makeParamSet
        noisy= TRUE
        )

ctrl  <-  makeMBOControl( save.on.disk.at.time = 600,  save.file.path = kbayesiana )
ctrl  <-  setMBOControlTermination(ctrl, iters = kBO_iter )
ctrl  <-  setMBOControlInfill(ctrl, crit = makeMBOInfillCritEI())

surr.km  <-  makeLearner("regr.km", predict.type= "se", covtype= "matern3_2", control = list(trace = FALSE))



if(!file.exists(kbayesiana))
{
  #lanzo la busqueda bayesiana
  run  <-  mbo(obj.fun, learner = surr.km, control = ctrl)
} else {

  #retoma el procesamiento en donde lo dejo
  run <- mboContinue( kbayesiana ) 
}


#obtengo el pnrounds de la mejor corrida
tbl <- as.data.table(run$opt.path)
setorder( tbl, -y)
mejor_pnrounds <- tbl[ 1, pnum_iterations]



cat( "ganancia",          run$y, "\n",
     "nrounds",           mejor_pnrounds, "\n",
     "num_leaves",        run$x$pnum_leaves, "\n",
     "feature_fraction",  run$x$pfeature_fraction, "\n",
     "min_gain_to_split", run$x$pmin_gain_to_split, "\n",
     "learning_rate",     run$x$plearning_rate, "\n",
     file=ksalida, 
     sep="\t", 
     append=TRUE )


#CUATRO MESES
dataset[ foto_mes>=201908 & foto_mes<=201911 , generate:=1]  #entreno en 10 meses

#dejo los datos en el formato que necesita LightGBM
dgeneracion  <- lgb.Dataset( data= data.matrix(  dataset[ generate==1, campos_buenos, with=FALSE]),
                             label= dataset[ generate==1, clase01],
                             free_raw_data= FALSE )

#genero el modelo usando los hiperparametros optimos
#encontrados en la optimizacion bayesiana
modelo  <- lgb.train( data= dgeneracion,
                      objective= "binary",
                      metric= "custom",  #ATENCION   tremendamente importante
                      boost_from_average= TRUE,
                      max_bin= 31,
                      num_iterations=    mejor_pnrounds,
                      learning_rate=     run$x$plearning_rate,
                      feature_fraction=  run$x$pfeature_fraction,
                      min_gain_to_split= run$x$pmin_gain_to_split,
                      num_leaves=        run$x$pnum_leaves,
                      lambda_l1=         0.0,
                      lambda_l2=         0.0
                    )

dapply <-  dataset[ foto_mes==202001 ]

#aplico el modelo a los datos sin clase, 202001
prediccion_202001  <- predict( modelo, 
                               data.matrix( dapply[, campos_buenos, with=FALSE ])                                 )

#genero el dataset de entrega
entrega <-   as.data.table(cbind( "numero_de_cliente"=dapply$numero_de_cliente,  "prob" =prediccion_202001) )
entrega[  ,  estimulo :=  as.integer( prob > 0.025)]

#genero el archivo de salida
fwrite( entrega[ ,  c("numero_de_cliente", "estimulo"), with=FALSE], sep=",",  file="./work/lightgbm_sincorregir_entrega.csv")

#------------------------------------------------------------------------------
#ahora paso a generar el modelo CORRIGIENDO los datos
#obviamente voy a tener que volver a hacer una nueva optimizacion bayesiana


#acomodo los errores del dataset
dataset[ foto_mes==201701,  ccajas_consultas   := -9999 ]
dataset[ foto_mes==201702,  ccajas_consultas   := -9999 ]

dataset[ foto_mes==201801,  internet   := -9999 ]
dataset[ foto_mes==201801,  thomebanking   := -9999 ]
dataset[ foto_mes==201801,  chomebanking_transacciones   := -9999 ]
dataset[ foto_mes==201801,  tcallcenter   := -9999 ]
dataset[ foto_mes==201801,  ccallcenter_transacciones   := -9999 ]
dataset[ foto_mes==201801,  cprestamos_personales   := -9999 ]
dataset[ foto_mes==201801,  mprestamos_personales   := -Inf ]
dataset[ foto_mes==201801,  mprestamos_hipotecarios  := -Inf ]
dataset[ foto_mes==201801,  ccajas_transacciones   := -9999 ]
dataset[ foto_mes==201801,  ccajas_consultas   := -9999 ]
dataset[ foto_mes==201801,  ccajas_depositos   := -9999 ]
dataset[ foto_mes==201801,  ccajas_extracciones   := -9999 ]
dataset[ foto_mes==201801,  ccajas_otras   := -9999 ]

dataset[ foto_mes==201806,  tcallcenter   :=  -9999 ]
dataset[ foto_mes==201806,  ccallcenter_transacciones   :=  -9999 ]

dataset[ foto_mes==201904,  ctarjeta_visa_debitos_automaticos  :=  -9999 ]
dataset[ foto_mes==201904,  mttarjeta_visa_debitos_automaticos  := -Inf ]

dataset[ foto_mes==201905,  mrentabilidad     := -Inf ]
dataset[ foto_mes==201905,  mrentabilidad_anual     := -Inf ]
dataset[ foto_mes==201905,  mcomisiones       := -Inf ]
dataset[ foto_mes==201905,  mpasivos_margen  := -Inf ]
dataset[ foto_mes==201905,  mactivos_margen  := -Inf ]
dataset[ foto_mes==201905,  ctarjeta_visa_debitos_automaticos  := -9999 ]
dataset[ foto_mes==201905,  ccomisiones_otras := -9999 ]
dataset[ foto_mes==201905,  mcomisiones_otras := -Inf ]

dataset[ foto_mes==201910,  mpasivos_margen  := -Inf ]
dataset[ foto_mes==201910,  mactivos_margen  := -Inf ]
dataset[ foto_mes==201910,  ccomisiones_otras := -9999 ]
dataset[ foto_mes==201910,  mcomisiones_otras := -Inf ]
dataset[ foto_mes==201910,  mcomisiones       := -Inf ]
dataset[ foto_mes==201910,  mrentabilidad     := -Inf ]
dataset[ foto_mes==201910,  mrentabilidad_annual     := -Inf ]
dataset[ foto_mes==201910,  chomebanking_transacciones   := -9999 ]
dataset[ foto_mes==201910,  ctarjeta_visa_descuentos   := -9999 ]
dataset[ foto_mes==201910,  ctarjeta_master_descuentos   := -9999 ]
dataset[ foto_mes==201910,  mtarjeta_visa_descuentos   := -Inf ]
dataset[ foto_mes==201910,  mtarjeta_master_descuentos    := -Inf ]
dataset[ foto_mes==201910,  ccajeros_propios_descuentos   := -9999 ]
dataset[ foto_mes==201910,  mcajeros_propios_descuentos   := -Inf ]

dataset[ foto_mes==202001,  cliente_vip   := -9999 ]

campos_buenos  <- setdiff(  campos_buenos  ,  c("cliente_vip") )

#--------------------------------------
#Ahora ESCALO los campos
#solo para los meses que necesito

fdividir_maximo  <- function( pvector )
{
  maximo <-  max( pvector, na.rm=TRUE )

  return(  pvector / maximo )
}

camposMonto <-  colnames(dataset)[  like( colnames(dataset), "^m|^Visa_m|^Master_m" ) ]

dataset[ foto_mes>=201906, (camposMonto) := lapply( .SD, frankv,  na.last="keep", ties.method="dense" ) , by=foto_mes, .SDcols= camposMonto ]
dataset[ foto_mes>=201906, (camposMonto) := lapply( .SD, fdividir_maximo ) , by=foto_mes, .SDcols= camposMonto ]

dataset[ foto_mes>=201906,

#--------------------------------------


#CUATRO MESES
dataset[ foto_mes>=201906 & foto_mes<=201909 , BO_train:=1]  #entreno en 10 meses
dataset[ foto_mes==201911,  BO_test:= 1]  #testeo en la union de DOS meses


#primero SIN  corregir
#dejo los datos en el formato que necesita LightGBM
dBO_train  <-   lgb.Dataset( data=  data.matrix(  dataset[ BO_train==1, campos_buenos, with=FALSE]),
                             label= dataset[ BO_train==1, clase01],
                             free_raw_data= TRUE
                           )

dBO_test   <-   lgb.Dataset( data=  data.matrix(  dataset[ BO_test==1, campos_buenos, with=FALSE]),
                             label= dataset[ BO_test==1, clase01],
                             free_raw_data= TRUE
                           )

#Aqui comienza la configuracion de la Bayesian Optimization

#atencion, cambio el archivo donde queda la bayesiana
#le cambio el nombre a corregido
kbayesiana  <-  paste0("./work/lightgbm_compara_corregido", kexperimento, ".RDATA" )
configureMlr(show.learner.output = FALSE)

funcion_optimizar <-  estimar_lightgbm  #esta funcion se debe construir

#configuro la busqueda bayesiana,  los hiperparametros que se van a optimizar
#por favor, no desesperarse por lo complejo
obj.fun <- makeSingleObjectiveFunction(
        name = "OptimBayesiana",  #un nombre que no tiene importancia
        fn   = funcion_optimizar,  #aqui va la funcion que quiero optimizar
        minimize= FALSE,  #quiero maximizar la ganancia 
        par.set = makeParamSet(
            makeIntegerParam("pnum_leaves",       lower=  8L   , upper= 1023L),
            makeNumericParam("pfeature_fraction", lower=  0.10 , upper=    1.0),
            makeNumericParam("pmin_gain_to_split",lower=  0.0  , upper=   20),
            makeNumericParam("plearning_rate",    lower=  0.01 , upper=    0.1)
        ),
        has.simple.signature = FALSE,  #porque le pase los paratros con makeParamSet
        noisy= TRUE
        )

ctrl  <-  makeMBOControl( save.on.disk.at.time = 600,  save.file.path = kbayesiana )
ctrl  <-  setMBOControlTermination(ctrl, iters = kBO_iter )
ctrl  <-  setMBOControlInfill(ctrl, crit = makeMBOInfillCritEI())

surr.km  <-  makeLearner("regr.km", predict.type= "se", covtype= "matern3_2", control = list(trace = FALSE))



if(!file.exists(kbayesiana))
{
  #lanzo la busqueda bayesiana
  run  <-  mbo(obj.fun, learner = surr.km, control = ctrl)
} else {

  #retoma el procesamiento en donde lo dejo
  run <- mboContinue( kbayesiana ) 
}

#obtengo el pnrounds de la mejor corrida
tbl <- as.data.table(run$opt.path)
setorder( tbl, -y)
mejor_pnrounds <- tbl[ 1, pnum_iterations]



cat( "ganancia",          run$y, "\n",
     "nrounds",           mejor_pnrounds, "\n",
     "num_leaves",        run$x$pnum_leaves, "\n",
     "feature_fraction",  run$x$pfeature_fraction, "\n",
     "min_gain_to_split", run$x$pmin_gain_to_split, "\n",
     "learning_rate",     run$x$plearning_rate, "\n",
     file=ksalida, 
     sep="\t", 
     append=TRUE )


#CUATRO MESES
dataset[ foto_mes>=201908 & foto_mes<=201911 , generate:=1]  #entreno en 10 meses

#dejo los datos en el formato que necesita LightGBM
dgeneracion  <- lgb.Dataset( data= data.matrix(  dataset[ generate==1, campos_buenos, with=FALSE]),
                             label= dataset[ generate==1, clase01],
                             free_raw_data= FALSE )

#genero el modelo usando los hiperparametros optimos
#encontrados en la optimizacion bayesiana
modelo  <- lgb.train( data= dgeneracion,
                      objective= "binary",
                      metric= "custom",  #ATENCION   tremendamente importante
                      boost_from_average= TRUE,
                      max_bin= 31,
                      num_iterations=    mejor_pnrounds,
                      learning_rate=     run$x$plearning_rate,
                      feature_fraction=  run$x$pfeature_fraction,
                      min_gain_to_split= run$x$pmin_gain_to_split,
                      num_leaves=        run$x$pnum_leaves,
                      lambda_l1=         0.0,
                      lambda_l2=         0.0
                    )

dapply <-  dataset[ foto_mes==202001 ]

#aplico el modelo a los datos sin clase, 202001
prediccion_202001  <- predict( modelo, 
                               data.matrix( dapply[, campos_buenos, with=FALSE ])                                 )

#genero el dataset de entrega
entrega <-   as.data.table(cbind( "numero_de_cliente"=dapply$numero_de_cliente,  "prob" =prediccion_202001) )
entrega[  ,  estimulo :=  as.integer( prob > 0.025)]

#genero el archivo de salida
fwrite( entrega[ ,  c("numero_de_cliente", "estimulo"), with=FALSE], sep=",",  file="./work/lightgbm_corregidos_entrega.csv")
