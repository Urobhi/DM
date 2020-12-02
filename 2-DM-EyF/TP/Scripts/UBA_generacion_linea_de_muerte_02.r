#Generacion de la Linea de Muerte para la UBA año 2020
#Solo necesita 32GB de memoria RAM  , 8 vCPU   y una hora para correr

#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")
require("lightgbm")
require("DiceKriging")
require("mlrMBO")

#en estos archivos queda el resultado
kbayesiana  <-  paste0("./work/linea_de_muerte_04.RDATA" )

kBO_iter    <-  10  #cantidad de iteraciones de la Optimizacion Bayesiana

#------------------------------------------------------------------------------
#esta es la funcion de ganancia, que se busca optimizar
#se usa internamente a LightGBM
#se calcula internamente la mejor ganancia para todos los puntos de corte posibles

fganancia_logistic_lightgbm   <- function(probs, data) 
{
  vlabels <- getinfo(data, "label")

  tbl <- as.data.table( list( "prob"=probs, "gan"= ifelse( vlabels==1, 29250, -750 ) ) )

  setorder( tbl, -prob )
  tbl[ , gan_acum :=  cumsum( gan ) ]
  gan <- max( tbl$gan_acum )

  return(  list( name= "ganancia", value=  gan, higher_better= TRUE ) )
}
#------------------------------------------------------------------------------
#funcion que va a optimizar la Bayesian Optimization

estimar_lightgbm <- function( x )
{
  modelo <-  lgb.train(data= dBO_train,
                       objective= "binary",  #la clase es binaria
                       eval= fganancia_logistic_lightgbm,  #esta es la fuciona optimizar
                       valids= list( valid1= dBO_test1 ),
                       first_metric_only= TRUE,
                       metric= "custom",  #ATENCION   tremendamente importante
                       num_iterations=  999999,  #un numero muy grande
                       early_stopping_rounds=  200,
                       min_data_in_leaf= as.integer( x$pmin_data_in_leaf ),
                       feature_fraction= 0.25,
                       learning_rate= 0.02,
                       feature_pre_filter= FALSE,
                       verbose= -1,
                       seed= 102191
                      )

  ganancia1  <- unlist(modelo$record_evals$valid1$ganancia$eval)[ modelo$best_iter ] 

  #esta es la forma de devolver un parametro extra
  attr(ganancia1 ,"extras" ) <- list("pnum_iterations"= modelo$best_iter )

  cat( modelo$best_iter, ganancia1, "\n" )

  return( ganancia1 )
}
#------------------------------------------------------------------------------
#Aqui comienza el programa
setwd("~/buckets/b2")

dataset  <- fread("./datasetsOri/paquete_premium.txt.gz")

campos_lags  <- setdiff(  colnames(dataset) ,  c("clase_ternaria","clase01", "numero_de_cliente","foto_mes") )

#agreglo los lags de orden 1
setorderv( dataset, c("numero_de_cliente","foto_mes") )
dataset[,  paste0( campos_lags, "_lag1") :=shift(.SD, 1, NA, "lag"), by=numero_de_cliente, .SDcols= campos_lags]

#agrego los deltas de los lags, de una forma nada elegante
for( vcol in campos_lags )
{
   dataset[,  paste0(vcol, "_delta1") := get( vcol)  - get(paste0( vcol, "_lag1"))]
}
 
#paso la clase a binaria que tome valores {0,1}  enteros
dataset[ , clase01 :=  ifelse( clase_ternaria=="BAJA+2", 1L, 0L)  ]


#los campos que se van a utilizar, intencionalmente no uso  numero_de_cliente
campos_buenos  <- setdiff(  colnames(dataset) ,  c("clase_ternaria","clase01","numero_de_cliente") )

#hago undersampling de los negativos
#me quedo con TODOS los positivos, pero con solo el 5% de los negativos
set.seed(102191)
dataset[ , azar:= runif( nrow(dataset) ) ]

dataset[ ( foto_mes>=201701 & foto_mes<=202003 & foto_mes!=201905 & ( clase01==1 | azar<=0.05) ),  BO_train := 1L]

#Testeo en 201902, el mismo mes pero un año antes
dataset[ foto_mes==201905,  BO_test1 := 1L]

#dejo los datos en el formato que necesita LightGBM
dBO_train  <- lgb.Dataset( data  = data.matrix(  dataset[ BO_train==1, campos_buenos, with=FALSE]),
                           label = dataset[ BO_train==1, clase01],
                           free_raw_data = TRUE
                         )

dBO_test1  <- lgb.Dataset( data  = data.matrix(  dataset[ BO_test1==1, campos_buenos, with=FALSE]),
                           label = dataset[ BO_test1==1, clase01],
                           free_raw_data = TRUE
                         )

dataset_aplicacion <- copy( dataset[ foto_mes==202005, ] )

#libero la memoria borrando el dataset
rm(dataset)
gc()

#Aqui comienza la configuracion de la Bayesian Optimization
configureMlr(show.learner.output = FALSE)

#configuro la busqueda bayesiana,  los hiperparametros que se van a optimizar
#por favor, no desesperarse por lo complejo
obj.fun <- makeSingleObjectiveFunction(
        name = "OptimBayesiana",  #un nombre que no tiene importancia
        fn   = estimar_lightgbm,  #aqui va la funcion que quiero optimizar
        minimize= FALSE,  #quiero maximizar la ganancia 
        par.set = makeParamSet(
            makeNumericParam("pmin_data_in_leaf",  lower=  10, upper=  30000 )
            ),
        has.simple.signature = FALSE,  #porque le pase los parametros con makeParamSet
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

#En  run$x$pmin_data_in_leaf  ha quedo el optimo
#------------------------------------------------------------------------------

#calculo el modelo final
modelo_final <- lgb.train(data= dBO_train,
                          objective= "binary",
                          eval= fganancia_logistic_lightgbm,
                          valids= list( valid1= dBO_test1 ),
                          first_metric_only= TRUE,
                          metric= "custom",
                          num_iterations=  999999,
                          early_stopping_rounds=  200,
                          learning_rate= 0.02,  #ATENCION, este es el valor que se cambia
                          min_data_in_leaf= as.integer( run$x$pmin_data_in_leaf ),
                          feature_pre_filter= FALSE,
                          feature_fraction= 0.25,
                          verbose= -1,
                          seed= 102191
                         )

#Genero los archivos que voy a probar contra el Leaderboard Publico y quedarme con el mejor
prediccion_202005  <- predict( modelo_final,  data.matrix( dataset_aplicacion[  , campos_buenos, with=FALSE]))

#Genero  posibles probabilidades de corte, tener en 
for( vprob_corte  in (15:35)/100 )  #de 0.15 a 0.35
{
  entrega  <- as.data.table( list( "numero_de_cliente"=  dataset_aplicacion[ , numero_de_cliente],  
                                   "estimulo"=  (prediccion_202005> vprob_corte)  ) )

  #genero el archivo de salida
  fwrite( entrega, logical01=TRUE, sep=",",  file= paste0("~/buckets/b2/work/BORRADOR_lineademuerte_04_", vprob_corte*100, ".csv") )
}

#Se deben probar con el metodo de busqueda binaria contra el leaderboard publico las salidas, y quedarse con el mejor

 