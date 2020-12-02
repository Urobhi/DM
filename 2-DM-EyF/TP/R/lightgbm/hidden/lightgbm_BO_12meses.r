#Optimizacion Bayesiana de hiperparametros
#algoritmo LightGBM
#libreria   lightgbm
#verificar que se tienen instaladas todas las LIBRERIAS necesarias


#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")
require("lightgbm")


#paquetes necesarios para la Bayesian Optimization
require("DiceKriging")
require("mlrMBO")


kexperimento <-  "31"   #cambiar esto en cada corrida !

#en estos archivos queda el resultado
kbayesiana  <-  paste0("./work/lightgbm_BO_12meses_", kexperimento, ".RDATA" )
ksalida     <-  paste0("./work/lightgbm_BO_12meses_salida_", kexperimento, ".txt" )
ksubmit     <-  paste0("./work/lightgbm_BO_12meses_submit_", kexperimento, ".csv" )
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
                       lambda_l1= 0,
                       lambda_l2= 0,
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

#Aqui comienza el programa
setwd("~/buckets/b1")

#cargo el dataset donde voy a entrenar
dataset <- fread("./datasetsOri/paquete_premium.txt.gz")

#paso la clase a binaria que tome valores {0,1}  enteros
dataset[ , clase01 :=  ifelse( clase_ternaria=="BAJA+2", 1L, 0L)  ]

#los campos que se van a utilizar
campos_buenos  <- setdiff(  colnames(dataset) ,  c("clase_ternaria","clase01") )


dataset[ foto_mes>=201809 & foto_mes<=201908 , BO_train:=1]  #entreno en 12 meses
dataset[ foto_mes==201910 | foto_mes==201911,  BO_test:= 1]  #lo aplico a 2 meses

#dejo los datos en el formato que necesita LightGBM
dBO_train  <-   lgb.Dataset( data  = data.matrix(  dataset[ BO_train==1, campos_buenos, with=FALSE]),
                             label = dataset[ BO_train==1, clase01],
                             free_raw_data=TRUE
                           )

dBO_test   <-   lgb.Dataset( data  = data.matrix(  dataset[ BO_test==1, campos_buenos, with=FALSE]),
                             label = dataset[ BO_test==1, clase01],
                             free_raw_data=TRUE
                           )

#ahora ya puedo borrar el dataset porque puse free_raw_data=TRUE
rm( dataset )
gc()


#Aqui comienza la configuracion de la Bayesian Optimization

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
            makeNumericParam("pmin_gain_to_split",lower=  0.0  , upper=   10),
            makeNumericParam("plearning_rate",    lower=  0.001 , upper=   0.050)
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
     "lambda_l1",         run$x$plambda_l1, "\n",
     "lambda_l2",         run$x$plambda_l2, "\n",
     file=ksalida, 
     sep="\t", 
     append=TRUE )
