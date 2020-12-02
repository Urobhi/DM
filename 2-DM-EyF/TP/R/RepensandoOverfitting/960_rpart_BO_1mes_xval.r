#Optimizacion Bayesiana de hiperparametros de  rpart
#atencion con los nombres de las carpetas
#Entrena sobre 201906
#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")
require("rpart")

#paquetes necesarios para la Bayesian Optimization
require("DiceKriging")
require("mlrMBO")


kexperimento <-  "960"   #cambiar esto en cada corrida !

#en estos archivos queda el resultado
kbayesiana  <-  paste0(".//work//rpart_BO_", kexperimento, ".RDATA" )
kBO_iter    <-  100  #cantidad de iteraciones de la Optimizacion Bayesiana


#------------------------------------------------------------------------------
#funcion que va a optimizar la Bayesian Optimization
estimar_rpart <- function( x )
{
  #multiplico para obtener vminbucket
  vminbucket <-  as.integer( round(x$pminbucket * x$pminsplit))

  #genero modelo sobre training
  ganancia_suma <- 0
  
  for( vfold  in 1:5 )  #5-fold cross validation
  {
    modelo <-  rpart("clase_binaria ~ . ",
                     data= d201906[ fold != vfold, ],
                     xval= 0, 
                     cp=        x$pcp, 
                     minsplit=  x$pminsplit, 
                     maxdepth=  x$pmaxdepth, 
                     minbucket= vminbucket
                    )

    #calculo la ganancia en los datos de testing
    prediccion_test  <- predict( modelo, d201906[fold == vfold], type = "prob")[,"POS"]
    ganancia_test    <- sum( (prediccion_test> 0.025) * 
                             d201906[fold == vfold, ifelse( clase_binaria=="POS",29250,-750)])
    
    ganancia_suma <- ganancia_suma + ganancia_test
  }
  
  return( ganancia_suma )
}
#------------------------------------------------------------------------------

#Aqui comienza el programa
setwd("~/buckets/b2/" )  #establezco la carpeta donde voy a trabajar
#cargo el dataset
dataset <- fread( "./datasetsOri/paquete_premium_201906_202005.txt.gz")

#creo la clase_binaria
dataset[  , clase_binaria := ifelse( clase_ternaria=="BAJA+2", "POS", "NEG" ) ]
dataset[  , clase_ternaria := NULL ]  #elimino la clase_ternaria, ya no la necesito

#genero los datasets de training y testing
d201906  <- copy( dataset[  foto_mes==201906, ] )

set.seed( 102191 )  #semilla

d201906[ , azar := runif( nrow( d201906 ) ) ]
setorderv( d201906, c("clase_binaria","azar") )
d201906[  , idx := .I ]
d201906[  , fold :=  ( (idx %% 5) +1 )]
d201906[ , idx:= NULL ]
d201906[ , azar:= NULL ]


#Aqui comienza la configuracion de la Bayesian Optimization
configureMlr(show.learner.output = FALSE)

funcion_optimizar <-  estimar_rpart

#configuro la busqueda bayesiana,  los hiperparametros que se van a optimizar
#por favor, no desesperarse por lo complejo
obj.fun <- makeSingleObjectiveFunction(
            fn   = funcion_optimizar,
            minimize= FALSE,   #estoy Maximizando la ganancia
            noisy=    TRUE,
            par.set = makeParamSet(
                        makeIntegerParam("pmaxdepth" , lower=3L    , upper=  25L),
                        makeNumericParam("pminbucket", lower=0.001 , upper=   0.5),
                        makeIntegerParam("pminsplit" , lower=1L    , upper= 400L),
                        makeNumericParam("pcp"       , lower=0.0   , upper=   0.001) ),
            has.simple.signature = FALSE
           )


ctrl <-  makeMBOControl( save.on.disk.at.time = 600,  save.file.path = kbayesiana )
ctrl <-  setMBOControlTermination(ctrl, iters = kBO_iter )
ctrl <-  setMBOControlInfill(ctrl, crit = makeMBOInfillCritEI())

surr.km <-  makeLearner("regr.km", predict.type= "se", covtype= "matern3_2", control = list(trace = FALSE))

t0       <-  Sys.time()

if(!file.exists(kbayesiana))
{
  #lanzo la busqueda bayesiana
  run  <-  mbo(obj.fun, learner = surr.km, control = ctrl)
} else {

  #retoma el procesamiento en donde lo dejo
  run <- mboContinue( kbayesiana ) 
}

t1       <-  Sys.time()
tcorrida <-  as.numeric( t1 - t0, units = "secs")


#genero modelo sobre TODO el mes 201906
dtrain_final <- dataset[  foto_mes==201906, ]
t0       <-  Sys.time()
modelo <-  rpart("clase_binaria ~ . ",
                 data= dtrain_final,
                 xval= 0, 
                 cp=        run$x$pcp, 
                 minsplit=  run$x$pminsplit, 
                 maxdepth=  run$x$pmaxdepth, 
                 minbucket= as.integer( round(run$x$pminbucket * run$x$pminsplit))
                )

daplicar  <-  dataset[  foto_mes==201908, ]

prediccion_aplicar  <- predict( modelo, daplicar, type = "prob")[,"POS"]
ganancia_aplicar    <- sum( (prediccion_aplicar> 0.025) * 
                            daplicar[, ifelse( clase_binaria=="POS",29250,-750)])
t1       <-  Sys.time()
taplicar <-  as.numeric( t1 - t0, units = "secs")

