#Optimizacion Bayesiana de hiperparametros de  rpart
#atencion con los nombres de las carpetas
#Entrena sobre 201909
#Testea sobre  201911
#verificar que se tienen instaladas todas las LIBRERIAS necesarias


#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")
require("rpart")

#paquetes necesarios para la Bayesian Optimization
require("DiceKriging")
require("mlrMBO")


kexperimento <-  "01"   #cambiar esto en cada corrida !

#en estos archivos queda el resultado
kbayesiana  <-  paste0(".//work//rpart_BO_", kexperimento, ".RDATA" )
klog        <-  paste0(".//work//rpart_BO_", kexperimento, ".txt" )
ksalida     <-  paste0(".//work//rpart_BO_salida", kexperimento, ".txt" )
kBO_iter    <-  100  #cantidad de iteraciones de la Optimizacion Bayesiana


#------------------------------------------------------------------------------
#funcion que va a optimizar la Bayesian Optimization

estimar_rpart <- function( x )
{
  #multiplico para obtener vminbucket
  vminbucket <-  as.integer( round(x$pminbucket * x$pminsplit))
  
  #genero modelo sobre training
  modelo <-  rpart("clase_binaria ~ . ",
                   data= dtrain,
                   xval= 0, 
                   cp=        x$pcp, 
                   minsplit=  x$pminsplit, 
                   maxdepth=  x$pmaxdepth, 
                   minbucket= vminbucket
                  )

  #calculo la ganancia en los datos de testing
  prediccion_test  <- predict( modelo, dtest, type = "prob")[,"POS"]
  ganancia_test    <- sum( (prediccion_test> 0.025) * 
                           dtest[, ifelse( clase_binaria=="POS",29250,-750)])

  #imprimo los resultados al archivo klog
  cat( file= klog, 
       append= TRUE,
       sep="",
       format(Sys.time(), "%Y%m%d %H%M%S"), "\t",
       x$pcp, "\t",
       x$pmaxdepth, "\t",
       x$pminsplit, "\t",
       vminbucket, "\t",
       ganancia_test, "\n" )

  return( ganancia_test )
}
#------------------------------------------------------------------------------

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


#escribo los titulos
if(!file.exists( klog ) )
{
  cat( file= klog, 
       append= FALSE,
       sep="",
       "fecha", "\t",
       "cp", "\t",
       "maxdepth", "\t", 
       "minsplit", "\t",
       "minbucket", "\t",
       "gan_testing", "\n")
}


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


if(!file.exists(kbayesiana))
{
  #lanzo la busqueda bayesiana
  run  <-  mbo(obj.fun, learner = surr.km, control = ctrl)
} else {

  #retoma el procesamiento en donde lo dejo
  run <- mboContinue( kbayesiana ) 
}

cat( "Los parametros optimos son:","\n" )
cat( "cp=", run$x$pcp, "\n" )
cat( "maxdepth=", run$x$pmaxdepth, "\n" )
cat( "minsplit=", run$x$pminsplit, "\n" )
cat( "minbucket=", round(run$x$pminsplit * run$x$pminbucket), "\n" )

cat( file= ksalida, append=TRUE, "Los parametros optimos son:","\n" )
cat( file= ksalida, append=TRUE, "cp=", run$x$pcp, "\n" )
cat( file= ksalida, append=TRUE, "maxdepth=", run$x$pmaxdepth, "\n" )
cat( file= ksalida, append=TRUE, "minsplit=", run$x$pminsplit, "\n" )
cat( file= ksalida, append=TRUE, "minbucket=", round(run$x$pminsplit * run$x$pminbucket), "\n" )
