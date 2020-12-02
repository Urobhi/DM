#Optimizacion Bayesiana de hiperparametros
#algoritmo Random Forest
#libreria  ranger
#atencion con los nombres de las carpetas
#Entrena sobre 201906, 201907, 201908, 201909
#Testea sobre  201911
#verificar que se tienen instaladas todas las LIBRERIAS necesarias


#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")
require("ranger")
require("randomForest")  #solo se usa para imputar nulos

#paquetes necesarios para la Bayesian Optimization
require("DiceKriging")
require("mlrMBO")


kexperimento <-  "30"   #cambiar esto en cada corrida !

#en estos archivos queda el resultado
kbayesiana  <-  paste0(".//work//ranger_BO_4meses_", kexperimento, ".RDATA" )
klog        <-  paste0(".//work//ranger_BO_4meses_", kexperimento, ".txt" )
ksalida     <-  paste0(".//work//ranger_BO_4meses_salida_", kexperimento, ".txt" )
ksubmit     <-  paste0(".//work//ranger_BO_4meses_submit_", kexperimento, ".csv" )
kBO_iter    <-  100  #cantidad de iteraciones de la Optimizacion Bayesiana


#------------------------------------------------------------------------------
#funcion que va a optimizar la Bayesian Optimization

estimar_ranger <- function( x )
{
  modelo  <- ranger( formula= "clase_binaria ~ .",
                     data= dtrain,  #aqui considero los 6 meses de 201906 a 201912
                     probability=   TRUE,  #para que devuelva las probabilidades
                     num.trees=     x$pnum.trees,
                     mtry=          x$pmtry,
                     min.node.size= x$pmin.node.size,
                     max.depth=     x$pmax.depth
                   )

  #aplico el modelo a testing
  prediccion_test  <- predict( modelo, dtest )

  #calculo la ganancia en los datos de testing
  ganancia_test  <- sum( (prediccion_test$predictions[ , "POS"] > 0.025) * 
                         dtest[, ifelse( clase_binaria=="POS",29250,-750)])

  #imprimo los resultados al archivo klog
  cat( file= klog, 
       append= TRUE,
       sep="",
       format(Sys.time(), "%Y%m%d %H%M%S"), "\t",
       x$pnum.trees, "\t",
       x$pmtry, "\t",
       x$pmin.node.size, "\t",
       x$pmax.depth, "\t",
       ganancia_test, "\n" )

  return( ganancia_test )
}
#------------------------------------------------------------------------------

#Aqui comienza el programa
setwd("/media/Shared/gustavo/cloud1/" )  #establezco la carpeta donde voy a trabajar
#cargo el dataset
dataset  <- fread( "./datasetsOri_ITBA/transmutado/paquete_premium_201906_202001.txt.gz", stringsAsFactors= TRUE)

#imputo los nulos, ya que ranger no acepta nulos
#Leo Breiman, ¿por que le temias a los nulos?
dataset  <-  na.roughfix( dataset )


#creo la clase_binaria
dataset[  , clase_binaria := as.factor(ifelse( clase_ternaria=="BAJA+2", "POS", "NEG" )) ]
dataset[  , clase_ternaria := NULL ]  #elimino la clase_ternaria, ya no la necesito


#genero los datasets de training y testing
dtrain <-  dataset[  foto_mes<=201909, ]   #aqui considero los meses de 201906, 201907, 201908, 201909
dtest  <-  dataset[  foto_mes==201911, ]


#escribo los titulos
if(!file.exists( klog ) )
{
  cat( file= klog, 
       append= FALSE,
       sep="",
       "fecha", "\t",
       "num.trees", "\t",
       "mtry", "\t", 
       "min.node.size", "\t",
       "max.depth", "\t",
       "gan_testing", "\n")
}


#Aqui comienza la configuracion de la Bayesian Optimization
#Aqui comienza la configuracion de la Bayesian Optimization

configureMlr(show.learner.output = FALSE)

funcion_optimizar <-  estimar_ranger  #esta funcion se debe construir

#configuro la busqueda bayesiana,  los hiperparametros que se van a optimizar
#por favor, no desesperarse por lo complejo
obj.fun  <- makeSingleObjectiveFunction(
              fn   = funcion_optimizar,
              minimize= FALSE,   #estoy Maximizando la ganancia
              noisy=    TRUE,
              par.set = makeParamSet(
                          makeIntegerParam("pnum.trees",     lower=100L, upper= 999L),
                          makeIntegerParam("pmtry",          lower=  2L, upper=  20L),
                          makeIntegerParam("pmin.node.size", lower=  1L, upper=  40L),
                          makeIntegerParam("pmax.depth",     lower=  0L, upper=  20L)),
             has.simple.signature = FALSE
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

cat( "Los parametros optimos son:","\n" )
cat( "num.trees=", run$x$pnum.trees, "\n" )
cat( "mtry=", run$x$pmtry, "\n" )
cat( "min.node.size=", run$x$pmin.node.size, "\n" )
cat( "max.depth=", run$x$max.depth, "\n" )

cat( file= ksalida, append=TRUE,"Los parametros optimos son:","\n" )
cat( file= ksalida, append=TRUE,"num.trees=", run$x$pnum.trees, "\n" )
cat( file= ksalida, append=TRUE,"mtry=", run$x$pmtry, "\n" )
cat( file= ksalida, append=TRUE,"min.node.size=", run$x$pmin.node.size, "\n" )
cat( file= ksalida, append=TRUE,"max.depth=", run$x$max.depth, "\n" )

#------------------------------------------------------------------------------
#Aqui genero el archivo que voy a entregar

modelo_final  <- ranger( formula= "clase_binaria ~ .",
                         data= dataset[ foto_mes<=201911, ]
                         probability=   TRUE,  #para que devuelva las probabilidades
                         num.trees=     run$x$pnum.trees,
                         mtry=          run$x$pmtry,
                         min.node.size= run$x$pmin.node.size,
                         max.depth=     run$x$pmax.depth
                       )

#aplico el modelo a los datos sin clase, 202001
prediccion_test  <- predict( modelo_final, dataset[ foto_mes<=202001, ] )


#genero el dataset de entrega
entrega  <- as.data.table(cbind( "numero_de_cliente"= dataset[ foto_mes==202001, numero_de_cliente],  
                                 "prob"= prediccion_final[, "POS"]) )
entrega[  ,  estimulo :=  as.integer( prob > 0.025)]

#genero el archivo de salida
fwrite( entrega[ ,  c("numero_de_cliente", "estimulo"), with=FALSE], 
        sep= ",",
        file= ksubmit )

