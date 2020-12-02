#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")
require("rpart")

setwd("U:" )  #establezco la carpeta donde voy a trabajar
#cargo el dataset
dataset  <- fread( "U:\\datasets\\paquete_premium_201906_202001.txt.gz")

#dejo la clase binaria
dataset[ , clase_binaria:= ifelse( clase_ternaria=="BAJA+2", "POS","NEG" ) ]
dataset[ , clase_ternaria:= NULL ]

tb_resultados  <- data.table(  maxdepth=numeric(), gan_201906=numeric(), gan_201908=numeric(), gan_201908_top=numeric())

for( vmaxdepth  in 1:30 )
{

  #genero el modelo
  modelo  <- rpart(formula= "clase_binaria ~ .",  
                   data= dataset[foto_mes==201906,], 
                   model= TRUE, #quiero que me devuelva el modelo
                   xval= 0,
                   cp= 0,
                   minsplit= 5,
                   maxdepth=  vmaxdepth
                  )

  prediccion_201906  <- predict( modelo, dataset[foto_mes==201906,], type = "prob")
  prediccion_201908  <- predict( modelo, dataset[foto_mes==201908,], type = "prob")

  entrega_201906  <-   as.data.table(cbind( "numero_de_cliente" = dataset[ foto_mes==201906, numero_de_cliente],
                                           "prob" =prediccion_201906[, "POS"],
                                           "clase_binaria" = dataset[ foto_mes==201906, clase_binaria] ))

  entrega_201908 <-   as.data.table(cbind( "numero_de_cliente" = dataset[ foto_mes==201908, numero_de_cliente],
                                           "prob" =prediccion_201908[, "POS"],
                                           "clase_binaria" = dataset[ foto_mes==201908, clase_binaria] ))

  entrega_201906[  , estimulo :=  as.integer( prob > 0.025)]
  ganancia_201906  <- sum(  entrega_201906[ estimulo==1, ifelse( clase_binaria=="POS", 29250, -750) ])
  
  entrega_201908[  , estimulo :=  as.integer( prob > 0.025)]
  ganancia_201908  <- sum(  entrega_201908[ estimulo==1, ifelse( clase_binaria=="POS", 29250, -750) ])
  
  #me quedo con los 3866 registros mejores 
  entrega_201908[ , azar := runif( nrow(entrega_201908) ) ]
  setorderv(  entrega_201908,  c( "prob", "azar" ), c(-1,1) )
  entrega_201908[  , gan := ifelse( clase_binaria=="POS", 29250, -750) ]
  entrega_201908[  , gan_acum :=  cumsum(gan) ]
  ganancia_201908_top  <- max( entrega_201908$gan_acum )

  tb_resultados  <- rbind(  tb_resultados,  list( vmaxdepth, ganancia_201906, ganancia_201908, ganancia_201908_top ) )

  cat( "maxdepth=", vmaxdepth, "train=", ganancia_201906, "test=", ganancia_201908, "test_top=", ganancia_201908_top, "\n" )
}

jpeg(file = ".\\work\\overfitting_maxdepth_trampa.jpg",  width = 10, height = 8, units = 'in', res = 200)

plot( x= tb_resultados$maxdepth,
      y= tb_resultados$gan_201906,
      type= "l",
      lwd= 4,
      col= "blue", 
      main= "Overfitting en accion, arbol entrenado en 201906",
      xlab= "profundidad del arbol",
      ylab= "Ganancia",
      pch= 19,
      yaxs= "i",
      xaxs= "i",
      panel.first=grid() )



lines( x= tb_resultados$maxdepth,
       y= tb_resultados$gan_201908, 
       type="o" , col="red", lwd=4, pch=21 )

lines( x= tb_resultados$maxdepth,
       y= tb_resultados$gan_201908_top, 
       type="o" , col="magenta", lwd=4, pch=21 )


abline( v=c(9), col=c("darkgreen"), lty=1, lwd=3 )

text(  5, 14e6, "Under", col="darkgreen", cex=1.5 )
text(  5, 13e6, "Fitting", col="darkgreen", cex=1.5 )

text( 12, 14e6, "Over",  col="darkgreen", cex=1.5 )
text( 12, 13e6, "Fitting",  col="darkgreen", cex=1.5 )


legend(  22, 10e6, 
         legend=c("201906", "201908", "201908 top"),
         col=c("blue", "red", "magenta"), 
         lty=1, lwd=2, cex=1 )


dev.off()
