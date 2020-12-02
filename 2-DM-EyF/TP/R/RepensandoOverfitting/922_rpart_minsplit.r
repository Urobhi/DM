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

tb_resultados  <- data.table(  minsplit=numeric(), gan_201906=numeric(), gan_201908=numeric())

for( vminsplit  in c(1,2,3,4,5,10,20,30,40,50,60,70,80,90,100, 150,200,250,300,400) )
{

  #genero el modelo
  modelo  <- rpart(formula= "clase_binaria ~ .",  
                   data= dataset[foto_mes==201906,], 
                   model= TRUE, #quiero que me devuelva el modelo
                   xval= 0,
                   cp= 0,
                   minsplit= vminsplit,
                   maxdepth=  30
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
  entrega_201908[  , estimulo :=  as.integer( prob > 0.025)]

  ganancia_201906  <- sum(  entrega_201906[ estimulo==1, ifelse( clase_binaria=="POS", 29250, -750) ])
  ganancia_201908  <- sum(  entrega_201908[ estimulo==1, ifelse( clase_binaria=="POS", 29250, -750) ])

  tb_resultados  <- rbind(  tb_resultados,  list( vminsplit, ganancia_201906, ganancia_201908 ) )

  cat( "minsplit=", vminsplit, "train=", ganancia_201906, "test=", ganancia_201908, "\n" )
}

#Imprimo los resultados

jpeg(file = ".\\work\\overfitting_minsplit.jpg",  width = 10, height = 8, units = 'in', res = 200)

plot( x= tb_resultados$minsplit,
      y= tb_resultados$gan_201906,
      type= "l",
      lwd= 4,
      col= "blue", 
      main= "Overfitting en accion, arbol entrenado en 201906",
      xlab= "minsplit",
      ylab= "Ganancia",
      ylim= c(0, max(tb_resultados$gan_201906) ),
      pch= 19,
      yaxs= "i",
      xaxs= "i",
      panel.first=grid() )



lines( x= tb_resultados$minsplit,
       y= tb_resultados$gan_201908, 
       type="o" , col="red", lwd=4, pch=21 )

abline( v=c(20), col=c("darkgreen"), lty=1, lwd=3 )


text( 80, 14e6, "Under",  col="darkgreen", cex=1.5 )
text( 80, 12e6, "Fitting",  col="darkgreen", cex=1.5 )


legend(  300, 10e6, 
         legend=c("201906", "201908"),
         col=c("blue", "red"), 
         lty=1, lwd=2, cex=1 )


dev.off()
