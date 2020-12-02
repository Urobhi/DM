#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")
require("rpart")

setwd("~/buckets/b2/" )   #establezco la carpeta donde voy a trabajar
#cargo el dataset
dataset <- fread( "./datasetsOri/paquete_premium_201906_202005.txt.gz")


#dejo la clase binaria
dataset[ , clase_binaria:= ifelse( clase_ternaria=="BAJA+2", "POS","NEG" ) ]
dataset[ , clase_ternaria:= NULL ]


#genero el modelo
modelo_original  <- rpart(formula= "clase_binaria ~ .",  
                          data= dataset[foto_mes==201906,], 
                          model= TRUE, #quiero que me devuelva el modelo
                          xval= 0,
                          cp= 0,
                          minsplit=   2,
                          minbucket=  2,
                          maxdepth=  30
                         )

#las complejidades por las que corto el arbol
complejidades <-  sort(unique( modelo_original$frame$complexity ))

tb_resultados  <- data.table(  cp=numeric(), gan_201906=numeric(), gan_201908=numeric())


for( vcp  in complejidades )
{

  #genero el modelo
  modelo_pruned <- prune(  modelo_original, vcp )


  prediccion_201906  <- predict( modelo_pruned, dataset[foto_mes==201906,], type = "prob")
  prediccion_201908  <- predict( modelo_pruned, dataset[foto_mes==201908,], type = "prob")

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

  tb_resultados  <- rbind(  tb_resultados,  list( vcp, ganancia_201906, ganancia_201908 ) )

  cat( "cp=", vcp, "train=", ganancia_201906, "test=", ganancia_201908, "\n" )
}

#Imprimo los resultados

jpeg(file = "./work/overfitting_cp.jpg",  width = 10, height = 8, units = 'in', res = 200)

plot( x= tb_resultados$cp,
      y= tb_resultados$gan_201906,
      type= "l",
      lwd= 4,
      col= "blue", 
      main= "arbol entrenado en 201906",
      xlab= "cp",
      ylab= "Ganancia",
      pch= 19,
      yaxs= "i",
      xaxs= "i",
      panel.first=grid() )



lines( x= tb_resultados$cp,
       y= tb_resultados$gan_201908, 
       type="o" , col="red", lwd=4, pch=21 )

abline( v=c(0.000880088), col=c("darkgreen"), lty=1, lwd=3 )


text( 0.00065,  14e6, "Over",  col="darkgreen", cex=1.5 )
text( 0.00065,  13e6, "Fitting",  col="darkgreen", cex=1.5 )

text( 0.0015, 14e6, "Under",  col="darkgreen", cex=1.5 )
text( 0.0015, 13e6, "Fitting",  col="darkgreen", cex=1.5 )


legend(  0.0020, 10e6, 
         legend=c("201906", "201908"),
         col=c("blue", "red"), 
         lty=1, lwd=2, cex=1 )


dev.off()
