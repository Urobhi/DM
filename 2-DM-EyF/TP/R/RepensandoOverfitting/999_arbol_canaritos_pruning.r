#limpio la memoria
rm( list=ls() )
gc()

require("data.table")
require("rpart")
require("rpart.plot")

setwd("~/buckets/b2/" )   #establezco la carpeta donde voy a trabajar
#cargo el dataset
dataset <- fread( "./datasetsOri/paquete_premium_201906_202005.txt.gz")


#dejo la clase binaria
dataset[ , clase_binaria:= ifelse( clase_ternaria=="BAJA+2", "POS","NEG" ) ]
dataset[ , clase_ternaria:= NULL ]

#uso esta semilla para los canaritos
set.seed(10219)

modelo_optimo  <- rpart(formula= "clase_binaria ~ .",
                        data= dataset[foto_mes==201906,],
                        xval= 0,
                        model= TRUE,
                        cp= 0.0002981249,
                        maxdepth= 25,
                        minsplit=  192,
                        minbucket= 0 )

#cuento cuantas hojas tiene el arbol  modelo_optimo
sum( modelo_optimo$frame$var == "<leaf>"  )

#calculo la profundidad 
max(rpart:::tree.depth(as.numeric(rownames(modelo_optimo$frame))))

prediccion      <- predict( modelo_optimo, dataset[foto_mes==201908,], type = "prob")[,"POS"]
ganancia_optimo_201908 <-  sum(  (prediccion>0.025) * dataset[foto_mes==201908, ifelse( clase_binaria=="POS",29250,-750)])



#agrego canaritos
for( i in 1:20 )  dataset[ , paste0("canarito", i ) :=  runif( nrow(dataset))]


#Genero un arbol sin limite
modelo_original  <- rpart(formula= "clase_binaria ~ .",
                          data= dataset[foto_mes==201906,],
                          xval= 0,
                          model= TRUE,
                          cp= 0.0,
                          maxdepth= 30,
                          minsplit=  3,
                          minbucket= 0 )

#cuento cuantas hojas tiene el arbol  modelo_original
sum( modelo_original$frame$var == "<leaf>"  )

#calculo la profundidad 
max(rpart:::tree.depth(as.numeric(rownames(modelo_original$frame))))


#imprimo el arbol original
#pdf(file = "./work/arbol_ilimitado.pdf",  width = 120, height = 12)
#prp(modelo_original, extra=101, digits=5, branch=1, type=4, varlen=0, faclen=0)
#dev.off()


#aplico el modelo a 201908
prediccion               <- predict( modelo_original, dataset[foto_mes==201908,], type = "prob")[,"POS"]
ganancia_original_201908 <-  sum(  (prediccion>0.025) * dataset[foto_mes==201908, ifelse( clase_binaria=="POS",29250,-750)])

cat( ganancia_original_201908, "\n" )


#hago el pruning de los canaritos
#haciendo un hackeo a la estructura  modelo_original$frame
# -100 es un valor arbritrariamente negativo que jamas es generado por rpart
modelo_original$frame[ modelo_original$frame$var %like% "canarito", "complexity"] <- -100
modelo_pruned <- prune(  modelo_original, -100.0 )

#cuento cuantas hojas tiene el arbol  modelo_pruned
sum( modelo_pruned$frame$var == "<leaf>"  )
#calculo la profundidad 
max(rpart:::tree.depth(as.numeric(rownames(modelo_pruned$frame))))


#aplico el modelo a 201908
prediccion      <- predict( modelo_pruned, dataset[foto_mes==201908,], type = "prob")[,"POS"]
ganancia_pruned_201908 <-  sum(  (prediccion>0.025) * dataset[foto_mes==201908, ifelse( clase_binaria=="POS",29250,-750)])

cat( ganancia_pruned_201908, "\n" )

pdf(file = "./work/arbol_pruned.pdf",  width = 60, height = 12)
prp(modelo_pruned, extra=101, digits=5, branch=1, type=4, varlen=0, faclen=0)
dev.off()
