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
set.seed(102191)

#agrego una variable canarito, random distribucion uniforme en el intervalo [0,1]
dataset[ ,  canarito1 :=  runif( nrow(dataset) ) ]

#agrego los siguientes canaritos
for( i in 2:20 ) dataset[ , paste0("canarito", i ) :=  runif( nrow(dataset)) ]

#Primero  veo como quedan mis arboles
modelo  <- rpart(formula= "clase_binaria ~ .",
                 data= dataset[foto_mes==201906,],
                 model= TRUE,
                 xval= 0,
                 cp= 0.001074819, #optimo
                 minsplit=  2,
                 minbucket= 2,
                 maxdepth= 10)

pdf(file = "./work/arbol_canaritos_cp.pdf",  width = 120, height = 12)
prp(modelo, extra=101, digits=5, branch=1, type=4, varlen=0, faclen=0)
dev.off()
