#construye un modelo aditivo
#utilizando XGBoost con  Decision Stumps ( arboles de altura 1 )
#sale un modelo "lineal"   GLOBAL

#limpio la memoria
rm( list=ls() )
gc()

require("data.table")
require("Matrix")
require("xgboost")
require("DiagrammeR")


fganancia_logistic_xgboost   <- function(probs, clases) 
{

  vlabels <- getinfo(clases, "label")

  gan <-sum(   (probs > 0.025  ) * 
                ifelse( vlabels== 1, +29250, -750 )   
           )

  return(  list(metric = "ganancia", value =  ifelse(  is.na(gan) , 0, gan ) )  )
}
#------------------------------------------------------------------------------

setwd("M:")

dataset  <- fread( ".\\datasets\\Fase I\\paquete_premium_201701_201910.txt")
dataset  <- dataset[  foto_mes>=201909 , ]
gc()


dataset[  , clase01  := ifelse( clase_ternaria=="BAJA+2", 1, 0) ]
dataset[  , clase_ternaria := NULL ]

campos_lags  <-  setdiff( colnames(dataset),  c("clase_ternaria", "clase01", "numero_de_cliente") )

setorderv( dataset, c("numero_de_cliente","foto_mes") )
dataset[,  paste0( campos_lags, "_lag1") := shift(.SD, 1, NA, "lag"), by=numero_de_cliente, .SDcols= campos_lags ]

dataset[,  paste0( campos_lags, "_delta1") :=  .SD - get(paste0( campos_lags, "_lag1")), .SDcols= campos_lags ]



#Creo los canaritos
set.seed( 13 )
for( i in 1:20 ) dataset[ , paste0( "canarito", i) :=  runif( nrow(dataset) ) ]


col_buenas <- setdiff(colnames(dataset) ,c("clase01") )

dtrain <- xgb.DMatrix( data=  as.matrix(dataset[ foto_mes==201910 , col_buenas, with=FALSE]),
                       label= dataset[ foto_mes==201910 , clase01 ]
                     )

#Primero determino la cantidad optima de arboles

modelocv <-  xgb.cv(data= dtrain,
                    objective= "binary:logistic",
                    feval= fganancia_logistic_xgboost,
                    maximize= TRUE,
                    nfold= 5,
                    stratified= TRUE,
                    base_score= 0.025, #Prob de corte del PROBLEMA
                    nrounds= 99999,  #un numero muy grande
                    early_stopping_rounds= 100,
                    max_depth= 1,   #Fundamental
                    eta= 0.1,
                    tree_method= "hist",
                    max_bin= 255,
                    nthread= 4
                   )

modelocv$best_iter

#uso hiperparametros que supuestamente optimice en una bayesiana, pero con max_depth=1
modelo <-  xgb.train(data= dtrain,
                     objective= "binary:logistic",
                     base_score= 0.025, #intencional seteo
                     nrounds= modelocv$best_iter,
                     max_depth= 1,   #Fundamental
                     eta= 0.1,
                     tree_method= "hist",
                     max_bin= 255
                    )


tb_importancia <-  as.data.table( xgb.importance( model=modelo ) )




#Agregado nuevo
#tabla con estructura de arboles
m <- xgb.model.dt.tree( model= modelo, feature_names= col_buenas )

tbl <- m[ Node==0, ]
tbl[ m[Node==1], on="Tree", score1:= i.Quality  ]
tbl[ m[Node==2], on="Tree", score2:= i.Quality  ]

tbl[ , scoreNA := ifelse( substr(Missing, nchar(Missing), nchar(Missing))=="1", score1, score2 ) ]

score_get <- function( pFeature, pvalor )
{
   return(  sum( tbl[ Feature==pFeature & Split < pvalor,  score2]) +
           sum( tbl[ Feature==pFeature & pvalor <= Split, score1]) 
        )
}


tbl2 <-  unique(  tbl[ , c("Feature","Split") ] )
setorderv( tbl2, c("Feature","Split"), c(1,1) )
tbl2[   , delta := max(Split,na.rm=TRUE)-min(Split,na.rm=TRUE) , by=Feature ]

tbl3 <-  tbl2[ ,   list( "Split" = min(Split) - min(ifelse( delta==0, Split, delta/10 )) ) ,  by=Feature ]
tbl4 <-  tbl2[ ,   list( "Split" = max(Split) + max(ifelse( delta==0, Split, delta/10 )) ) ,  by=Feature ]

tbl5 <- as.data.table( rbind( tbl2[ ,c("Feature","Split"), with=FALSE], tbl3, tbl4 ))

setorderv( tbl5, c("Feature","Split"), c(1,1) )

for( i in 1:nrow(tbl5) )
{
  reg <- tbl5[i] 
  tbl5[ i, score :=  score_get( reg$Feature, reg$Split) ]
}

tbl5[  Feature=="mcuentas_saldo"  ]
tbl5[ , delta :=  max(score) - min(score) , by=Feature ]
setorderv(  tbl5, c("delta", "Feature","Split"), c(-1,1,1) )

tblNA <- tbl[ , list( "score"=sum(scoreNA) ), by=Feature ]

options(scipen=5)

#Exporto el modelo aditivo
pdf( ".\\work\\ModeloAditivo_DELTAS_101.pdf" )

for( f in unique( tbl5$Feature  ))
{
  plot( tbl5[ Feature==f , Split ],
        tbl5[ Feature==f , score ],
        main= paste( f,  round( tbl5[ Feature==f , min(delta) ], 3)),
        xlab= f,
        ylab = "Score",
        type="S",
        col= "blue"
      )

  grid()
}
dev.off()


