#limpio la mmoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")

setwd("M:\\" )  #establezco la carpeta donde voy a trabajar
dataset_grande <- fread( ".\\datasets\\paquete_premium_201906_202001.txt.gz")

dataset <-  dataset_grande[ foto_mes==201911, ]

#defino los positivos y negativos
dataset[  , pos :=  as.integer( clase_ternaria == "BAJA+2" ) ]
dataset[  , neg :=  as.integer( clase_ternaria != "BAJA+2" ) ]

POS <- sum( dataset$pos )
NEG <- sum( dataset$neg )

setorder( dataset, mcaja_ahorro )

dataset[  , pos_acum :=  cumsum( pos ) ]
dataset[  , neg_acum :=  cumsum( neg ) ]

#dibujo la Curva ROC
plot( x=dataset$neg_acum,  y=dataset$pos_acum, col="blue" )

#dibujo la diagonal
abline( a=0, b=POS/NEG )

#calculo del area bajo la curva

tb_simple <-  dataset[  ,  list( "pos_acum_min" = min(pos_acum)),  by=neg_acum ]

AUC <-  sum( tb_simple$pos_acum_min ) / (POS*NEG )


#--------------------------------------
#mejoro un poco el grafico

plot( x= dataset$neg_acum,  
      y= dataset$pos_acum, 
      main= "ROC Curve  mcaja_ahorro",
      xlab= "neg",
      ylab= "pos",
      xaxs= "i",
      yaxs= "i",
      col= "blue",
      panel.first = grid())
      )

abline( a=0,  b=POS/NEG, col="black", lwd=2 )

