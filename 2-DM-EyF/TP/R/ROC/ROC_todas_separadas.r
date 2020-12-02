#limpio la mmoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")

setwd("M:\\" )  #establezco la carpeta donde voy a trabajar
dataset_grande <- fread( ".\\datasets\\paquete_premium_201906_202001.txt.gz")

dataset <-  dataset_grande[ foto_mes==201911, ]

dataset[  , azar := runif( nrow(dataset) ) ]

#defino los positivos y negativos
dataset[  , pos :=  as.integer( clase_ternaria == "BAJA+2" ) ]
dataset[  , neg :=  as.integer( clase_ternaria != "BAJA+2" ) ]

POS <- sum( dataset$pos )
NEG <- sum( dataset$neg )


#recorro todos los campos MENOS  la clase
campos <-  setdiff( colnames( dataset ), c("clase_ternaria", "pos","neg") )


for(  campo  in campos )
{

  setorderv( dataset, c(campo,"azar"), c(1,1) )

  dataset[  , pos_acum :=  cumsum( pos ) ]
  dataset[  , neg_acum :=  cumsum( neg ) ]

  #calculo del area bajo la curva
  tb_simple <-  dataset[  ,  list( "pos_acum_min" = min(pos_acum)),  by=neg_acum ]
  AUC <-  sum( tb_simple$pos_acum_min ) / (POS*NEG )

  jpeg( paste0( ".//work//",campo,".jpg"), 
        width =  768, 
        height = 768)
  
  plot( x= dataset$neg_acum,
        y= dataset$pos_acum,
        main= paste0( "ROC Curve  ", campo, " AUC=", round(AUC,4) ),
        xlab= "neg",
        ylab= "pos",
        xaxs= "i",
        yaxs= "i",
        xlim= c(0, NEG ),
        ylim= c(0, POS ),
        col= "blue",
        panel.first = grid()
      )
  
  #la diagonal
  abline( a=0,  b=POS/NEG, col="black", lwd=2 )
  
  dev.off()

}

