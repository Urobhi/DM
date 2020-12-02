#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")


#Aqui comienza el programa
setwd("~/buckets/b1")

#cargo el dataset donde voy a entrenar
dataset <- fread("./datasetsOri/paquete_premium.txt.gz")

#ordeno el dataset
setorder( dataset,  foto_mes )

campos_buenos <-  setdiff(  colnames( dataset),  c("numero_de_cliente","foto_mes","clase_ternaria" ) )


pdf("./work/densidades_11_01.pdf")
for( campo in  campos_buenos )
{
  cat( campo, "  " )
  q201911 <-  quantile(  dataset[ foto_mes==201911 , get(campo) ] , prob= c(0.05, 0.95), na.rm=TRUE )
  q202001 <-  quantile(  dataset[ foto_mes==202001 , get(campo) ] , prob= c(0.05, 0.95), na.rm=TRUE )

  a1 <- pmin( q201911[[1]], q202001[[1]] )
  a2 <- pmax( q201911[[2]], q202001[[2]] )

  densidad_201911 <- density( dataset[ foto_mes==201911, get(campo) ] , kernel="gaussian", na.rm=TRUE)
  densidad_202001 <- density( dataset[ foto_mes==202001, get(campo) ] , kernel="gaussian", na.rm=TRUE)

  plot(densidad_201911, 
       col="blue",
       xlim= c( a1, a2),
       main= paste0("Densidades 201911 2012001   ",  campo), )

  lines(densidad_202001, col="red", lty=2)
  
  legend(  "topright",  
           legend=c("201911", "202001"),
           col=c("blue", "red"), lty=c(1,2))

}
dev.off()

#------------------------------------------------------------------------------
#Densidades corregidas

fdividir_maximo  <- function( pvector )
{
  maximo <-  max( pvector, na.rm=TRUE )

  return(  pvector / maximo )
}

camposMonto <-  colnames(dataset)[  like( colnames(dataset), "^m|^Visa_m|^Master_m" ) ]

dataset[ foto_mes==201911 | foto_mes==202001, (camposMonto) := lapply( .SD, frankv,  na.last="keep", ties.method="dense" ) , by=foto_mes, .SDcols= camposMonto ]
dataset[ foto_mes==201911 | foto_mes==202001, (camposMonto) := lapply( .SD, fdividir_maximo ) , by=foto_mes, .SDcols= camposMonto ]



pdf("./work/densidades_corregidas_11_01.pdf")
for( campo in  campos_buenos )
{
  cat( campo, "  " )
  q201911 <-  quantile(  dataset[ foto_mes==201911 , get(campo) ] , prob= c(0.05, 0.95), na.rm=TRUE )
  q202001 <-  quantile(  dataset[ foto_mes==202001 , get(campo) ] , prob= c(0.05, 0.95), na.rm=TRUE )

  a1 <- pmin( q201911[[1]], q202001[[1]] )
  a2 <- pmax( q201911[[2]], q202001[[2]] )

  densidad_201911 <- density( dataset[ foto_mes==201911, get(campo) ] , kernel="gaussian", na.rm=TRUE)
  densidad_202001 <- density( dataset[ foto_mes==202001, get(campo) ] , kernel="gaussian", na.rm=TRUE)

  plot(densidad_201911, 
       col="blue",
       xlim= c( a1, a2),
       main= paste0("Densidades 201911 2012001   ",  campo), )

  lines(densidad_202001, col="red", lty=2)
  
  legend(  "topright",  
           legend=c("201911", "202001"),
           col=c("blue", "red"), lty=c(1,2))

}
dev.off()

