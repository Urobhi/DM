#Este script es para descubrir daños en el dataset
#hay meses, que algunas variables estan "pisadas" con CEROS


#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")


medianonula <- function( x )
{
  return(  mean( x, na.rm=TRUE ) )
}


ceros_prevalencia <- function( x )
{
  return( sum( as.integer(x==0), na.rm=TRUE ) / length( x ) )
}


na_prevalencia <- function( x )
{
  return( sum( as.integer(is.na(x)) ) / length( x ) )
}

divide <- function( x, y )
{
  z <- ifelse( y==0, 1, y )
  return( x/z )
}

#Aqui comienza el programa
setwd("~/buckets/b1")

#cargo el dataset donde voy a entrenar
dataset <- fread("./datasetsOri/paquete_premium.txt.gz")


campos_buenos <- setdiff(  colnames( dataset) ,"clase_ternaria" )


#------------------------------------------------------------------------------
#Veo si me pisaron con NULOS

dnas  <- dataset[ , lapply(.SD, na_prevalencia), by=foto_mes, .SDcols=campos_buenos ]
setorder( dnas, foto_mes )
dnas_median  <- dnas[ , lapply(.SD, median)]

dnas2 <- as.data.table( mapply( divide, 
                                dnas[, !"foto_mes", with=FALSE], 
                                dnas_median[, !"foto_mes", with=FALSE ]
                              ))

dnas2[ , foto_mes:= dnas$foto_mes ]

dfinal <-  as.data.table( melt(dnas2, id.vars = "foto_mes") )
setorderv(  dfinal, c("foto_mes","value"), c(1,-1) )

dfinal[ foto_mes==201908, ][1:20]
dfinal[ foto_mes==201910, ][1:20]


#------------------------------------------------------------------------------
#Veo si me pisaron con ceros el campo

dceros <- dataset[ , lapply(.SD, ceros_prevalencia), by=foto_mes, .SDcols=campos_buenos]
setorder( dceros, foto_mes )
dceros_median  <- dceros[ , lapply(.SD, median)]

dceros2 <- as.data.table( mapply( divide, 
                                  dceros[, !"foto_mes", with=FALSE], 
                                  dceros_median[, !"foto_mes", with=FALSE ]
                                ))

dceros2[ , foto_mes:= dceros$foto_mes ]

dfinal <-  as.data.table( melt(dceros2, id.vars = "foto_mes") )
setorderv(  dfinal, c("foto_mes","value"), c(1,-1) )

dfinal[ foto_mes==201905, ][1:20]
dfinal[ foto_mes==201908, ][1:20]
dfinal[ foto_mes==201910, ][1:20]

dfinal[ foto_mes==201910, ][130:155]

#------------------------------------------------------------------------------

dmean <- dataset[foto_mes>=201903 , lapply(.SD, medianonula), by=foto_mes, .SDcols=campos_buenos]
setorder( dmean, foto_mes )
dmean_median  <- dmean[ , lapply(.SD, median)]

dmean2 <- as.data.table( mapply( divide, 
                                  dmean[, !"foto_mes", with=FALSE], 
                                  dmean_median[, !"foto_mes", with=FALSE ]
                                ))

dmean2[ , foto_mes:= dmean$foto_mes ]

dfinal <-  as.data.table( melt(dmean2, id.vars = "foto_mes") )
setorderv(  dfinal, c("foto_mes","value"), c(1,-1) )

dfinal[ foto_mes==201908, ][1:20]
dfinal[ foto_mes==201910, ][1:20]