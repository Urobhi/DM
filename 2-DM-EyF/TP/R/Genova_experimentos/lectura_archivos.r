#Experimentos de tiempos de lectura, desde R, de archivos grandes en el bucket, usando distintas estrategias
#Antes desde la consola hice un  cd ~/buckets/b1/datasetsOri/  && gunzip -k  paquete_premium.txt.gz
require( data.table )

t0       <-  Sys.time()
dataset1 <-  fread( "~/buckets/b1/datasetsOri/paquete_premium.txt.gz", verbose=TRUE)
t1       <-  Sys.time()
delta1   <-  as.numeric( t1 - t0, units = "secs")
rm(dataset1)
gc()

t0       <-  Sys.time()
dataset2 <-  fread( "~/buckets/b1/datasetsOri/paquete_premium.txt", verbose=TRUE)
t1       <-  Sys.time()
delta2   <-  as.numeric( t1 - t0, units = "secs")
rm(dataset2)
gc()


t0       <-  Sys.time()
dataset3 <-  fread( "file:///home/gustavo_denicolay/buckets/b1/datasetsOri/paquete_premium.txt", verbose=TRUE)
t1       <-  Sys.time()
delta3   <-  as.numeric( t1 - t0, units = "secs")
rm(dataset3)
gc()

#Ahora fuerzo a un solo thread
setDTthreads(threads = 1)
t0       <-  Sys.time()
dataset4 <-  fread( "~/buckets/b1/datasetsOri/paquete_premium.txt", verbose=TRUE)
t1       <-  Sys.time()
delta4   <-  as.numeric( t1 - t0, units = "secs")
rm(dataset4)
gc()

t0       <-  Sys.time()
dataset5 <-  readRDS( "~/buckets/b1/datasetsOri/paquete_premium.rds")
t1       <-  Sys.time()
delta5   <-  as.numeric( t1 - t0, units = "secs")


cat( delta1, delta2, delta3, delta4, delta5 )

