
ftabla_reglas <- function (pmodelo, pdatos, ptarget = "clase_ternaria") {
  
  # Tomamos la columna con el target
  target_vector <- pdatos[, get(ptarget)]
  # Tomamos las clases de nuestro target
  classes <- unique(target_vector)
  
  # Tomamos las posicion de las reglas que aplican a los registro de nuestro ds,
  # y obtenemos las reglas
  
  row_leaf <- unique(pmodelo$where)
  row_name_leaf <- as.integer(rownames(pmodelo$frame[row_leaf,]))
  rules <- path.rpart(pmodelo, row_name_leaf, pretty = 0, print.it=FALSE)
  rules_concat <- lapply(rules, 
                         function(y) paste0(tail(y, n=-1), collapse = " & "))
  leaves <- data.table(row_frame = row_leaf, 
                       rules = rules_concat)
  setkey(leaves,row_frame)
  
  # Relacion target ~ hojas
  leaves_target <- dcast(
    
    data.table(
      target=target_vector, 
      leaf = pmodelo$where), 
    
    leaf ~ target, length, 
    value.var = "target")
  
  setkey(leaves_target, leaf) 
  
  # Juntamos todo
  leaves_target <- leaves_target[leaves,nomatch=0]
  
  # Sumamos algunas columnas calculadas
  colnames(leaves_target[,classes,with=FALSE])[apply(leaves_target[,classes,with=FALSE],1,which.max)]
  # Clase mayoritaria
  leaves_target[, 
                y:=colnames(
                  leaves_target[,classes,with=FALSE]
                )[
                  apply(leaves_target[,classes,with=FALSE],1,which.max)]
  ]
  
  # Cantidad clase mayoritaria
  leaves_target[, y_n:=unlist(apply(leaves_target[,classes,with=FALSE],1,max))]
  # Cantidad de elementos de la hoja
  leaves_target[, n := unlist(Reduce(function(a,b) Map(`+`,a,b), .SD)), .SDcols=classes]
  # Perdida
  leaves_target[, loss := n - y_n]
  
  # Return
  leaves_target
}