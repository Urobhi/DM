#Ejercicio 1: Calcular la media y mediana del vector x y el número de valores que están por debajo de la media y de la mediana, siendo x=c(1,5,7,9,3,5,6,2,4,7,5,6,9,8,6,2,6,1,4).


x <-c(1,5,7,9,3,5,6,2,4,7,5,6,9,8,6,2,6,1,4)
mean(x)
median(x)
length(x[x<mean(x)&x<median(x)])

#Ejercicio 2: Escribir la función que calcula el módulo de un número real.

Modulo <- function(x)
{
  if ( x<0) {return(-x)} else{return(x)}
}


#Ejercicio 3: Usar for para hallar el resultado de dividir de manera consecutiva el número 1111 por los siguientes divisores (en este orden): 2, 3, 4, 5, 6.

a=1111
for (i in 2:6 )
{
  a = a/i
}

#Ejercicio 4: Escribir una función que responda el signo del producto de dos factores dado, es decir "Positivo", o "Negativo", y en el caso que el producto sea 0 devuelva "Nulo".

ej4 <- function(x,y)
{
  if (x*y < 0) {return("Negativo")} else {if(x*y>0) {return("Positivo")}else{return("Nulo")}}
}
