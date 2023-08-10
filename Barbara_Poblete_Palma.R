# Desarrollo Tarea Clase 2 ####

# Nombre: Bárbara Daniela Poblete Palma

# Librerías a utilizar

library(dplyr)
library(tidyverse)
library(data.table)
library(MASS)
library(ggplot2)
library(datasets)
library(haven)

# Eliminación de objetos existentes

rm(list = ls())

# Nota: Las respuestas de las preguntas del Trabajo Práctico están contenidas en los comentarios que comienzan con "R:"





# EJERCICIO 1 ####

# Planteamiento de la función

get_cv <- function(vector, remov_na = FALSE){
  if (remov_na) {
    vector <- vector[!is.na(vector)]
  }
  n <- length(vector)
  
  media <- mean(vector)
  desv_est <- sd(vector)
  
  cv <- round((desv_est / media), 3)
  
  return(list("Coeficiente de variación" = cv, "Tamaño" = n)) # Me genera una lista con los resultados, donde el primer elemento es el cv y el segundo el número de valores empleados en el cálculo
}


# Probando la función con un vector aleatorio

set.seed(123) # Establecemos semilla

vector_aleatorio <- runif(10) # Generamos el vector aleatorio de 10 valores

vector_aleatorio[5] <- NA # El valor NA está en la posición 5 del vector

prueba1 <- get_cv(vector_aleatorio, remov_na = FALSE) # Función que no excluye los NA en el cálculo del cv
print(prueba1)

prueba2 <- get_cv(vector_aleatorio, remov_na = TRUE) # Función que excluye los NA en el cálculo del cv
print(prueba2)



# Probamos el cálculo de la función sin emplear la función, sino que de forma manual para comparar resultados

no_funcion_1 <- round((sd(vector_aleatorio, na.rm = FALSE)/mean(vector_aleatorio, na.rm = FALSE)), 3) 

no_funcion_2 <- round((sd(vector_aleatorio, na.rm = TRUE)/mean(vector_aleatorio, na.rm = TRUE)), 3) 



# Comparamos resultados

print(ifelse(is.na(prueba1[[1]]) & is.na(no_funcion_1), "La función está correctamente formulada", "Hay que modificar la función"))

print(ifelse(prueba2[[1]]==no_funcion_2, "La función está correctamente formulada", "Hay que modificar la función"))



# R: Vemos que los resultados cuadran, por lo que la función está correctamente formulada.





# PREGUNTA 2 ####

# Planteamiento de la función

build_address <- function(street, number, apartment){
  street <- sub(".*(calle|avenida|av|av.|pasaje)\\s+", "", street)
  number <- as.numeric(gsub("[^0-9]", "", number))
  apartment <- as.numeric(gsub("[^0-9]", "", apartment))
  apartment <- ifelse(!is.na(apartment), paste0(", depto.", " ", apartment), "")
  address <- paste0(street, " ", number, apartment)
  return(address)
}



# Probando la función

street1 <- "pasaje Cerro Provincia"
number1 <- "num 813"
apartment1 <- 5

direccion1 <- build_address(street1, number1, apartment1) 

street2 <- "av. Américo Vespucio"
number2 <- "345"
apartment2 <- "depto 10"

direccion2 <- build_address(street2, number2, apartment2)



# R: Para los casos probados, la función funciona correctamente





# EJERCICIO 3 ####

# Creamos el dataframe del enunciado

df <- tribble(~calle,              ~numero,      ~depto,
              "calle Hemingway",   "num 345",    "depto. 345",
              "av. Albert Camus",  "número 123", "123",
              "Manuel Rojas",      "234",        "departamento 231",
              "Nicanor Parra",     "678",        NULL
) 
df



# Probando la función build_address en el dataframe del enunciado

df <- df %>% 
  mutate(dirección = build_address(street = calle, number = numero, apartment = depto))

print(df)

# R: Vemos que la función reporta la dirección con el formato propuesto en el enunciado del ejercicio 2

