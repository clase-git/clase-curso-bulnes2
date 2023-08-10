
# Cargamos librerias ------------------------------------------------------
library(tidyverse)
library(janitor)                       # limpieza de datos
library(stringr)                       # trabajar con cadenas de caracteres

# Ejercicio 1 -------------------------------------------------------------

# Creamos funcion para calcular el coeficiente de variación
get_cv <- function(p1,p2 = F){
  
  cv <- sd(p1, na.rm = p2) / (mean(p1, na.rm = p2))
  
  return(cv)
}

# Revisamos funcion 
vector <- c(12, 90, NA, 8, NA, -61)   # se crea vector de prueba
get_cv(vector)                        # se observa resultado con 'FALSE' por defecto
get_cv(p1 = vector, p2 = T)           # se observa resultado modificando la opción que se encontraba por defecto



# Ejercicio 2 -------------------------------------------------------------

# Creamos funcion para construir dirección
build_address <- function(street, number, apartment){
  
  street <- base::chartr('áéíóúñ','aeioun', street)
    
  street <- street %>% 
    stringr::str_to_lower() %>% 
    stringr::str_remove_all("[:digit:]+|[:punct:]+|avenida|calle|pasaje|villa|poblacion|^av") %>% 
    stringr::str_squish()
  
  number <- number %>% stringr::str_extract(pattern = "[:digit:]+")
  
  apartment <- apartment %>% stringr::str_extract(pattern = "[:digit:]+")
  
  resultado <- base::paste0(street, " ", number, ", depto. ", apartment)
  
  resultado <- resultado %>% stringr::str_replace(pattern=", depto. NA", replacement="")
  
  return(resultado)
}



# Ejercicio 3 -------------------------------------------------------------

# Se crea dataframe
df <- dplyr::tribble(~calle,              ~numero,      ~depto,
              "calle Hemingway",   "num 345",    "depto. 345",
              "av. Albert Camus",  "número 123", "123",
              "Manuel Rojas",      "234",        "departamento 231",
              "Nicanor Parra",     "678",        NULL) 

df 

# Se aplica formato texto a cada columna y crea nueva columna llamada dirección
df <- df %>%
  dplyr::mutate(calle = as.character(calle),
                numero = as.character(numero),
                depto = as.character(depto),
                direccion = build_address(street = calle, number = numero, apartment = depto))

df

