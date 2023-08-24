# TAREA 2 -----------------------------------------------------------------
# Carga de librerias ------------------------------------------------------
library(tidyverse)                     
library(feather)
library(quanteda)

# Ejercicio 1 -------------------------------------------------------------
casen <- feather::read_feather("data/casen_2020_edit (1).feather")


## 1 ----
# Ayudándote de expresiones regulares, y de la manera más suscinta que puedas, 
# selecciona todas las variables de cuestionario de:
# - el módulo de Ocupación
# - el módulo de vivienda

vivienda <- casen[stringr::str_detect(colnames(casen), pattern = "^v{1}\\d+")]          # al comienzo de la cadena se buscan las columnas que comiencen con "v" y tengan 1 o más digitos
ocupacion <- casen[stringr::str_detect(colnames(casen), pattern = "^o{1}\\w|^rama")]    # al comienzo de la cadena se buscan las columnas que comiencen con "o" y posean letras, números y guiones bajos, o bien las que comiencen con la palabra "rama" (existen 4 variables que comienzan con estos caracteres y forman parte del módulo de ocupación)

## 2 ----
# Crea una función que reciba como argumento una variable character y la procese 
# de la siguiente manera:
# - Pase todos los caracteres de una glosa a minúscula
# - Remueva todos los signos de puntuación y caracteres especiales
# - Remueva todos los números
# - Extraiga espacios adicionales entre palabras
# - Remueva *stopwords* (para esto pueden usar las librerías `tm`, `quanteda`, entre otras)

depura_caracter <- function(df, variable){
  
  df %>% dplyr::select(variable) %>%                                                                       # se selecciona la variable a procesar
    purrr::map(~.x %>%                                                                                     # .x: representa cada elemento dentro del objeto "variable"
                 quanteda::tokens(remove_punct = TRUE, remove_numbers = TRUE, remove_symbols = TRUE) %>%   # Removemos puntuación, números y simbolos
                 quanteda::tokens_tolower() %>%                                                            # Dejamos con minuscula
                 quanteda::tokens_remove(stopwords("es")))                                                 # Removemos stopword en idioma spanish
  
}

# testeamos para las primeras 30 observaciones de las variables a procesar
ocupacion_test <- ocupacion[1:30,]

depura_caracter(ocupacion_test,"o24")
depura_caracter(ocupacion_test,"o9a")
depura_caracter(ocupacion_test,"o9b")

