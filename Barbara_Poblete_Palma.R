# Desarrollo Tarea Clase 4 ####

# Nombre: Bárbara Daniela Poblete Palma

# Librerías a utilizar

library(dplyr)
library(tidyverse)
library(data.table)
library(MASS)
library(ggplot2)
library(datasets)
library(feather)
library(purrr)
library(stringi)
library(tm)

# Eliminación de objetos existentes

rm(list = ls())

# Directorio

setwd("C:/Users/bdpobletep/OneDrive - Instituto Nacional de Estadisticas/Cursos R/Curso R intermedio 2023")

# Nota: Las respuestas de las preguntas del Trabajo Práctico están contenidas en los comentarios que comienzan con "R:"




# PREGUNTA 1 #### 

# Abrimos la bbdd casen 2020

casen_2020 <- read_feather("casen_2020_edit (1).feather")



# Creamos vectores que contienen los nombres de variables a seleccionar

var_ocupacion <- names(casen_2020)[str_detect(names(casen_2020), "^o\\d+")]

var_vivienda <- names(casen_2020)[str_detect(names(casen_2020), "^v\\d+")]



# Utilizamos comando select de dplyr para seleccionar las variables en base a los vectores creados previamente y crear dataframes exclusivos de ocupacion y vivienda

ocupacion <- casen_2020 %>%
  dplyr::select(all_of(var_ocupacion))

vivienda <- casen_2020 %>% 
  dplyr::select(all_of(var_vivienda))



# Dataframes de ocupacion y vivienda

ocupacion %>% head()

vivienda %>% head()



# Output tabla que contiene las variables de cada módulo (entendí que se debe crear un listado con dataframes)

var_vivienda <- c(var_vivienda, c("","","","","","")) # Agregamos espacios en blanco en el vector var_vivienda para que su dimensión cuadre con var_ocupacion

tabla <- data.frame(Variables_ocupacion = var_ocupacion, Variables_vivienda = var_vivienda) 

print(tabla)










# EJERCICIO 2 #### 

# Definición de stopwords en español

stopwords_es <- stopwords("es")



# Planteamiento de la función

clasificador <- function(data, var) {
  minusc <- str_to_lower(data[[var]]) # reemplaza todas las mayúsculas por minúsculas
  sin_punct <- str_replace_all(minusc, "[[:punct:]]", "") # remueve todos los signos de puntuación y caracteres especiales
  no_num <- str_replace_all(sin_punct, "[0-9]", "") # remueve todos los números
  
  corpus <- Corpus(VectorSource(no_num))
  corpus <- tm_map(corpus, removeWords, stopwords_es)
  
  var_modificada <- sapply(corpus, as.character) # Crea un vector chr eliminando los stopwords de la variable var
  
  data$var_modificada <- str_replace_all(var_modificada, "\\s+", " ") # reemplaza los espacios adicionales entre palabras por un solo espacio
  
  return(data)
}



# Prueba de función sobre las variables planteadas por el enunciado

ocupacion <- clasificador(ocupacion, "o9a")

ocupacion <- ocupacion %>% # Esto es para modificar el nombre de la var_modificada por el nombre de la variable correspondiente
  rename(o9a_modificada = var_modificada)



ocupacion <- clasificador(ocupacion, "o9b")

ocupacion <- ocupacion %>% 
  rename(o9b_modificada = var_modificada)



ocupacion <- clasificador(ocupacion, "o24")

ocupacion <- ocupacion %>% 
  rename(o24_modificada = var_modificada)



nuevas_var <- names(ocupacion)[str_detect(names(ocupacion), "modificada")]
print(nuevas_var) # Para ver el nombre de las nuevas variables creadas del módulo ocupación





