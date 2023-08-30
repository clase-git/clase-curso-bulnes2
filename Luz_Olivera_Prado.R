install.packages("dplyr")
install.packages("feather")
install.packages("arrow")
install.packages("tm")

library(feather)
library(tidyverse)
library(arrow)
library(stringr)
library(tm)

# Ejercicio_1
#A partir de la base de datos de la encuesta Casen 2020, disponible en la carpeta de la clase de hoy "casen_2020_edit.feather", realice las siguientes tareas

#library(feather)
#ibrary(tidyverse)
#casen = read_feather("data/casen_2020_edit.feather")
#1- Ayudándote de expresiones regulares, y de la manera más suscinta que puedas, selecciona todas las variables de cuestionario de:
#  - el módulo de Ocupación
#- el módulo de vivienda
#El output debe ser una tabla solo con las variables de cada módulo, por separado.
#nota: puedes omitir las variables construidas ex-post en cada módulo.


casen_2020 <- read_feather("casen_2020_edit.feather")

ocupacion <- casen_2020[str_detect(colnames(casen_2020),"^o\\w|^ra\\w")] 
vivienda <- casen_2020[str_detect(colnames(casen_2020),"^v\\w")]
#Ejercicio 2
#La tabla "casen_2020_edit.feather" contiene las variables o9a, o9b y o24, que representan, respectivamente, "ocupación", "tareas en la ocupación" y "rama de actividad económica" de las personas que declaran estar ocupadas en el periodo de referencia.
#Estas variables son utilizadas como insumo para la codificación automática de los clasificadores de ocupación y rama de actividad económica.
#Crea una función que reciba como argumento una variable character y la procese de la siguiente manera:
# - Pase todos los caracteres de una glosa a minúscula
#- Remueva todos los signos de puntuación y caracteres especiales
#- Remueva todos los números
#- Extraiga espacios adicionales entre palabras
#- Remueva *stopwords* (para esto pueden usar las librerías `tm`, `quanteda`, entre otras)
#La función debe retornar una variable con glosas procesadas.
#No es necesario que evalúes tu función, basta con el código

limpieza_variables <- function(base,variable){ 
  base %>%
   select(variable)%>%
    str_to_lower(variable)%>%
    str_remove_all("[[:punct:]]&\\s+\\d+")
  
   }

limpieza_variables(ocupacion,"o9a")
