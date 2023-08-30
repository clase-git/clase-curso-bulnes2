#### TAREA 3 ####

library(dplyr)
library(feather)
library(tidyverse)
library(stringr)

casen = read_feather("Tarea3/casen_2020_edit.feather")

#1- Ayudándote de expresiones regulares, y de la manera más suscinta que puedas, selecciona todas las variables de cuestionario de:

#- el módulo de Ocupación
#- el módulo de vivienda

#El output debe ser una tabla solo con las variables de cada módulo, por separado.

variables <- variable.names(casen) 
variables <- as.data.frame(variables)

Variables_Ocupación <- variables %>% 
  filter(str_detect(variables, ("^o\\w|^ra\\w")))

Variables_Vivienda <- variables %>% 
  filter(str_detect(variables, ("^v\\d")))

Ocup <- casen[str_detect(colnames(casen), pattern = "^o\\w|^ra\\w")]

Viv <- casen[str_detect(colnames(casen), pattern = "^v\\d")]


#---

#2- La tabla "casen_2020_edit.feather" contiene las variables o9a, o9b y o24, que representan, respectivamente, "ocupación", "tareas en la ocupación" y "rama de actividad económica" de las personas que declaran estar ocupadas en el periodo de referencia.
#Estas variables son utilizadas como insumo para la codificación automática de los clasificadores de ocupación y rama de actividad económica.
#Crea una función que reciba como argumento una variable character y la procese de la siguiente manera:
#- Pase todos los caracteres de una glosa a minúscula
#- Remueva todos los signos de puntuación y caracteres especiales
#- Remueva todos los números
#- Extraiga espacios adicionales entre palabras
#- Remueva *stopwords* (para esto pueden usar las librerías `tm`, `quanteda`, entre otras)

limpieza <- function(data, var) {
  data %>% 
    select(all_of(var)) %>% 
    str_to_lower() %>% 
    str_remove_all("[[:punct:]]") %>% 
    str_remove_all("\\d") %>% 
    str_trim() #remueve los espacios entre palabras
}

limpieza(Ocup,"o9a")
limpieza(Ocup,"o9b")
limpieza(Ocup,"o24")

#-----------------------------

