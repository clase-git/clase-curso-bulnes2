library(guaguas)
library(tidyverse)
library(stringr)
library(purrr)
library(feather)
library(tidyverse)
library(tidyr)
library(tm)

casen<-read_feather("casen_2020_edit (1).feather")

#Ejercicio 1
#Generar una lista con todos los nombres de las columnas 
lista_columnas<-list()

for (i in names(casen)) {
  if (str_count(i)>0){
    lista_columnas<-c(lista_columnas,i)
  }
}
#generar un dataframe de la lista resultante 
nueva_tabla <- data.frame(Columna1 = unlist(lista_columnas))
casen_ordenada<-nueva_tabla %>% 
  mutate(ocupacion=ifelse(str_detect(Columna1, pattern = "^o"),Columna1,"-"),#se asume q las terminadas en 'o' son de ocupación
         vivienda=ifelse(str_detect(Columna1, pattern = "^v"),Columna1,"-")) %>% #se asume que las terminadas en 'v' son de vivienda
  select(ocupacion,vivienda)

#Ejercicio 2

funcion_modificar_glosa <- function(variable) {
  variable <- str_to_lower(variable) %>%
    str_replace_all(pattern = "[[:punct:]@#]|\\d+\\s{2,}", replacement = "")
  
  return(variable)
}