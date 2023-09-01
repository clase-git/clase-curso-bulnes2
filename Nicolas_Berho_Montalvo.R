
library(tm)
library(feather)
library(dplyr)
library(magrittr)
library(stringr)

casen <- read_feather('~/R/datasets/casen_2020_edit (1).feather')


# Ejercicio 1

names(casen)
ocup <- casen[,str_detect(names(casen), '^(o.|ram|act)')]
viv <- casen[,str_detect(names(casen), '^(v\\d|hac|nh|ind|ten)')]


# Ejercicio 2

stopwords_regex <- paste(stopwords('es'), collapse = '\\b|\\b')


limpiar_texto <- function(texto){
  
  texto <- str_to_lower(texto)
  texto <- str_remove_all(texto, '[[:punct:]]')
  texto <- str_remove_all(texto, '\\d')
  texto <- str_replace_all(texto, stopwords_regex, ' ')
  texto <- str_replace_all(texto, ' +', ' ')
  
  return(texto)
}


df_test <- data.frame(
  o9a = limpiar_texto(casen$o9a),
  o9b = limpiar_texto(casen$o9b),
  o24 = limpiar_texto(casen$o24)
)

