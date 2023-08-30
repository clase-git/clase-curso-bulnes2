library(feather)
library(tidyverse)
library(stringr)
library(tm)
casen = read_feather("data/casen_2020_edit (1).feather")

#1

Modulo_viviendas <- select(casen,matches("^v\\d*\\w*"))

Modulo_ocupación <- select(casen,matches("^o\\d*\\w*"))

#2 

stopwords = paste(stopwords('es'), collapse = '\\b|\\b')
stopwords = paste0('\\b', stopwords, '\\b')

Insumo_Clasificador <- function(data,svar) {
  data %>% 
    mutate({{svar}} := str_to_lower({{svar}}) %>% 
             str_replace_all(pattern = "[:punct:]","") %>% 
             str_replace_all(pattern = "\\s\\d+","") %>% 
             str_replace_all(stopwords,""))
}