
# Paquetería --------------------------------------------------------------

# install.packages("tidyverse")
# install.packages("dplyr")
# install.packages("stringr")
# install.packages("stringi")
# install.packages("guaguas")
# install.packages("feather")
# install.packages("tm") #stopwords
# install.packages("NLP") 
# install.packages("quanteda") #stopwords


# librerias ---------------------------------------------------------------

library(tidyverse)
library(dplyr)
library(stringr)
library(stringi)
library(guaguas)
library(feather)
library(NLP)
library(quanteda)
library(tm)



# Ejercicio 1 -------------------------------------------------------------


casen = read_feather("casen_2020_edit.feather")

names(casen)


variables_select <- casen %>%
  select(matches("^o[1-9]$|^o[1-2][0-9]$|^o3[0-6]$|^v[1-9]$|^v[1-2][0-9]$"))



# Ejercicio 2 -------------------------------------------------------------

process_glossa <- function(input_string) {
  processed_text <- tolower(input_string) # Pase a minúsculas
  processed_text <- gsub("[[:punct:]]", " ", processed_text) # Remover signos de puntuación y caracteres especiales
  processed_text <- gsub("[[:digit:]]", "", processed_text) # Remover números
  processed_text <- gsub("\\s+", " ", processed_text) # Extraer espacios adicionales entre palabras
  
  # Crear un corpus temporal y cargar las stopwords en español
  my_corpus <- Corpus(VectorSource(processed_text))
  my_stopwords <- tm::stopwords("es")
  
  # Aplicar el proceso de eliminación de stopwords
  my_corpus <- tm_map(my_corpus, content_transformer(tolower))
  my_corpus <- tm_map(my_corpus, removePunctuation)
  my_corpus <- tm_map(my_corpus, removeNumbers)
  my_corpus <- tm_map(my_corpus, removeWords, my_stopwords)
  
  # Unir el corpus procesado en una sola cadena
  processed_text <- unlist(sapply(my_corpus, `[`))
  
  return(processed_text)
}

# Usos para o9a, o9b y o24
(glosa_limpia_o9a <- process_glossa(casen$o9a))
(glosa_limpia_o9b <- process_glossa(casen$o9b))
(glosa_limpia_o24 <- process_glossa(casen$o24))


print(glosa_limpia_o9a)
print(glosa_limpia_o9b)
print(glosa_limpia_o24)


