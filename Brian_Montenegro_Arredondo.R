library(feather)
library(tidyverse)

# setwd("C:/Users/bdmontenegroa/Desktop/INE")

casen = read_feather("casen_2020_edit.feather")

# Parte 1
casen_vivienda <- casen %>% 
  select_if(str_detect(names(.), "^o\\d"))

casen_ocupación <- casen %>% 
  select_if(str_detect(names(.), "^v\\d"))

# Parte 2
fn_str <- function(chr_var) {
  chr_var <- as.character(chr_var)
  chr_var <- tolower(chr_var)
  chr_var <- str_replace_all(chr_var, "[[:punct:]]", "")
  chr_var <- str_replace_all(chr_var, "[0-9]", "")
  chr_var <- str_replace_all(chr_var, "\\s+", " ")
}

casen$v1 <- fn_str(casen$o9a)
casen$v2 <- fn_str(casen$o9b)
casen$v3 <- fn_str(casen$o24)


