library(tidyverse)

# Ejercicio 1

# Construye una función llamada get_cv que calcule el coeficiente de variación de un vector. 
#get_cv debe recibir 2 parámetros:

# un vector de tamaño n
# un valor boolean que indique si los valores NA deben ser removidos. Por defecto, 
#la función no debe remover dichos valores.
# En la construcción de la función no está permitido utilizar la función cv, pero sí mean y sd

# La fórmula para calcular el coeficiente de variación es cv = sd/promedio

get_cv <- function(x, na.rm = T) {
  cv <-  (sd(x) / mean(x))
  return(cv)
}

set.seed(123)
vector <- rnorm(n = 10000)
get_cv(vector)

# Ejercicio 2

# Crea una función llamada build_address que construya una dirección en base a algunos inputs, 
#siguiendo un formato establecido. build_address recibe 2 parámetros obligatorios y uno opcional.

# Parámetros obligatorios:

# calle: string que contiene el nombre de la calle
# numero: string que contiene el número de la dirección
# El parámetro opcional es depto y contiene el número del departamento.

# A continuación se muestra un ejemplo del resultado esperado

# calle <- "calle Los Alerces"
# numero <- "número 123"
# build_address(calle, numero)
## [1] "los alerces 123"

build_address<- function(calle,numero,depto = NULL) {
  calle <- gsub("avenida","",calle)
  calle <- gsub("av.","",calle)
  calle <- gsub("calle","",calle)
  calle <- gsub("pasaje","",calle)
  numero <- gsub("numero","",numero)
  numero <- gsub("num","",numero)
  numero <- gsub("número","",numero)
  numero <- gsub("n","",numero)
  depto <- gsub("depto.","",depto)
  depto <- gsub("departamento","",depto)
  if (is.null(depto)){
    address <-print(paste0(calle," ", numero))
  }else{
    address <-print(paste0(calle," ", numero,", depto ",depto))
  }
  
  return(address)
}

calle <- "calle Los Alerces"
numero <- "número 123"
depto <- "2"

build_address(calle, numero,depto)

# Ejercicio 3

# Utilice la función creada sobre el siguiente dataframe, generando una nueva columna llamada dirección.

df <- tribble(~calle,              ~numero,      ~depto,
              "calle Hemingway",   "num 345",    "depto. 345",
              "av. Albert Camus",  "número 123", "123",
              "Manuel Rojas",      "234",        "departamento 231",
              "Nicanor Parra",     "678",        NULL
) 
df

df <- df %>% 
  mutate(dirección = build_address(calle, numero, depto ))
