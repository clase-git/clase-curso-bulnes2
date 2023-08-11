library(dplyr)
library(tibble)

# Parte 1
get_cv <- function(n, boolean_na = FALSE) {
  if (boolean_na) {
    cv <- sd(n, na.rm = TRUE) / mean(n, na.rm = TRUE)
  } else {
    cv <- sd(n) / mean(n)
  }
  return(cv)
}

# Vector
x <- c(2,4,6,8,NA)

# F
get_cv(x)

# T
get_cv(x, TRUE)

#Parte 2
build_address <- function(street, number, apartment) {
  # Procesar la variable street
  street <- gsub("\\b(avenida |pasaje|psje|calle )\\b", "", street, ignore.case = TRUE)
  
  # Procesar la variable number
  number <- as.numeric(gsub("[^0-9]", "", number))
  
  # Si no es NULL, se conserva sólo el número de la cadena
  if (!is.null(apartment)) {
    apartment <- as.numeric(gsub("[^0-9]", "", apartment))
    # Si es NULL, el resultado para apartment es NULL
    if (is.na(apartment)) {
      apartment <- NULL
    }
  }
  # Se pega la dirección
  address <- paste0(street, " ", number)
  if (!is.null(apartment)) {
    address <- paste0(address, ", Depto. ", apartment)
  }
  return(address)
}

# Data frame a probar
df <- tribble(~calle,              ~numero,      ~depto,
              "calle Hemingway",   "num 345",    "depto. 345",
              "avenida Albert Camus",  "número 123", "123",
              "Manuel Rojas",      "234",        "departamento 231",
              "Nicanor Parra",     "678",        NULL) 

# Resultado, sin interación
build_address(df$calle[1], df$numero[1], df$depto[1])

# Se itera la función con rowwise
df <- df %>%
  rowwise() %>%
  mutate(direccion = build_address(calle, numero, depto))