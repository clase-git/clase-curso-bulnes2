library(dplyr)

#### Ejercicio 1 ####

#Construye una función llamada get_cv que calcule el coeficiente de variación de un vector. get_cv debe recibir 2 parámetros:

#un vector de tamaño n
#un valor boolean que indique si los valores NA deben ser removidos. Por defecto, la función no debe remover dichos valores.

vector1 <- c(10,6,11,8,19,84)

get_cv <- function(vector1) {
  mean_vector1 <- mean(vector1)
  sd_vector1 <- sd(vector1)

# Formula para calcular coeficiente de variación
  cv <- sd_vector1 / mean_vector1
  return(cv)
}

get_cv(vector1)

#### Ejercicio 2 ####

#Crear una función llamada build_address que construya una dirección en base a algunos inputs, siguiendo un formato establecido. build_address recibe 2 parámetros obligatorios y uno opcional.

# Parámetros obligatorios:
# street: string que contiene el nombre de la calle
# number: string que contiene el numero de la dirección
# El parámetro opcional es apartment y contiene el número del departamento

build_address <- function(street, number) {
  street <- gsub("^\\s+|\\s+$|calle|av\\.|avenida", "", street)
  number <- gsub("[^0-9]", "", number)
  address <- paste(street, number, sep=", ")

  return(address)
}

# Probar la función
street <- "avenida Doroteo"
number <- "número 11"
build_address(street, number)

#### Ejercicio 3 ####

#Utilizar la función creada sobre el siguiente dataframe, generando una nueva columna llamada dirección 

df <- tribble(~calle,           ~ numero,    ~ depto,
              "calle Hemingway", "num 345", "depto.345",
              "av. Albert Camus","número 123", "123",
              "Manuel Rojas",   "234" ,   "departamento 231",
              "Nicanor Parra", "678", NULL )

df <- df %>%
  rowwise() %>% 
  mutate(direccion = build_address(calle, numero))

df
