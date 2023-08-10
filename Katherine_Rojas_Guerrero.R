
# Ejercicio 1 -------------------------------------------------------------


# Definición de la función get_cv que calcula el coeficiente de variación de un vector.
# Parámetros:
#   - vector: El vector numérico del cual se calculará el coeficiente de variación.
#   - remove_na: Un valor booleano que indica si los valores NA deben ser removidos antes del cálculo.

# Definir la función get_cv que calcula el coeficiente de variación de un vector

get_cv <- function(vector, remove_na = FALSE) {
  # Si remove_na es TRUE, eliminar valores NA del vector
  if (remove_na) {
    vector <- vector[!is.na(vector)]
  }
  
  # Verificar si el vector tiene al menos dos elementos para calcular el coeficiente de variación
  if (length(vector) <= 1) {
    stop("El vector debe tener al menos dos elementos para calcular el coeficiente de variación.")
  }
  
  # Calcular la desviación estándar y la media del vector
  sd_value <- sd(vector, na.rm = TRUE)
  mean_value <- mean(vector, na.rm = TRUE)
  
  # Manejar el caso en que la media sea cero para evitar división por cero
  if (mean_value == 0) {
    cv <- Inf  # Asignar Inf como coeficiente de variación si la media es cero
    message("No es posible calcular el coeficiente de variación porque la media es igual a cero.")
  } else {
    cv <- sd_value / mean_value
  }
  
  
  return(cv)
}

# Ejemplos de uso
vector1 <- c(10, 20, 30, 40, 50)
cv1 <- get_cv(vector1)
cat("Coeficiente de variación (sin NA) del vector 1:", cv1, "\n")

vector2 <- c(15, 25, NA, 35, 45)
cv2 <- get_cv(vector2, remove_na = TRUE)
cat("Coeficiente de variación (con NA removidos) del vector 2:", cv2, "\n")

vector3 <- c(0, 0, 0, 0, 0)
cv3 <- get_cv(vector3)
cat("Coeficiente de variación (caso con mean igual a cero) del vector 3:", cv3, "\n")

# VERIFICANDO
sd(vector2, na.rm = TRUE)/mean(vector2, na.rm =TRUE)


# Ejercicio 2 -------------------------------------------------------------


# Definición de la función build_address que construye una dirección en base a inputs.
# Parámetros:
#   - street: string que contiene el nombre de la calle.
#   - number: string que contiene el número de la dirección.
#   - apartment: (opcional) string que contiene el número del departamento.

build_address <- function(street, number, apartment = NULL) {
  street <- tolower(street)
  street <- gsub("(calle|av\\.|av\\.|avenida|pasaje) ", "", street)
  
  number <- gsub("(numero|num|número|n) ", "", number) # Eliminar palabras comunes
  number <- gsub("#", "", number, fixed = TRUE)  # Eliminar el signo "#"
  
  if (!is.null(apartment)) {
    apartment <- gsub("(departamento|depto.|depto)\\s*", "", apartment, ignore.case = TRUE)
  }
  
  address <- paste(street, number, sep = " ")
  
  if (!is.null(apartment)) {
    address <- paste(address, apartment)
  }
  
  return(address)
}


# Ejemplos de uso

# Dirección sin depto
street1 <- "calle Los Alerces"
number1 <- "número 123"
address1 <- build_address(street1, number1)
cat("Dirección:", address1, "\n")

# Dirección con depto
street2 <- "avenida Los Cerezos"
number2 <- "num 456"
apartment2 <- "34"
address2 <- build_address(street2, number2, apartment2)
cat("Dirección con departamento:", address2, "\n")

# Dirección con signo gato
street3 <- "avenida Los Cerezos"
number3 <- "#4566"
apartment3 <- "5634"
address3 <- build_address(street3, number3, apartment3)
cat("Dirección con departamento:", address3, "\n")






# Ejercicio 3 -------------------------------------------------------------

# Cargar el paquete dplyr para trabajar con el DataFrame
library(dplyr)

# Crear un DataFrame usando la función tribble con columnas "calle", "numero" y "depto"
df <- tribble(
  ~calle,              ~numero,      ~depto,
  "calle Hemingway",   "num 345",    "depto. 345",
  "av. Albert Camus",  "número 123", "123",
  "Manuel Rojas",      "234",        "departamento 231",
  "Nicanor Parra",     "678",        NULL
) 

# Agregar una nueva columna "direccion" al DataFrame usando la función mutate del paquete dplyr
df <- df %>%
  mutate(direccion = build_address(calle, numero, depto))

# Imprimir el DataFrame resultante con la nueva columna "direccion"
df





df <- df %>%
  rowwise() %>%
  mutate(direccion = build_address(calle, numero, depto))

df


library(dplyr)

# Definir la función build_address
build_address <- function(street, number, apartment = NULL) {
  street <- tolower(street)
  street <- gsub("(calle|av\\.|av\\.|avenida|pasaje) ", "", street)
  
  number <- gsub("(numero|#num|#número|#n) ", "", number)
  number <- gsub("#", "", number, fixed = TRUE)
  
  if (!is.null(apartment)) {
    apartment <- gsub("(departamento|depto)\\s*", "", apartment, ignore.case = TRUE)
  }
  
  address <- paste(street, number, sep = " ")
  
  if (!is.null(apartment)) {
    address <- paste(address, apartment)
  }
  
  return(address)
}

# Crear el DataFrame
df <- tribble(
  ~calle,              ~numero,      ~depto,
  "calle Hemingway",   "num 345",    "depto. 345",
  "av. Albert Camus",  "número 123", "123",
  "Manuel Rojas",      "234",        "departamento 231",
  "Nicanor Parra",     "678",        NA
) 

# Aplicar la función build_address a cada fila del DataFrame usando rowwise() y mutate()
df <- df %>%
  rowwise() %>%
  mutate(direccion = build_address(calle, numero, depto))

df
# Imprimir el DataFrame resultante
print(df)




