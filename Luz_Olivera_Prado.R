install.packages("dplyr", dependencies = T)
library(dplyr)
install.packages("tidyverse")
library(tidyverse)

#Ejercicio 1

#Construye una función llamada get_cv que calcule el coeficiente de variación de un vector. get_cv debe recibir 2 parámetros:
#un vector de tamaño n
#un valor boolean que indique si los valores NA deben ser removidos. Por defecto, la función no debe remover dichos valores.
#En la construcción de la función no está permitido utilizar la función cv, pero sí mean y sd

#La fórmula para calcular el coeficiente de variación es 
#cv=  sd/¯X

#vector numerico
x <- c(1,2,3)
#valor boolean (1 para verdadero, 0 para falso)
y<-1

get_cv<- function(x,y){
  if(y==1) {
    cv<- sd(na.omit(x))/mean(na.omit(x))
    return(cv)}  
}

get_cv(x,y)

#Ejercicio 2
#Crea una función llamada build_address que construya una dirección en base a algunos inputs, siguiendo un formato establecido. build_address recibe 2 parámetros obligatorios y uno opcional.
#Parámetros obligatorios:
#street: string que contiene el nombre de la calle
#number: string que contiene el número de la dirección
#El parámetro opcional es apartment y contiene el número del departamento.
#A continuación se muestra un ejemplo del resultado esperado
#street <- "calle Los Alerces"
#number <- "número 123"
#build_address(street, number)
## [1] "los alerces 123"
#Si la vivienda se encuentra en un departamento, el formato esperado es "los alerces 123, depto. 34"
#Considere que no existe un formato preestablecido para registrar calle y número, lo que implica que la función debe procesar cosas como: "numero 123", "123", "num 123", "número 123", "n 123". Análogamente, la calle podría estar descrita de diferentes modos: "avenida Los Alerces", "Los Alerces", "av. Los Alerces", "calle los alerces", "pasaje Los Alerces", "los alerces", etc.
#Independiente de lo que reciba la función, el formato siempre debe ser: NOMBRE CALLE, NÚMERO, DEPTO.
#En la construcción de build_address puede usar cualquier función que sea de utilidad para lograr el objetivo. No es necesario que la función sea capaz de procesar todos los casos posibles.
#Pista: En la construcción de la función pueden ser útiles algunas expresiones regulares

build_address <- function(street,number,apartment=NULL){
  street<-tolower(street)
  number<-tolower(number)
  apartment<-tolower(apartment)
  
  street<- (gsub("^\\s+|\\s+$|psje|psje.|pasaje|calle|av\\.|avenida", "", street))
  number<- (gsub("^\\s+|\\s+$|número|n|num|n", "", number))
  apartment<-(gsub("^\\s+|\\s+$|dep\\.", "", number))
  
  direc <- paste(street,number, sep=" ")
  
  if(!is.null(apartment)){
    direc <- paste(direc, paste("depto.", apartment), sep=", ")
  }
  
  return(direc)
}

street <-" PASAJE los alerces "
number <- "número 2030"
apartment <- 541

build_address(street,number,apartment)

#  Ejercicio 3

# Utilice la función creada sobre el siguiente dataframe, generando una nueva columna llamada dirección.

df <- tribble(~calle,              ~numero,      ~depto,
              "calle Hemingway",   "num 345",    "depto. 345",
              "av. Albert Camus",  "número 123", "123",
              "Manuel Rojas",      "234",        "departamento 231",
              "Nicanor Parra",     "678",        NULL
) 
df

df <- df %>%
  rowwise () %>%
  mutate (direccion = build_address(calle,numero,depto))
df







         
