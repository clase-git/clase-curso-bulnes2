library("dplyr")
library("tidyverse")
library("stringr")
library("tibble")

# Ejercicio 1: Funcion para calcular el coeficiente de variacion

get_cv<-function(vector,Eliminar_NA){
  
  cv<-sd(vector,na.rm = Eliminar_NA)/mean(vector,na.rm=Eliminar_NA)
  return(cv)
}
# Probanfo la funcion
datos_uno<-c(1,2,3,4,5,NA)

print(get_cv(datos_uno,TRUE))


# Ejercicio 2: Funcion que construye una direccion

buil_address <- function(street, number, apartament = "") {
  number_vec <- str_extract(number, "\\d+")
  
  # Condicional con ifelse
  address <- ifelse(
    apartament != "",
    {
      apartament_vec <- str_extract(apartament, "\\d+")
      paste(street, number_vec, paste(", depto.", apartament_vec), sep = " ")
    },
    paste(street, number_vec, sep = " ")
  )
  
  return(address)
}
#Probando la funcionn
mi_direccion<-buil_address("Lor cochrane","calle 176","1905")
print(mi_direccion)

#Ejercicio 3: Probar la funcion sobre un dataframes:

df <- tribble(
  ~calle,             ~numero,      ~depto,
  "calle Hemingway",  "num 345",    "depto. 345",
  "av.Albert Camus",  "numero 123", "123",
  "Manuel Rojas",     "234",        "departamento 231",
  "Nicanor Parra",    "678",        NA
)

df_con_direccion<-df %>% 
  mutate(direccion=buil_address(calle,numero,depto))
print(df_con_direccion)