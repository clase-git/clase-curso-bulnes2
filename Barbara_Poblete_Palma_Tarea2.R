# Desarrollo Tarea 2 Clase 3 ####

# Nombre: Bárbara Daniela Poblete Palma

# Librerías a utilizar

library(dplyr)
library(tidyverse)
library(data.table)
library(MASS)
library(ggplot2)
library(datasets)
library(feather)
library(purrr)

# Eliminación de objetos existentes

rm(list = ls())

# Nota: Las respuestas de las preguntas del Trabajo Práctico están contenidas en los comentarios que comienzan con "R:"





# EJERCICIO 1 ####

# Primero abrimos la base de datos

library(gapminder)

gapminder::gapminder

gapminder_list <- split(gapminder, gapminder$year)



# Creamos las funciones vistas en clases

sum_something <- function(data, group_var, var) {
  data %>% 
    group_by({{group_var}}) %>% 
    summarise(n = sum({{var}}))
}

plot_table <- function(data, x_var, y_var, title_plot) {
  data %>% 
    ggplot(aes(x={{x_var}}, y={{y_var}})) +
    geom_bar(stat = "identity") +
    labs(title=title_plot)
}


# Creamos la función plot_with_purrr con ayuda de imap

plot_with_purrr <- gapminder_list %>% 
  map(sum_something, continent, pop) %>% 
  imap(~ plot_table(.x, continent, n, paste("Población mundial, según continente. Año", .y)))


# Pruebas donde el título varía

plot_with_purrr[2]

plot_with_purrr[8]

plot_with_purrr[12]

# R: Efectivamente el título varía dependiendo del elemento que se seleccione en la lista plot_with_purr










# EJERCICIO 2 ####

rm(list = ls())

# Primero abrimos la base de datos

library(gapminder)

gapminder::gapminder

gapminder_list <- split(gapminder, gapminder$year)



# Creamos las funciones vistas en clases

sum_something <- function(data, group_var, var) {
  data %>% 
    group_by({{group_var}}) %>% 
    summarise(n = sum({{var}}))
}

plot_table <- function(data, x_var, y_var, title_plot, subtitle_plot) {
  data %>% 
    ggplot(aes(x={{x_var}}, y={{y_var}})) +
    geom_bar(stat = "identity") +
    labs(title=title_plot, subtitle = subtitle_plot) # Aquí modificamos ligeramente el código de clases para agregar el subtítulo
}



# Creamos la función plot_with_purrr con ayuda de imap

plot_with_purrr <- gapminder_list %>% 
  map(sum_something, continent, pop) %>% 
  imap(~ plot_table(.x, continent, n, "Población mundial, según continente", paste("Año:",.y)))



# Pruebas donde el subtítulo varía

plot_with_purrr[2]

plot_with_purrr[8]

plot_with_purrr[12]

# R: Efectivamente el subtítulo varía dependiendo del elemento que se seleccione en la lista plot_with_purr










# EJERCICIO 3 ####

rm(list = ls())

# Definimos los vectores

vector1 <- c(1:3)
vector2 <- c(5:8)



# Definimos un dataframe que contiene todas las combinaciones posibles entre los vectores definidos previamente

combinations <- expand.grid(vector1 = vector1, vector2 = vector2)



# Empleamos pmap para definir nested_map porque combinations tiene más de una variable (vector1 y vector2)

nested_map <- function(data, vector1, vector2) {
  pmap(combinations, function(vector1, vector2) {
    paste(vector1, vector2, sep = " ") 
  })
}



# Finalmente aplicamos walk a la función nested_map para obtener el listado de valores sin formato lista

walk(nested_map(combinations, vector1, vector2), print)

# R: Vemos que el código efectivamente devuelve las combinaciones solicitadas, pero en distinto orden










# EJERCICIO BONUS #### (incompleto porque no me resultó :c)

rm(list = ls())

# Creamos un objeto que contiene el directorio donde se encuentran los archivos

ruta_ene <- "C:/Users/barby/OneDrive/Desktop/Capacitación R intermedio/Clase 3/datos_ene"



# Extracción de nombres de archivos y construcción de etiquetas

files <- list.files(ruta_ene, full.names = T)
trimestres <- list.files(ruta_ene) %>% 
  str_extract(pattern = "-[[:digit:]]{2}-") %>% 
  str_remove_all("-")



# Carga de todos los archivos en la lista varios_ene

varios_ene <-  map(files, read_csv2 )
names(varios_ene) <-  paste0("trimestre_", trimestres)  
nombres_lista <-  imap(varios_ene, ~.y)
nombres_lista



# 

get_employment_sample <- imap(varios_ene, .f = ~mutate(., !!rlang::parse_expr(.y)  := if_else(activ == 1, 1, 0) ))

print(get_employment_sample)


sum_something <- function(data, group_var, var) {
  data %>% 
    group_by({{group_var}}) %>% 
    summarise(n = sum({{var}}))
}

plot_table <- function(data, x_var, y_var, title_plot) {
  data %>% 
    ggplot(aes(x={{x_var}}, y={{y_var}})) +
    geom_bar(stat = "identity") +
    labs(title=title_plot)
}


plot_with_purrr <- varios_ene %>% 
  map(sum_something, activ, id_identificacion) %>% 
  imap(~ plot_table(.x, activ, n, "Población mundial, según continente", paste("Trimestre:",.y)))



