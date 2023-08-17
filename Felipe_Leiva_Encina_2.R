# TAREA 2 -----------------------------------------------------------------

# Carga de librerias ------------------------------------------------------
library(tidyverse)                     # carga de purrr, dplyr, ggplot2, otras

# Ejercicio 1 -------------------------------------------------------------

gapminder <- gapminder::gapminder
gapminder_list <- base::split(gapminder, gapminder$year)

# Función suma
sum_something <- function(data, group_var, var) {
  data %>% 
    dplyr::group_by(!!enexpr(group_var)) %>% 
    dplyr::summarise(n = sum(!!enexpr(var)))
}


# Función gráfico de barras
plot_table <- function(table, x_var, y_var, titulo_var){
  
  ggplot2::ggplot(table, aes(x = {{x_var}}, y = !!enexpr(y_var), fill = {{x_var}}))+
    ggplot2::geom_bar(stat = "identity")+
    ggplot2::scale_y_continuous(labels = function(x) format(x, big.mark = ".", decimal.mark = ",", scientific = FALSE)) +
    ggplot2::theme_minimal()+
    ggplot2::theme(legend.position = "none")+
    ggplot2::labs(title = titulo_var)
  
}


# Función gráfico de barras por año usando purrr
plot_with_purrr <- function(tablas){
  
  plots <- tablas %>% 
    purrr::map(~sum_something(.x, continent, pop)) %>%                                                            # .x representa a cada elemento del objeto table
    purrr::imap(.f = ~mutate(., year =!!rlang::parse_expr(.y))) %>%                                               # .y representa el indice de c/u de los elementos procesados previamente
    purrr::map(~plot_table(.x, continent, n, paste0("Población mundial, según continente. Año ",.x$year[1],"."))) # .x representa a cada elemento dentro de las lineas de código procesadas previamente

  return(plots)
}

# Generamos output
plots <- plot_with_purrr(gapminder_list)
plots

# testeamos
# a <- gapminder_list %>% purrr::map(~sum_something(.x, continent, pop)) %>%
#   purrr::imap(.f = ~mutate(., year =!!rlang::parse_expr(.y))) %>%
#   purrr::map(~plot_table(.x, continent, n,
#                          paste("Población mundial, según continente. Año ", .x$year[1])))



# Ejercicio 2 -------------------------------------------------------------
# Modificamos función  plot_table
plot_table <- function(table, x_var, y_var, titulo_var, year_var){
  
  ggplot2::ggplot(table, aes(x = {{x_var}}, y = !!enexpr(y_var), fill = {{x_var}}))+
    ggplot2::geom_bar(stat = "identity")+
    ggplot2::scale_y_continuous(labels = function(x) format(x, big.mark = ".", decimal.mark = ",", scientific = FALSE)) +
    ggplot2::theme_minimal()+
    ggplot2::theme(legend.position = "none")+
    ggplot2::labs(title = titulo_var, subtitle = year_var)
  
}

# Modificamos función  plot_with_purrr
plot_with_purrr <- function(tablas){
  
  plots <- tablas %>% 
    purrr::map(~sum_something(.x, continent, pop)) %>%                                                        # .x representa a cada elemento del objeto table
    purrr::imap(.f = ~mutate(., year =!!rlang::parse_expr(.y))) %>%                                           # .y representa el indice de c/u de los elementos procesados previamente
    purrr::map(~plot_table(.x, continent, n, "Población mundial, según continente",paste("Año", .x$year[1]))) # .x representa a cada elemento dentro de las lineas de código procesadas previamente
  
  return(plots)
}

# Generamos output
plots <- plot_with_purrr(gapminder_list)
plots


# Ejercicio 3 -------------------------------------------------------------

# Función con for anidado
nested_for <- function(v1, v2) {
  for (x in v1) {
    for (y in v2){
      print(paste(x, y))
    }
  }
}

nested_for(1:3, 5:8)

# Función con sintaxis de purrr
nested_purrr <- function(vector1, vector2){
  
  c <- min(vector1)
  
  while (c <= max(vector1)){
    purrr::walk(paste(vector1[c], vector2), print)
    c <- c+1
  }
  
}

nested_purrr(1:3, 5:8)



# Bonus -------------------------------------------------------------------

# Extraer nombres de archivos y construir etiquetas
files <- list.files("data/datos_ene/", full.names = T)

trimestres <- list.files("data/datos_ene/") %>% 
  stringr::str_extract(pattern = "-[[:digit:]]{2}-") %>% 
  stringr::str_remove_all("-")

# Leer archivos y etiquetar
datos_ene <- purrr::map(files, read_csv2)
names(datos_ene) <-  paste0("trimestre_", trimestres)  


# Escribe una función llamada get_employment_sample que reciba un dataframe y 
# devuelva la cantidad de ocupados sin expandir. La categoría 1 de la variable 
# activ muestra a los ocupados

get_employment_sample <- function(df){
  
  output <- imap(df, .f = ~mutate(., ocupados = if_else(activ == 1, 1, 0) )) %>% 
    purrr::map2(., filtro, ~.x %>%                                           # .: vector 1; filtro: vector 2; .x: c/u de los elementos del vector 1
                  dplyr::filter(ocupados == .y) %>%                          # .y: c/u de los elementos del vector 2
                  dplyr::summarise(total = sum(ocupados)))                   # crea la columna total con la suma del total de ocupados

  }

ocupados <- get_employment_sample(datos_ene)
ocupados

# Usando get_employment_sample, genera un gráfico que muestre la cantidad de 
# ocupados y no ocupados para cada uno de los trimestres.

# PROXIMAMENTE... EN CONSTRUCCIÓN
