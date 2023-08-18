 
# Set up ------------------------------------------------------------------
library(dplyr)
library(tidyverse)
library(ggplot2)
library(purrr)

# Cargar base -------------------------------------------------------------


gapminder <- gapminder::gapminder

gapminder_list <- split(gapminder, gapminder$year)


# Funciones auxiliaries  -------------------------------------------------

sum_something <- function(df, var1,var2){
  df %>% 
    group_by({{var1}}) %>% 
    summarise(n = sum({{var2}}))
}


plot_table <- function(table, X, Y , titulo){
  table %>% 
    ggplot(aes(x = {{X}}, y = {{Y}})) + 
    geom_bar(stat = "identity")+
    labs(title = paste("Población en el año", titulo)) +
    theme_classic()
}


# Ejercicio 1 -------------------------------------------------------------

plot_with_purrr <- function(lista_df){
  lista_df %>% 
    map( ~sum_something(.x, continent, pop)) %>% 
    imap( ~plot_table(.x, continent, n, .y))
  
}

U <- plot_with_purrr(gapminder_list)

U[[1]]
U[[2]]
U[[3]]
#etc...

# Ejercicio 2 -------------------------------------------------------------

# Modifica la función plot_table para que el año del gráfico aparezca en el subtítulo y no en el
# título. La función modificada debería recibir un parámetro extra llamado subtitulo, que permita
# agregar el año al subtítulo del gráfico.
# Una vez que hayas modificado tu función, utilízala dentro de plot_with_purrr. Cada gráfico
# debería tener el año correspondiente en el subtítulo.


plot_table_modificado <- function(table, X, Y , subtitulo){
  table %>% 
    ggplot(aes(x = {{X}}, y = {{Y}})) + 
    geom_bar(stat = "identity")+
    labs(title = "Población de",
         subtitle = subtitulo) +
    theme_classic()
}


plot_with_purrr_modficado <- function(lista_df){
  lista_df %>% 
    map( ~sum_something(.x, continent, pop)) %>% 
    imap( ~plot_table_modificado(.x, continent, n, .y))
  
}


Y <- plot_with_purrr_modficado(gapminder_list)

Y[[1]]
Y[[2]]
Y[[3]]
#Etc...


# Ejercicio 3 -------------------------------------------------------------


nested_for <- function(v1, v2) {
  for (x in v1) {
    for (y in v2){
      print(paste(x, y))
    }
  }
}

nested_for(1:3, 5:8)

nested_map <- function(var1, var2) {
  L <-  map(var1, function(x) {
    map(var2, function(y) {
      paste(x, y)
    }) 
  })
  print(unlist(L))
}

nested_map(1:3, 5:8)


