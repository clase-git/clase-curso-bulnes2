library(tidyverse)
library(dplyr)
library(rlang)
library(ggplot2)
library(gapminder)

#### Ejercicio 1 #####
#Crea una función llamada plot_with_purrr que reciba una lista de tablas y devuelva una lista de gráficos

sum_something <- function(data, group_var, var) {
  data %>%
    group_by(!!enexpr(group_var)) %>%
    summarise(n = sum(!!enexpr(var)))
}

plot_table <- function(table, x_var, y_var, input_title) {
  ggplot(table, aes(x = !!enexpr(x_var), y = !!enexpr(y_var))) +
    geom_bar(stat = "identity") +
    labs(title = input_title)
}

gapminder_list <- split(gapminder, gapminder$year)

plots_by_year <- gapminder_list %>%
  map(~sum_something(.x, continent, pop)) %>%
  map(~plot_table(.x, continent, n, "Población mundial, según continente"))

#-------------

plot_with_purrr <- function(tablas) {
  plots <- tablas %>%
    unname() %>%
    map(., function(plot) {
      table <- sum_something(plot, continent, pop)
      plot_table(table, continent, n, paste("Población mundial, según continente. Año", plot$year[1]))
    })
  return(plots)
}

ejercicio1 <- plot_with_purrr(gapminder_list)

#### Ejercicio 2 #####

#Modifica la función plot_table para que el año del gráfico aparezca en el subtítulo y no en el título. La función modificada debería recibir un parámetro extra llamado subtitulo, que permita agregar el año al subtítulo del gráfico.

#Una vez que hayas modificado tu función, utilízala dentro de plot_with_purrr. Cada gráfico debería tener el año correspondiente en el subtítulo.

plot_table <- function(table, x_var, y_var, subtitulo) {
  ggplot(table, aes(x = !!enexpr(x_var), y = !!enexpr(y_var))) +
    geom_bar(stat = "identity") +
    labs(subtitle = subtitulo)
}

plot_with_purrr <- function(tablas) {
  plots <- tablas %>%
    unname() %>%
    map(., plot) {
      table <- sum_something(plot, continent, pop)
      plot_table(table, continent, n, paste("Año", plot$year[1]))
    }
  return(plots)
}

##Ejercicio 3##

#El siguiente for anidado genera pares de x e y. El ejercicio consiste en escribir una función llamada nested_map que utilice una sintaxis de purrr. La función debe recibir dos vectores numéricos (de igual o distinto largo) e imprimir pares de número.

nested_for <- function(v1, v2) {
  for (x in v1) {
    for (y in v2) {
      print(paste(x, y))
    }
  }
}

nested_for(1:3, 5:8)
v1 <- 1:3
v2 <- 5:8

## Combinación de números (número pares no queda claro si son números pares):

nested_map <- function(v1, v2) {
  map(v1, function(x) map(v2, ~paste0(x, .x)))
}

nested_map(1:3, 5:8)
