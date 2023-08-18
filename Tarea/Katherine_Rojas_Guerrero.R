

# Librerias ---------------------------------------------------------------

library(purrr)
library(gapminder)
library(tidyverse)
library(ggplot2)
library(tidyr)


# Ejercicio 1 -------------------------------------------------------------

# El siguiente código genera un resultado muy similar al del último ejercicio 
# revisado en la clase. La diferencia es que la implementación es mediante un 
# ciclo for. Adicionalmente, se agrega una funcionalidad que agrega al título 
# el año correspondiente.
# 
# gapminder_list <- split(gapminder, gapminder$year)
# plot_with_for <- function(tablas){
#   plots <- list(vector(length = length(tablas) ))
#   i <- 1
#   for (plot in tablas) {
#     table <- sum_something(plot, continent, pop)
#     plots[[i]] <- plot_table(table, continent, n, paste("Población mundial, según continente. Año", plot$year[1] )  )
#     i <-  i + 1
#   }
#   return(plots)
# }
#
# plots <- plot_with_for(gapminder_list)
# 
# La tarea consiste en llegar al mismo resultado, pero utilizando únicamente 
# las herramientas de purrr. Crea una función llamada plot_with_purrr que reciba 
# una lista de tablas y devuelva una lista de gráficos
#Pista: La función imap puede ser de gran ayuda

plot_with_purrr <- function(tablas) {
  plots <- imap(tablas, ~ {
    table <- sum_something(.x, continent, pop)
    plot_table(table, continent, n, paste("Población mundial, según continente. Año", .y))
  })
  return(plots)
}

plots <- plot_with_purrr(gapminder_list)
plots



# Ejercicio 2 -------------------------------------------------------------

# Modifica la función plot_table para que el año del gráfico aparezca en el 
# subtítulo y no en el título. La función modificada debería recibir un parámetro
# extra llamado subtitulo, que permita agregar el año al subtítulo del gráfico.
# Una vez que hayas modificado tu función, utilízala dentro de plot_with_purrr.
# Cada gráfico debería tener el año correspondiente en el subtítulo.


plot_table <- function(data, xv, yv, titulo, subtitulo) {
  ggplot(data, aes(x = {{xv}}, y = {{yv}})) +
    geom_bar(stat = "identity") +
    labs(title = titulo, subtitle = subtitulo)
}

plot_with_purrr <- function(tablas) {
  plots <- imap(tablas, ~ {
    table <- sum_something(.x, continent, pop)
    plot_table(table, continent, n, "Población mundial, según continente.", paste("Año", .y))
  })
  return(plots)
}

plots <- plot_with_purrr(gapminder_list)
plots



# Ejercicio 3 -------------------------------------------------------------

# El siguiente for anidado genera pares de x e y. El ejercicio consiste en 
# escribir una función llamada nested_map que utilice una sintaxis de purrr.
# La función debe recibir dos vectores numéricos (de igual o distinto largo) 
# e imprimir pares de número.
# Es posible que la sintaxis llegue a ser un poco confusa. Reflexiona sobre la 
# pertinencia de purrr para tareas de este tipo.
# nested_for <- function(v1, v2) {
#   for (x in v1) {
#     for (y in v2){
#       print(paste(x, y))
#     }
#   }
# }
# nested_for(1:3, 5:8)

nested_map <- function(v1, v2) {
  combinations <- expand_grid(v1 = v1, v2 = v2)
  for (i in seq_len(nrow(combinations))) {
    print(paste(combinations$v1[i], combinations$v2[i]))
  }
}

nested_map(1:3, 5:8)


