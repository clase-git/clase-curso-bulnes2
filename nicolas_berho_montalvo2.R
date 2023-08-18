
# Ejercicio 1 --------

# librerias
library(ggplot2)
library(gapminder)
library(dplyr)
library(magrittr)
library(purrr)

# split list
gapminder_list <- split(gapminder, ~year)

# funcion 1
sum_something <- function(df, subpop, var){
  out <- df %>% 
    group_by({{subpop}}) %>% 
    summarise({{var}} := sum({{var}}))
  return(out)
}

sum_something(gapminder, continent, pop)

# funcion 2
plot_table <- function(df, subpop, var, title){
  df %>% 
    ggplot(aes(x={{subpop}}, y={{var}})) + 
    geom_bar(stat = "identity") +
    ggtitle(title)
}

sum_something(gapminder, continent, pop) %>% 
  plot_table(continent, pop, 'titulo')

# funcion 3
plot_annio <- function(df, annio){
    sum_something(df, continent, pop) %>% 
    plot_table(continent, pop, paste0("Poblacion continente en año ", annio))
}

plot_annio(gapminder, 'x')

# funcion con purrr (tarea, ejercicio 1)
plots_with_purrr <- function(list){
  annios <- names(list)
  graficos <- map2(list, annios, plot_annio)
}

# test
graficos <- plots_with_purrr(gapminder_list)

# revisamos
graficos[[1]]
graficos[[5]]
graficos[[12]]


# ejercicio 2 ------

# funcion 2 mod
plot_table <- function(df, subpop, var, title, subtit){
  df %>% 
    ggplot(aes(x={{subpop}}, y={{var}})) + 
    geom_bar(stat = "identity") +
    ggtitle(label = title, subtitle = subtit) 
}

sum_something(gapminder, continent, pop) %>% 
  plot_table(continent, pop, 'titulo', 'año')

# funcion 3 mod
plot_annio <- function(df, annio){
  sum_something(df, continent, pop) %>% 
    plot_table(continent, pop, "Poblacion continente", annio)
}

plot_annio(gapminder, 'x')


# test
graficos <- plots_with_purrr(gapminder_list)

# revisamos
graficos[[1]]
graficos[[5]]
graficos[[12]]


# Ejercicio 3 -----

nested_for <- function(v1, v2) {
  for (x in v1) {
    for (y in v2){
      print(paste(x, y))
    }
  }
}
nested_for(1:3, 5:8)

nested_map <- function(v1, v2){
  
  if(length(v1) < length(v2)){
    while(length(v1) < length(v2)){
      v1 <- c(v1,v1)
    }
    v1 <- v1[1:length(v2)]
  }
  
  if(length(v2) < length(v1)){
    while(length(v2) < length(v1)){
      v2 <- c(v2,v2)
    }
    v2 <- v2[1:length(v1)]
  }
  
  out <- map2(v1,v2,paste)
  
  out
}

nested_map(1:3, 5:8)

'La funcion requiere poner bucles while para homologar largos,
lo que rompe la propiedad de evitar los bucles.
Esto se debe a que map2 no admite vectores de largo distinto.
También el comportamiento del output es distinto, ya que en este
segundo caso imprime anidado en una lista, en vez de imprimir vectores.'
