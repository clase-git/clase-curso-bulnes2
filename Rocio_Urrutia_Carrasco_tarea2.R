library(purrr)
library(tidyverse)
library(ggplot2)
library(gapminder)

gapminder_list <- split(gapminder, gapminder$year)

sum_something <- function(data, group_var, var) {
  data %>% 
    group_by(!!enexpr(group_var)) %>% 
    summarise(n = sum(!!enexpr(var)))
}

tabla <- sum_something(gapminder, continent, pop)

plot_table <- function(tabla, x_var, y_var,  input_title ) {
  ggplot(table, aes(x = !!enexpr(x_var), y = !!enexpr(y_var) )) +
    geom_bar(stat = "identity") +
    labs(title = input_title)
}
plot_table(tabla, continent, n,  "Población mundial, según continente" )

plots_by_year <- gapminder_list %>% 
  map(sum_something, continent, pop) %>% 
  map(plot_table, continent, n, "Población mundial, según continente" )

print(plots_by_year)

######## Ejercicio 1

gapminder_list <- split(gapminder, gapminder$year)

plot_with_for <- function(tabla){
  plots <- list(vector(length = length(tabla) ))
  i <- 1
  for (plot in tabla) {
    table <- sum_something(plot, continent, pop)
    plots[[i]] <- plot_table(table, continent, n, paste("Población mundial, según continente. Año", plot$year[1] )  )
    i <-  i + 1
  }
  return(plots)
}

plots <- plot_with_for(gapminder_list)

print(plots) 

plot_with_purrr <- gapminder_list %>% 
  imap(~sum_something(.x, continent, pop)) %>%
  imap(~plot_table(.x, continent, n, paste("Población mundial, según continente. Año",.y )))

print(plot_with_purrr)

######## Ejercicio 2

plot_table1 <- function(tabla, x_var, y_var, input_title , imput_subtitle) {
  ggplot(table, aes(x = !!enexpr(x_var), y = !!enexpr(y_var) )) +
    geom_bar(stat = "identity") +
    labs(title = input_title,subtitle = imput_subtitle)
}

plot_table1(tabla, continent, n,  "Población mundial, según continente.","Año")

plot_with_purrr <- gapminder_list %>% 
  imap(~sum_something(.x, continent, pop)) %>%
  imap(~plot_table1(.x, continent, n, "Población mundial, según continente",paste("Año",.y )))

print(plot_with_purrr)

######## Ejercicio 3

nested_for <- function(v1, v2) {
  for (x in v1) {
    for (y in v2){
      print(paste(x, y))
    }
  }
}

nested_for(1:3, 5:8)

nested_map <- function(v1, v2) {
  walk(v1, function(x) {
    walk(v2, function(y) {
      print(paste(x, y))
    })
  })
}

nested_map(1:3, 5:8)

######## Ejercicio 4


