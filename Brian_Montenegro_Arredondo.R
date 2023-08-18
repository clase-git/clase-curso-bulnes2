library(gapminder)
library(tidyverse)

# Ejercicio 1

gapminder_list <- split(gapminder, gapminder$year)

# Función 1 
sum_something <- function(data, group, var){
  data %>% 
    group_by({{group}}) %>%
    summarise(n = sum({{var}}))}

# Función 2
plot_table <- function(tab, x_var, y_var, input_title){
  tab %>%
    ggplot(aes(x = {{x_var}}, y = {{y_var}})) +
    geom_bar(stat = "identity") +
    labs(title = input_title)}

plot_with_purrr <- function(tablas) {
  plots <- imap(tablas, ~ {
    table <- sum_something(.x, continent, pop)
    plot_table(table, continent, n, paste("Población mundial, según continente. Año", .x$year[1]))
  })
  return(plots)
}

plot_with_purrr(gapminder_list)

# Ejercicio 2

plot_table <- function(tab, x_var, y_var, titulo, subtitulo) {
  tab %>%
    ggplot(aes(x = {{x_var}}, y = {{y_var}})) +
    geom_bar(stat = "identity") +
    labs(title = titulo, subtitle = subtitulo)}

plot_with_purrr <- function(tablas) {
  plots <- imap(tablas, ~ {
    table <- sum_something(.x, continent, pop)
    plot_table(table, continent, n, titulo = "Población mundial por continente", subtitulo = paste("Año", .x$year[1]))
  })
  return(plots)}

plot_with_purrr(gapminder_list)