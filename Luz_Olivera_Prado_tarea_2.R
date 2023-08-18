library(dplyr)
library(rlang)
library(ggplot2)
library(gapminder)
library(arrow)


## Ejercicio 1

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


plot_with_for <- function(tablas) {
  plots <- list(vector(length = length(tablas)))
  i <- 1
  for (plot in tablas) {
    table <- sum_something(plot, continent, pop)
    plots[[i]] <- plot_table(table, continent, n, paste("Población mundial, según continente. Año", plot$year[1]))
    i <- i + 1
  }
  return(plots)
}

plot_with_purrr <- function(tablas) {
  plots <- tablas %>%
    unname() %>%
    map(., function(plot) {
      table <- sum_something(plot, continent, pop)
      plot_table(table, continent, n, paste("Población mundial, según continente. Año", plot$year[1]))
    })
  return(plots)
}


lst2 <- plot_with_for(gapminder_list)
lst1 <- plot_with_purrr(gapminder_list)

## Ejercicio 2

## Ejercicio 3

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

nested_map <- function(v1, v2) {
  map(v1, function(x) map(v2, ~paste0(x, .x)))
}

nested_map (v1, v2)


