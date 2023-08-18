library(dplyr)
library(tidyverse)
library(purrr)
library(gapminder)
library(ggplot2)
library(tidyr)

#Ejercicio 1
#funcion sum_something
sum_something <- function(data, group_var, var) {
  data %>% 
    group_by(!!enexpr(group_var)) %>% 
    summarise(n = sum(!!enexpr(var)))
}

#funcion plot, agregando el año en el titulo
plot_table <- function(table, x_var, y_var,  input_title, year ) {
  ggplot(table, aes(x = !!enexpr(x_var), y = !!enexpr(y_var) )) +
    geom_bar(stat = "identity") +
    labs(title = paste(input_title,"-Año",year))
}
#Creacion de los gráficos contitulo que considera el año
gapminder_list <- split(gapminder, gapminder$year)
plots_by_year <- gapminder_list %>% 
  imap(~sum_something(.x, continent, pop) %>% 
         plot_table(continent, n, "Población mundial, según continente", .y))



#Ejercicio 2: 

#funcion plot que considera el año en el subitutlo
plot_table <- function(table, x_var, y_var,  input_title, input_subtitle, year ) {
  ggplot(table, aes(x = !!enexpr(x_var), y = !!enexpr(y_var) )) +
    geom_bar(stat = "identity") +
    labs(title = input_title, subtitle =paste("Año",year))
}
#Creacion de graficos que consideran el año en el subtitulo

gapminder_list <- split(gapminder, gapminder$year)
plots_by_year <- gapminder_list %>% 
  imap(~sum_something(.x, continent, pop) %>% 
         plot_table(continent, n, "Población mundial, según continente", "Año", .y))

print(plots_by_year)

#Ejercicio 3:

pegar<-function(valor_uno,valor_dos){
  paste(valor_uno,valor_dos)
  
  
}

nested_map<-function(v1,v2){
  cruzar<-crossing(v1,v2)
  map2(cruzar$v1,cruzar$v2,pegar)
} 


#Probando la funcion
probando_funcion<-nested_map(1:3,5:8)

walk(probando_funcion,print)