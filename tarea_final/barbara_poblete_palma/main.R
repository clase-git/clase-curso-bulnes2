# DESARROLLO TAREA FINAL - CURSO R INTERMEDIO

# Instalación y carga de librerías ####

if(!require("dplyr")) install.packages("dplyr") 
library(dplyr)

if(!require("stringr")) install.packages("stringr")
library(stringr)

if(!require("purrr")) install.packages("purrr")
library(purrr)

if(!require("readr")) install.packages("readr")
library(readr)

if(!require("data.table")) install.packages("data.table")
library(data.table)

if(!require("microbenchmark")) install.packages("microbenchmark")
library(microbenchmark)

if(!require("ggplot2")) install.packages("ggplot2")
library(ggplot2)





# Ejercicio 1 ####

urls <- c("https://www.ine.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2021/esi-2021---personas.csv?sfvrsn=d03ae552_4&download=true",
          "https://www.ine.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2020/esi-2020---personas.csv?sfvrsn=fb1f7e0c_4&download=true",
          "https://www.ine.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2019/esi-2019---personas.csv?sfvrsn=9eb52870_8&download=true",
          "https://www.ine.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2018/esi-2018---personas.csv?sfvrsn=a5de2b27_6&download=true",
          "https://www.ine.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2017/esi-2017---personas.csv?sfvrsn=d556c5a1_6&download=true",
          "https://www.ine.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2016/esi-2016---personas.csv?sfvrsn=81beb5a_6&download=true"
)


# Vector con nombre de archivos

file_names <- map_chr(urls, extract_name)


# Carpeta data que contendrá los archivos descargados

if (!dir.exists("data")) {
  dir.create("data")
} 


# Descarga de archivos en carpeta data

map2(urls, file_names, ~download_esi_data(.x, .y))





# Ejercicio 2 ####

# Creamos un vector con la ruta de los archivos descargados previamente

file_paths <- map_chr(file_names, ~ paste0("data/", .x))


# Aplicación de la función read_esi_data para abrir archivos

lista_data_esi <- map(file_paths, read_esi_data) %>% set_names(paste0(file_names))





# Ejercicio 3 ####

# Tabla que contiene 3 columnas: version, n_personas (idrph) y n_hogares (id_identificacion)

lista_tabla_n1 <- map(lista_data_esi, tabla_n1)


# Convertimos la lista creada con los resultados en data.table

data_table_n1 <- lista_tabla_n1 %>%
  imap(~ mutate(.x, Origen = .y)) %>%
  bind_rows()

rm(lista_tabla_n1)


# Creación de variable "version"

data_table_n1 <- data_table_n1 %>% 
  mutate(version = str_extract(Origen, "(esi-[[:digit:]]{4})")) %>% 
  select(-Origen)


# Ordenamos variables, logrando así la tabla solicitada

data_table_n1 <- data_table_n1[, c(3,1,2)]


#---


# Para obtener la segunda tabla, primero filtramos la base para que quede a nivel de hogar

lista_data_esi_filter <- map(lista_data_esi, filter_h)


# Tabla que contiene el mínimo, máximo, media, mediana, p10 y p90 del factor expansión de cada versión

lista_tabla_n2 <- map(lista_data_esi_filter, tabla_n2)


# Convertimos la lista creada con los resultados en data.table

data_table_n2 <- lista_tabla_n2 %>%
  imap(~ mutate(.x, Origen = .y)) %>%
  bind_rows()

rm(lista_tabla_n2)


# Creación de variable "version"

data_table_n2 <- data_table_n2 %>% 
  mutate(version = str_extract(Origen, "(esi-[[:digit:]]{4})")) %>% 
  select(-Origen)


# Ordenamos variables, logrando así la tabla solicitada

data_table_n2 <- data_table_n2[, c(7,1,2,3,4,5,6)]

data_table_n2 <- data_table_n2 %>% 
  mutate_if(is.numeric, ~ round(., 2))


# Respuesta: La esi-2020 cuenta con peso de muestro atípico porque su promedio es bastante superior
# al resto de los promedios de las demás versiones de la encuesta. Además, su peso máximo es inferior
# al resto de las versiones.


#---


# Tabla que contiene número de estratos por conglomerado

lista_tabla_n3 <- map(lista_data_esi, tabla_n3)


# Convertimos la lista creada con los resultados en data.table

data_table_n3 <- lista_tabla_n3 %>%
  imap(~ mutate(.x, Origen = .y)) %>%
  bind_rows()

rm(lista_tabla_n3)

# Creación de variable "version"

data_table_n3 <- data_table_n3 %>% 
  mutate(version = str_extract(Origen, "(esi-[[:digit:]]{4})")) %>% 
  select(-Origen)


# Ordenamos variables, logrando así la tabla solicitada

data_table_n3 <- data_table_n3[, c(2,1)]


#---


# Tabla que contiene mínimo, máximo, media, mediana, p10 y p90 de los ingresos del trabajo principal considerando factor de expansión

lista_tabla_n4 <- map(lista_data_esi, tabla_n4)


# Convertimos la lista creada con los resultados en data.table

data_table_n4 <- lista_tabla_n4 %>%
  imap(~ mutate(.x, Origen = .y)) %>%
  bind_rows()

rm(lista_tabla_n4)

# Creación de variable "version"

data_table_n4 <- data_table_n4 %>% 
  mutate(version = str_extract(Origen, "(esi-[[:digit:]]{4})")) %>% 
  select(-Origen)


# Ordenamos variables, logrando así la tabla solicitada

data_table_n4 <- data_table_n4[, c(7,1,2,3,4,5,6)]





# Ejercicio 4 ####

# Generación de variable agno que representa el año de la encuesta

lista_agnos <- map_chr(names(lista_data_esi), ~ str_extract(.x, "\\d{4}"))

lista_data_esi <- map2(lista_data_esi, lista_agnos, ~ mutate(.x, agno = .y))


# Obtenemos un dataframe único conformado por los dataframes de la lista

esi_total <- bind_rows(lista_data_esi)

# Transformamos el dataframe anterior en datatable para usar comandos tipo data.table

esi_total_dt <- as.data.table(esi_total)


# El siguiente comando presenta las distintas formas de calcular el promedio de ingresos (excluye personas con ingreso igual a 0)

results_mean <- microbenchmark(
  
                 mean1 <- map(lista_data_esi, ~ mean(.x[.x$ing_t_p != 0, "ing_t_p"])), # Cálculo de promedio con herramienta purrr
                 
                 mean2 <- esi_total %>% 
                   filter(ing_t_p != 0) %>%
                   group_by(agno) %>% 
                   summarise(prom_ing = mean(ing_t_p)), # Cálculo con group_by() y summarise()
                 
                 mean3 <- map(lista_data_esi, mean_datatable), # Cálculo con purrr y función creada con data.table
                 
                 mean4 <- esi_total_dt[ing_t_p != 0, .(prom_ing = mean(ing_t_p)), by = .(agno)], # Cálculo con data.table
                 
                 times = 5)

results_mean_graph <- autoplot(results_mean) 

# Respuestas: 

# Todas las estrategias presentan mismo resultado en términos de valor. La única diferencia entre estas
# es la presentación de los resultados, por ejemplo, mean1 y mean3 contienen una lista de los resultados;
# mientras que mean2 presenta un dataframe ordenado desde 2016 a 2021, y mean4 un dataframe tipo datatable
# que contiene el orden inverso de los resultados (desde 2021 a 2016).

# El comando que se desempeña mejor es el map() del primer resultado (mean1), ya que presenta
# un tiempo de ejecución inferior al resto de los métodos, el cual llega a ser menor que 30 milisegundos.
# No obstante, si a map() se le añade una función que trabaje con comando datable, es menos eficiente que
# group_by(). Entonces, map() es más eficiente que group_by() cuando no emplea funciones que contienen
# comandos provenientes de data.table.
