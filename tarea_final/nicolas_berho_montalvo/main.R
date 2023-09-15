# ENCABEZADO -------

# instalacion de librerias ------

if(!require("dplyr")) install.packages("dplyr")
if(!require("curl")) install.packages("curl")
if(!require("readr")) install.packages("readr")
if(!require("data.table")) install.packages("data.table")
if(!require("microbenchmark")) install.packages("microbenchmark")



# cargar librerias ------

library(curl)
library(dplyr)
library(purrr)
library(data.table)
library(microbenchmark)
library(stringr)

# cargar funciones ------

source("tarea_final/nicolas_berho_montalvo/functions.R")

# EJERCICIO 1 -------

# opciones ----

#curl_options(timeout = 100000000000)

# urls -------

urls <- c("https://www.ine.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2021/esi-2021---personas.csv?sfvrsn=d03ae552_4&download=true",
          "https://www.ine.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2020/esi-2020---personas.csv?sfvrsn=fb1f7e0c_4&download=true",
          "https://www.ine.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2019/esi-2019---personas.csv?sfvrsn=9eb52870_8&download=true",
          "https://www.ine.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2018/esi-2018---personas.csv?sfvrsn=a5de2b27_6&download=true",
          "https://www.ine.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2017/esi-2017---personas.csv?sfvrsn=d556c5a1_6&download=true",
          "https://www.ine.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2016/esi-2016---personas.csv?sfvrsn=81beb5a_6&download=true"
)

# convertir a nombres de archivos ------

archivos <- extract_name(urls)

# descargar data -----

pmap(list(urls, archivos, 'data'), download_esi_data)


# EJERCICIO 2 -------

# cargar data a R ------

esis <- map(archivos, read_esi_data)
names(esis) <- paste0('esi_', str_extract(archivos, '\\d{4}'))


# EJERCICIO 3 --------

# añadir columna de version -----

esis <- map2(esis, names(esis), set_esi_version)

# limpiar columnas esis -----

esis <- map(esis, get_cols_esi)

# compilar en un unico df ------

esi <- bind_rows(esis)

# Tabla 3.1 -------

esi %>% 
  group_by(version) %>% 
  summarise(n_personas = length(unique(idrph)),
            n_hogares = length(unique(id_identificacion)))

# Tabla 3.2 -------

esi %>% 
  group_by(id_identificacion) %>%
  slice(1) %>% 
  ungroup() %>% 
  group_by(version) %>% 
  summarise(minimo = min(fact_cal_esi),
            maximo = max(fact_cal_esi),
            media = mean(fact_cal_esi),
            mediana = median(fact_cal_esi),
            p10 = quantile(fact_cal_esi,0.1),
            p90 = quantile(fact_cal_esi,0.9)
            )

# Tabla 3.3 -------

esi %>% 
  group_by(version, estrato) %>% 
  summarise(n_conglomerados = length(unique(conglomerado))) %>% 
  filter(n_conglomerados == 1) %>% 
  print(n=25)

# Tabla 3.4 -------

esi %>% 
  group_by(version) %>%
  mutate(ing_expandidos = ing_t_p*fact_cal_esi) %>% 
  summarise(minimo = min(ing_expandidos),
            maximo = max(ing_expandidos),
            media = mean(ing_expandidos),
            mediana = median(ing_expandidos),
            p10 = quantile(ing_expandidos,0.1),
            p90 = quantile(ing_expandidos,0.9)
  )




# EJERCICIO 4 --------

# benchmark 4.1 ------

map(names(esis), function(v) mean(esi$ing_t_p[esi$version %in% v]*
                                    esi$fact_cal_esi[esi$version %in% v]))

results4_1 <- microbenchmark(
  map.test = map(names(esis), function(v) mean(esi$ing_t_p[esi$version %in% v]*
                                                 esi$fact_cal_esi[esi$version %in% v])),
  times = 50)


# benchmark 4.2 ------

esi %>% 
  group_by(version) %>%
  summarise(media = mean(ing_t_p*fact_cal_esi))

results4_2 <- microbenchmark(
  pipe.test = esi %>% 
    group_by(version) %>%
    summarise(media = mean(ing_t_p*fact_cal_esi)),
  times = 50)


# benchmark 4.3 ------

esi_dt <- data.table(esi)

map(names(esis), mean_dt)

results4_3 <- microbenchmark(
  purrr.datatable.test = map(names(esis), mean_dt),
  times = 50)


# benchmark 4.4 ------

esi_dt[,mean(ing_t_p*fact_cal_esi), by = version]

results4_4 <- microbenchmark(
  datatable.test = esi_dt[,mean(ing_t_p*fact_cal_esi), by = version],
  times = 50)

# comparacion -----

results4_1
results4_2
results4_3
results4_4


'Los resultados son algo inestables pero tiende a dar que la opción de data.table
es la más rapida. Se aumentan las repeticiones a 50 para obtener una métrica más
robusta. Se confirma con esto que la cuarta alternativa con data.table es la más rapida.'