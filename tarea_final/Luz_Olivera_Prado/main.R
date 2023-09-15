## TAREA FINAL

## Cargar paquetes

paquetes <- c("dplyr","tidyverse","purrr","data.table","stringr","readr","curl",
              "Hmisc","microbenchmark","hdd","glue","survey","srvyr")


for(p in paquetes) {
  if (!require(p,character.only = TRUE))    install.packages(p);    
  library(p,character.only = TRUE)
}

## Script functions

source(file = "functions.R", encoding = "utf-8")

## Ejercicio 1: 1.1 Descargar.Encontrar nombres de archivos

urls <- 
  c("https://www.ine.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2021/esi-2021---personas.csv?sfvrsn=d03ae552_4&download=true",
    "https://www.ine.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2020/esi-2020---personas.csv?sfvrsn=fb1f7e0c_4&download=true",
    "https://www.ine.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2019/esi-2019---personas.csv?sfvrsn=9eb52870_8&download=true",
    "https://www.ine.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2018/esi-2018---personas.csv?sfvrsn=a5de2b27_6&download=true",
    "https://www.ine.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2017/esi-2017---personas.csv?sfvrsn=d556c5a1_6&download=true",
    "https://www.ine.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2016/esi-2016---personas.csv?sfvrsn=81beb5a_6&download=true")

file_names <- map_chr(urls, extract_name)


## 1.2 y 1.3 Usando funcion "download_esi_data", purrr, las url y el vector de nombres, 
## descargo todos los archivos en una carpeta llamada data

if ( !dir.exists("data") ) dir.create("data")
pmap(list(urls, file_names,"data"),download_esi_data)


## Ejercicio 2: leer archivos."read_esi_data" debe ser capaz de reconocer el tipo de separador 
## y leer el archivo correctamente en todos los casos:

direcciones <- list.files("./data")
direcciones <- paste0("./data/", direcciones)
bases_ESI <- map(direcciones, ~read_esi_data(.x))

## homologar nombres de encuestas en lista de encuestas

a <- str_extract(list.files("./data"), pattern = "[[:alpha:]]{3}-[[:digit:]]{4}")
a <- str_replace_all(a, "-", "_")
names(bases_ESI) <- a
bases_ESI


## Ejercicio 3: obtener datos

options(survey.lonely.psu="certainty")


## 3.1. Tabla que contenga 3 columnas: version, n_personas (idrph) y n_hogares (id_identificacion)

tbl1 <- imap_dfr(bases_ESI, ~tabla3_1(x = .x, nombre = .y))
tbl1


### 3.2. Tabla que contenga mínimo, máximo, media, mediana, p10 y p90 del factor de expansión (fact_cal_esi)

tbl2 <- imap_dfr(bases_ESI, ~tabla3_2(x = .x, nombre = .y))
tbl2

## ¿Se observan pesos de muestreo atípicos? 

## ## Los años 2020 y 2021 es donde se observan aumentos considerables en los percentiles 10, 50 (mediana)
## 90 y 99, lo que puede tener relación con la pandemia para recolectar información a partir de encuestas a hogares, los otros años son similares.


## 3.3. Tabla que contenga el número de estratos (estrato) con una sola unidad primaria de muestro (conglomerado).

tbl3 <- imap_dfr(bases_ESI, ~tabla3_3(x = .x, nombre = .y))
tbl3


## 3.4. Tabla que contenga mínimo, máximo, media, mediana, p10 y p90 de los ingresos del trabajo principal (ing_t_p) 

tbl4 <- imap_dfr(bases_ESI, ~tabla3_4(x = .x, nombre = .y))
tbl4


## Ejercicio 4: mejorando el codigo

## 4.1. Lista de tablas: calcular promedio con herramientas de purrr 

lista_tabs <- 
  imap(bases_ESI, ~.x %>%  
         as_survey_design(ids = conglomerado, 
                          strata = estrato, 
                          weights = fact_cal_esi) %>% 
         filter(ocup_ref==1) %>%  
         summarise(media = survey_mean(ing_t_p, na.rm=T)) %>% 
         select(media) %>% 
         mutate(version = .y) %>% 
         select(version, everything()))
lista_tabs

## 4.2 Tablas apiladas: calcular promedio con group_by() %>% summarise()

tabs_ap <-  
  lapply(bases_ESI, function(x) select(x,
                                          ano_trimestre, 
                                          ing_t_p, 
                                          conglomerado, 
                                          estrato, 
                                          fact_cal_esi,
                                          ocup_ref)) %>%
  bind_rows() %>%
  as_survey_design(id = conglomerado, strata = estrato, weights = fact_cal_esi) %>%
  filter(ocup_ref==1) %>%
  group_by(ano_trimestre) %>%
  summarise( media = survey_mean(ing_t_p, rm.na = T))
tabs_ap

## 4.3. Lista de tablas: calcular promedio con herramientas de purrr, utilizando una función creada,
## que utilice data.table.

lista_tabs_dt <- map(bases_ESI, ~as.data.table(.x))

## Calcular promedio de prueba con herramietnas de data.table 
## (se usa weighted mean)

lista_tabs_dt_media_ingresos <- map(lista_tabs_dt, ~ingr_prom_trab(.x))
lista_tabs_dt_media_ingresos


## 4.4. Tablas apiladas, utilizando data table

tabs_ap2 <-  
  lapply(bases_ESI, function(x) select(x,
                                          ano_trimestre, 
                                          ing_t_p, 
                                          conglomerado, 
                                          estrato, 
                                          fact_cal_esi,
                                          ocup_ref)) %>%
  bind_rows()

## Paso dataframe a data.table 
dt_ap <- as.data.table(tabs_ap2)

media_dt_ap <- 
  dt_ap [ocup_ref == 1 ,
                      weighted.mean(x = ing_t_p, 
                                    w = fact_cal_esi,
                                    fact_cal_esi, 
                                    na.rm = T), by = ano_trimestre]
media_dt_ap


## ¿Existen diferencias importantes entre las distintas estrategias?¿Hay alguna más eficiente que otra? ¿Usar "group_by" versus "map" hace alguna diferencia?

## En mi opinión,  Ocupar data.table es más eficiente, ya que es más rápido en generar los resultados que los otros, y se muestran más ordenado.
