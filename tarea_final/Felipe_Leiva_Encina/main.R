
# Setup -------------------------------------------------------------------
# Carga de funciones y librerias
source("functions.R")

# Ejercicio 1:descargar archivos ------------------------------------------

urls <- c("https://www.ine.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2021/esi-2021---personas.csv?sfvrsn=d03ae552_4&download=true",
          "https://www.ine.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2020/esi-2020---personas.csv?sfvrsn=fb1f7e0c_4&download=true",
          "https://www.ine.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2019/esi-2019---personas.csv?sfvrsn=9eb52870_8&download=true",
          "https://www.ine.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2018/esi-2018---personas.csv?sfvrsn=a5de2b27_6&download=true",
          "https://www.ine.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2017/esi-2017---personas.csv?sfvrsn=d556c5a1_6&download=true",
          "https://www.ine.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2016/esi-2016---personas.csv?sfvrsn=81beb5a_6&download=true"
)

# Creamos carpeta data
dir.create("data")

# Extraemos nombres de archivos  
file_names <- extract_name(urls)

# Descargamos los archivos en carpeta data
download_esi_data(urls, file_names, directory="data/")


# Ejercicio 2: leer archivos ----------------------------------------------
# Creamos vector con las rutas de los archivos
rutas <- paste0("data/", file_names)

# Leemos los archivos y dejamos en una lista llamada datos
datos <- purrr::map(rutas, ~.x %>% 
             read_esi_data())


# Ejercicio 3: obtener datos ----------------------------------------------
# Creamos tablas con las características sobre las variables de diseño y sobre 
# la variable principal de ingresos (ing_t_p)

## Tabla 1 -----------------------------------------------------------------
# Creamos una lista con los años
agno <- str_extract_all(file_names, pattern = "\\d{4}")

# Asignamos nombres a cada elemento de la lista
names(datos) <- paste0("esi_",agno)


# Modificamos el la lista 'datos'
datos <- purrr::map2(datos, agno,  ~.x %>%
                        mutate(version = paste0("esi_", .y),                           # creamos columna 'version'
                               id_identificacion = as.character(id_identificacion)))   # dejamos la columna 'id_identificacion' como texto dentro de cada elemento de datos

# Creamos tabla1
tabla1 <- purrr::map(datos, ~.x %>%
                        select(id_identificacion,idrph, version))


## Tabla 2 -----------------------------------------------------------------
tabla2 <- purrr::map2(datos, agno,
                     ~.x %>% 
                       select(id_identificacion, fact_cal_esi) %>% 
                       group_by(id_identificacion) %>%                          
                       summarise(mínimo = min(fact_cal_esi),
                                 máximo = max(fact_cal_esi),
                                 media = mean(fact_cal_esi),
                                 mediana = median(fact_cal_esi),
                                 p10 = quantile(fact_cal_esi, probs = 0.10),
                                 p90 = quantile(fact_cal_esi, probs = 0.90)) %>% 
                       mutate(version = paste0("esi_", .y)) 
)


## Tabla 3 -----------------------------------------------------------------
tabla3 <- purrr::map(datos,
                      ~.x %>% 
                        select(conglomerado, estrato, version) %>% 
                        group_by(conglomerado, estrato) %>% 
                        mutate(n_estrato = n()) %>% 
                        filter(n_estrato == 1)
)

## Tabla 4 -----------------------------------------------------------------
tabla4 <- purrr::map(datos, 
                      ~.x %>% 
                        select(idrph, ing_t_p, fact_cal_esi) %>% 
                        group_by(idrph) %>%                                     # identificador a nivel persona
                        summarise(mínimo = min(ing_t_p)*fact_cal_esi,
                                  máximo = max(ing_t_p)*fact_cal_esi,
                                  media = mean(ing_t_p)*fact_cal_esi,
                                  mediana = median(ing_t_p)*fact_cal_esi,
                                  p10 = quantile(ing_t_p, probs = 0.10)*fact_cal_esi,
                                  p90 = quantile(ing_t_p, probs = 0.90)*fact_cal_esi)
)


# Ejercicio 4: mejorando el código ----------------------------------------
tiempo_ejecucion <- microbenchmark(
  
## 1. Lista de tablas: ----------------------------------------------------
  lista_tabla_1 <- map(datos, ~.x %>% 
                         group_by(idrph) %>% 
                         summarise(mean_ing_t_p = mean(ing_t_p)) %>% 
                         select(mean_ing_t_p))
  ,

## 2. Tablas apiladas -----------------------------------------------------
  tabla_apilada_1 <- datos %>% 
    bind_rows() %>% 
    group_by(version, idrph) %>%                        # Si solo se agrupa por idrph se corre el riesgo de que los valores de esta columna se repitan entre versiones, con lo cual los resultados serían incongruentes con los de 'lista_tabla_1'
    summarise(mean_ing_t_p = mean(ing_t_p)) %>% 
    select(mean_ing_t_p)
  ,

## 3. Lista de tablas: ----------------------------------------------------
  lista_tabla_2 <- map(datos, ~.x %>% 
        fun_mean(., "ing_t_p"))
  ,
## 4. Tablas apiladas -----------------------------------------------------
  tabla_apilada_2 <- fun_dt(datos)

,times = 5)

print(tiempo_ejecucion)


# ¿Existen diferencias importantes entre las distintas estrategias? 
# Si, al revisar los tiempos de ejecución se observan mejores resultados en los
# objetos que se crearon por medio de data.table

# ¿Hay alguna más eficiente que otra? 
# Los objetos creados con data.table son los más eficientes, puntualmente
# el objeto que se genera de forma más eficiente es 'lista_tabla_2'. Por otro 
# lado se observa que tabla_apilada_2 es más eficiente que 'tabla_apilada_1', 
# pero aún asi los tiempos son elevados en comparación a 'lista_tabla_2', esto 
# principalmente por el uso de bind_rows() en estos objetos apilados.

# ¿Usar group_by versus map hace alguna diferencia?
# En terminos de recursos map es más eficiente que gropu_by