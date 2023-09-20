
# Librerias ---------------------------------------------------------------
# Paquetes a utilizar 
paquetes <- c("tidyverse", "utils", "data.table", "stats", "microbenchmark")

# Instalar paquetes que aún no están instalados
paquetes_instalados <- paquetes %in% rownames(installed.packages())

if (any(paquetes_instalados == FALSE)) {
  install.packages(paquetes[!paquetes_instalados])
}

# Cargar paquetes
invisible(lapply(paquetes, library, character.only = TRUE))




# extract_name ------------------------------------------------------------
# Funcion que se utiliza para extraer el nombres de archivos de la ESI

extract_name <- function(url){
  url %>%                                                                                 # se selecciona el vector con las url
    purrr::map(~.x %>%                                                                    # .x: representa cada elemento dentro url
                 stringr::str_extract(pattern = "\\w{3}(-)\\d+(-){3}[:graph:]+(sv)"))  
}



# download_esi_data -------------------------------------------------------
# Funcion que se utiliza para descargar los archivos de la ESI

download_esi_data <- function(url, file_name, directory){
  
  file_name <- paste0(directory, file_name)
  
  purrr::map2(url, file_name, ~.x %>%                                         # .x: representa cada elemento dentro del vector url
                utils::download.file(destfile = .y, mode = "wb"))             # .y: representa cada elemento dentro del vector file_name
}



# read_esi_data -----------------------------------------------------------
# Funcion que se utiliza para leer los archivos de la ESI. Se utilizan las
# funciones de data.table para optimizar recursos

read_esi_data <- function(ruta){
  data.table::fread(ruta)
}



# fun_mean ----------------------------------------------------------------
# Se crea esta función para utilizar en el ejercicio 4, sección 3
fun_mean <- function(df, variable){
  
  df[, mean_ing_t_p := mean(ing_t_p), by=idrph][
    ,c("mean_ing_t_p")]
  
}


# fun_dt ------------------------------------------------------------------
# Se crea esta función para utilizar en el ejercicio 4, sección 4
# nota: Si solo se agrupa por idrph se corre el riesgo de que los valores de esta 
# columna se repitan entre versiones, con lo cual los resultados serían 
# incongruentes con los de 'lista_tabla_1'
fun_dt <- function(df){
  
  df <- df %>% bind_rows() %>% as.data.table()
  
  df[,
     version_idrph := paste0(version,idrph)][
       ,c("version_idrph", "ing_t_p")][
       ][,
         mean_ing_t_p := mean(ing_t_p),
         by = version_idrph][
           ,c("mean_ing_t_p")]
  
  
}






