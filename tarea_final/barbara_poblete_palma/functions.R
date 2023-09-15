# FUNCIONES TAREA FINAL



# Ejercicio 1 ####

# Función que tiene como input una url y devuelve el nombre del archivo

extract_name <- function(url){
  url %>% 
  str_extract("(esi-[[:digit:]]{4}---personas.csv)") 
}



# Función para descargar archivos y colocarlos en la carpeta data

download_esi_data <- function(url, file_name, directory = "data") {
  file_path <- file.path(directory, file_name)
  download.file(url, file_path, mode = "wb")
}





# Ejercicio 2 ####

# Función para abrir los archivos

read_esi_data <- function(file_path, separator = ',') {
  data <- read.table(file_path, sep = separator, header = TRUE, stringsAsFactors = FALSE)
  return(data)
}





# Ejercicio 3 ####

# Función para obtener tabla del primer enunciado

tabla_n1 <- function(lista_data) {
  
  data_table <- as.data.table(lista_data)
  
  resultado <- data_table[, .(n_personas = length(idrph), n_hogares = length(unique(id_identificacion)))]
  
  resultado <- as.data.frame(resultado)
  
  return(resultado)
}



#---



# Función para filtrar por datos únicos de hogar, que toma la primera observación de cada hogar

filter_h <- function(lista_data) {
lista_data %>%
    group_by(id_identificacion) %>%
    filter(row_number() == 1) %>%
    ungroup()
}



# Función para obtener tabla del segundo enunciado

tabla_n2 <- function(lista_data) {
  
  data_table <- as.data.table(lista_data)
  
  resultado <- data_table[, .(min_fexp = min(fact_cal_esi, na.rm = TRUE), max_fexp = max(fact_cal_esi, na.rm = TRUE), 
         mean_fexp = mean(fact_cal_esi, na.rm = TRUE), median_fexp = median(fact_cal_esi, na.rm = TRUE), 
         p10_fexp = quantile(fact_cal_esi, probs = 0.10, na.rm = TRUE), p90_fexp = quantile(fact_cal_esi, probs = 0.90, na.rm = TRUE))]
  
  resultado <- as.data.frame(resultado)
  
  return(resultado)
}



#---


# Función para obtener tabla del tercer enunciado

tabla_n3 <- function(lista_data) {
  
  data_table <- as.data.table(lista_data)
  
  resultado <- data_table[, .(unico_c = sum(uniqueN(conglomerado) == 1)), by = estrato
                          ][unico_c == 1
                            ][, .(n_estrato = sum(unico_c == 1))]
  
  resultado <- as.data.frame(resultado)
  
  return(resultado)
}



#---



# Función para obtener tabla del cuarto enunciado

tabla_n4 <- function(lista_data) {
  
  data_table <- as.data.table(lista_data)
  
  resultado <- data_table[ing_t_p>0, .(min_fexp = min(ing_t_p*fact_cal_esi, na.rm = TRUE), 
                              max_fexp = max(ing_t_p*fact_cal_esi, na.rm = TRUE), 
                              mean_fexp = mean(ing_t_p*fact_cal_esi, na.rm = TRUE)/sum(fact_cal_esi, na.rm = TRUE), 
                              median_fexp = quantile(ing_t_p*fact_cal_esi, probs = 0.5, na.rm = TRUE), 
                              p10_fexp = quantile(ing_t_p*fact_cal_esi, probs = 0.10, na.rm = TRUE), 
                              p90_fexp = quantile(ing_t_p*fact_cal_esi, probs = 0.90, na.rm = TRUE))]
  
  resultado <- as.data.frame(resultado)
  
  return(resultado)
}





# Ejercicio 4 ####

# Función para obtener promedio con data.table y aplicarlo en un map()

mean_datatable <- function(lista_data) {
  
  data_table <- as.data.table(lista_data)
  
  resultado <- data_table[ing_t_p != 0, .(prom_ing = mean(ing_t_p))]
  
  resultado <- as.data.frame(resultado)
  
  return(resultado)
}





