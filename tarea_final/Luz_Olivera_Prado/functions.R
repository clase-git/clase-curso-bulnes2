### TAREA FINAL - functions
### Francisco Gonzalez Prado

### funcion "extract_name", extrae nombres de URLs
extract_name <- function(url){
  file <- str_extract(url, pattern = "[[:alpha:]]{3}-[[:digit:]]{4}---[[:alpha:]]{8}.[[:alpha:]]{3}")
  file <- as.character(file)
  return(file)
}


### funcion "download_esi_data" descarga la base de datos desde la URL
download_esi_data <- function(url,file_name,directory){
  if ( !dir.exists(directory) ) dir.create(directory)
  curl_download(url = url, destfile = glue("{directory}/{file_name}"))
}


### funcion "read_esi_data" lee la bases de datos del directorio
read_esi_data <- function(ruta){
  delim <- hdd::guess_delim(ruta)
  if (delim == ";") a <-fread(ruta, data.table=FALSE, dec=",")
  else a <-fread(ruta, data.table=FALSE)
  }


### funcion tabla3_1

tabla3_1 <-
  function(x, nombre){
    x <- as.tibble({{x}})
    n_personas = as.integer(x %>% group_by(idrph) %>% summarise(n=n()) %>% count())
    n_hogares = as.integer(x %>% group_by(id_identificacion) %>% summarise(n=n()) %>% count())
    version = nombre
    prueba <- as.data.frame(cbind(version, n_personas, n_hogares))
    return(prueba)
  }


### funcion tabla3_2

tabla3_2 <-
  function(x, nombre){
    tbl1 <- {{x}} %>%
      group_by(id_identificacion) %>%
      summarise(fact_cal_esi = mean(fact_cal_esi)) %>%
      ungroup() %>%
      summarise(minimo = min(fact_cal_esi, na.rm = TRUE),
                maximo = max(fact_cal_esi, na.rm = TRUE),
                media = mean(fact_cal_esi, na.rm = TRUE),
                p10 = quantile(fact_cal_esi, probs = 0.1, na.rm = TRUE),
                mediana = quantile(fact_cal_esi, probs = 0.5, na.rm = TRUE),
                p90 = quantile(fact_cal_esi, probs = 0.9, na.rm = TRUE),
                p99 = quantile(fact_cal_esi, probs = 0.99, na.rm = TRUE)) %>%
      mutate(version = nombre) %>%
      select(version, everything())
    return(tbl1)
  }


### funcion tabla3_3

tabla3_3 <-
  function(x, nombre){
    tbl1 <- {{x}} %>%
      group_by(estrato,as.character(conglomerado)) %>%
      tally() %>%
      group_by(estrato) %>%
      tally() %>%
      filter(n ==1) %>%
      ungroup() %>%
      summarise(num_estratos=sum(n)) %>%
      mutate(version = nombre) %>%
      select(version, everything())
    return(tbl1)
  }

### funcion tabla3_4

tabla3_4 <-
  function(x, nombre){
    tbl1 <- {{x}} %>%
      as_survey_design(ids = conglomerado,
                       strata = estrato,
                       weights = fact_cal_esi) %>%
      filter(ocup_ref==1) %>%
      summarise(media = survey_mean(ing_t_p, na.rm=T),
                mediana = survey_median(ing_t_p, na.rm = TRUE),
                quantile = survey_quantile(ing_t_p, quantile = c(0.1, 0.9), na.rm =T),
                min = min(ing_t_p,na.rm =T),
                max = max(ing_t_p, na.rm =T) ) %>%
      select(media, mediana, quantile_q10, quantile_q90, min, max ) %>%
      mutate(version = nombre) %>%
      select(version, everything())
    return(tbl1)
  }

### funcion ingr_prom_trab

ingr_prom_trab <- function(data){
    data[ocup_ref == 1 ,
         weighted.mean(x = ing_t_p,
                       w = fact_cal_esi,
                       fact_cal_esi,
                       na.rm = T)]
  }

