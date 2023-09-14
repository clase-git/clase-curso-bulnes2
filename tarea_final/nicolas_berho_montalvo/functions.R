# extract_name ------

extract_name <- function(url){
  
  name <- stringr::str_extract(url, 'esi-20..-+personas.csv')
  
  return(name)
  
}

#extract_name(urls)

# download_esi_data ------

download_esi_data <- function(url, file_name, directory){
    
    directorio <- paste0('tarea_final/nicolas_berho_montalvo/',directory,'/')
    
    descargados <- list.files(directorio)
    
    if(!file_name %in% descargados){
    curl::curl_download(url = url, destfile = paste0(directorio,file_name), quiet = F)
    print(paste0('Se ha descargado ',file_name, ' en ', directorio))
    } else {
      print(paste0('Ya se encuentra ',file_name, ' en ', directorio))
      
    }
    
}



# read_esi_data -----

read_esi_data <- function(archivo){
  
  archivo <- paste0('tarea_final/nicolas_berho_montalvo/data/',archivo)
  
  lineas <- readr::read_lines(file(archivo),n_max = 3)
  
  close(file(archivo))
  
  if(any(stringr::str_detect(lineas,','))){
    
    df <- readr::read_csv(archivo)
    
  } else {
    
    df <- readr::read_csv2(archivo)
    
  }
  
  return(df)
  
}


# get_cols_esi ----

get_cols_esi <- function(esi){
  
  vars <- c('idrph','id_identificacion','version',
            'fact_cal_esi','estrato','conglomerado','ing_t_p')
  
  esi <- esi[,vars]
  
  return(esi)
  
}


# set_esi_version ----

set_esi_version <- function(esi, version){

  esi$version <- version

  return(esi)

}


# media con data.table ------

mean_dt <- function(v){
  
  esi_dt[version == v,mean(ing_t_p*fact_cal_esi)]
  
}