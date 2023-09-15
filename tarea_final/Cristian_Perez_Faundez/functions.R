

#funcion ejercicio 1:extraer nombre de archivos
extract_name <- function(url) {
  name <- str_extract(url, "[^/]+\\.csv")
  return(name)
}

#Funcion ejercicio 1: para descargar archivos
dowload_esi_data<-function(url,file_name){
  download.file(url,destfile=file_name)
  
  
}

#Funcion ejercicio 2:
read_esi_data<-function(ruta,file_name){
  file_name<-read.csv(ruta)
  return(file_name)
}