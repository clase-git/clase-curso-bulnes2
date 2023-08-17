
# ejercicio 1 -----

# creamos funcion
get_cv <- function(x, na.rm = F){
  sd(x, na.rm = na.rm) / mean(x, na.rm = na.rm)
}

# testeamos
get_cv(1:10)
get_cv(c(1,2,3,6,8))
get_cv(c(1,2,3,6,8,NA))
get_cv(c(1,2,3,6,8,NA), na.rm = T)


# ejercicio 2 ------

# creamos tribble
df <- tibble::tribble(~calle,              ~numero,      ~depto,
              "calle Hemingway",   "num 345",    "depto. 345",
              "av. Albert Camus",  "número 123", "123",
              "Manuel Rojas",      "234",        "departamento 231",
              "Nicanor Parra",     "678",        NULL
) 

# creamos funcion
build_address <- function(calle, numero, depto = NULL){
  
  # to lower case
  calle <- stringr::str_to_lower(calle)
  numero <- stringr::str_to_lower(numero)
  
  # manejo de depto para casos NULL y NA
  if(is.null(depto[[1]])){
    depto <- ""
  } 
  
  if(length(depto) > 0 && is.na(depto[[1]])){
    depto <- ""
  } 
  
  # limpiamos texto calle
  calle <- stringr::str_remove_all(calle, "(calle)|(avenida)|(\\bave?\\b)|(pasaje)|(ps?je)")
  calle <- stringr::str_remove_all(calle, "\\.")
  calle <- stringr::str_remove_all(calle, "^ +")
  
  # limpiamos texto numero
  numero <- stringr::str_remove_all(numero, ' *')
  numero <- stringr::str_extract_all(numero, '\\d+')[[1]]
  
  # acciones según si es o no depto
  if(depto == ""){
    address <- paste0(calle, ' ', numero)
  } else {
    depto <- stringr::str_remove_all(depto, ' *')
    depto <- stringr::str_extract_all(depto, '\\d+')[[1]]
    address <- paste0(calle, ' ', numero, ', depto. ', depto)
  }
  
  # devuelve resultado
  return(address)
  
}


# testeamos general (tres argumentos)

direcciones <- list()

for(i in 1:nrow(df)){
  direcciones[[i]] <- build_address(df$calle[i], df$numero[i], df$depto[i])
}

df$direccion <- unlist(direcciones)

df

# testeamos con dos argumentos (se verifica que tercer argumento es opcional)

build_address(df$calle[1], df$numero[1])
