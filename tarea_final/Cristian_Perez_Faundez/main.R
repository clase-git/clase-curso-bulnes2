library(dplyr)
library(purrr)
library(readr)
library(tidyverse)
base_encuesta<-read.csv2("C:\\Users\\caper\\OneDrive\\Escritorio\\Curso R\\Ultima clase\\data_reportería\\data\\ene-2022-01-def.csv")
esi_2016<-read.csv("C:\\Users\\caper\\OneDrive\\Escritorio\\Curso R\\Cristian_Perez_Faundez\\esi-2016---personas.csv")
esi_2017<-read.csv("C:\\Users\\caper\\OneDrive\\Escritorio\\Curso R\\Cristian_Perez_Faundez\\esi-2017---personas.csv")
esi_2018<-read.csv("C:\\Users\\caper\\OneDrive\\Escritorio\\Curso R\\Cristian_Perez_Faundez\\esi-2018---personas.csv")
esi_2019<-read.csv("C:\\Users\\caper\\OneDrive\\Escritorio\\Curso R\\Cristian_Perez_Faundez\\esi-2019---personas.csv")
esi_2020<-read.csv("C:\\Users\\caper\\OneDrive\\Escritorio\\Curso R\\Cristian_Perez_Faundez\\esi-2020---personas.csv")
esi_2021<-read.csv("C:\\Users\\caper\\OneDrive\\Escritorio\\Curso R\\Cristian_Perez_Faundez\\esi-2021---personas.csv")
source("C:\\Users\\caper\\OneDrive\\Escritorio\\Curso R\\Cristian_Perez_Faundez\\functions.R")

#Ejercicio 1

urls <- c("https://www.ine.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2021/esi-2021---personas.csv?sfvrsn=d03ae552_4&download=true",
          "https://www.ine.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2020/esi-2020---personas.csv?sfvrsn=fb1f7e0c_4&download=true",
          "https://www.ine.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2019/esi-2019---personas.csv?sfvrsn=9eb52870_8&download=true",
          "https://www.ine.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2018/esi-2018---personas.csv?sfvrsn=a5de2b27_6&download=true",
          "https://www.ine.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2017/esi-2017---personas.csv?sfvrsn=d556c5a1_6&download=true",
          "https://www.ine.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2016/esi-2016---personas.csv?sfvrsn=81beb5a_6&download=true"
)

# Luego, utiliza purrr para aplicar esta función a cada URL en el vector 'urls'
file_names <- map_chr(urls, extract_name)

# Imprime los nombres de archivo extraídos
print(file_names)


#Guardar las descargas en el directorio de trabajo

map2(urls,file_names,dowload_esi_data)

#Ejercicio 2

file_names <- map_chr(urls, extract_name)

#Directorio de archivos csv
carpeta <- "C:\\Users\\caper\\OneDrive\\Escritorio\\Curso R\\Cristian_Perez_Faundez"  

# Obtener una lista de todos los archivos en la carpeta
archivos_en_carpeta <- list.files(path = carpeta, pattern = "\\.csv$", full.names = TRUE)

# Abrir de manera masiva los archivos creados
lista_de_datos <- map2(archivos_en_carpeta, file_names, read_esi_data)

# Ver un data frame específico en la pestaña "Data" (por ejemplo, el primer data frame)
View(lista_de_datos[[1]])

#Ejercicio 3
esi_2016_acotada<-esi_2016 %>% 
  mutate(version="esi_2016") %>% 
  select(idrph,id_identificacion,version,fact_cal_esi,ing_t_p)

esi_2017_acotada<-esi_2017 %>% 
  mutate(version="esi_2017") %>% 
  select(idrph,id_identificacion,version,fact_cal_esi,ing_t_p)

esi_2018_acotada<-esi_2018 %>% 
  mutate(version="esi_2018") %>% 
  select(idrph,id_identificacion,version,fact_cal_esi,ing_t_p)

esi_2019_acotada<-esi_2019 %>% 
  mutate(version="esi_2019") %>% 
  select(idrph,id_identificacion,version,fact_cal_esi,ing_t_p)

esi_2020_acotada<-esi_2020 %>% 
  mutate(version="esi_2020") %>% 
  select(idrph,id_identificacion,version,fact_cal_esi,ing_t_p)

esi_2021_acotada<-esi_2021 %>% 
  mutate(version="esi_2021") %>% 
  select(idrph,id_identificacion,version,fact_cal_esi,ing_t_p)
#Unir las bases_primera solicitud del ejercicio
esi_tabla<-rbind(esi_2016_acotada,esi_2017_acotada,esi_2018_acotada,esi_2019_acotada,esi_2020_acotada,
                 esi_2021_acotada)

#Unir las bases_segunda solicitud del ejercicio
esi_facto_expansion<-esi_tabla %>%
  group_by(version) %>% 
  summarise(minimo=min(fact_cal_esi),maximo=max(fact_cal_esi),median(fact_cal_esi),p10=quantile(fact_cal_esi,probs = 0.10),
            p90=quantile(fact_cal_esi,probs = 0.90))
  
#Unir las bases_cuarta solicitud del ejercicio
esi_facto_ingresos<-esi_tabla %>%
  group_by(version) %>% 
  summarise(minimo=min(ing_t_p),maximo=max(ing_t_p),median(ing_t_p),p10=quantile(ing_t_p,probs = 0.10),
            p90=quantile(ing_t_p,probs = 0.90))
