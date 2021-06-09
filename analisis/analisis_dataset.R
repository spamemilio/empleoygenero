source("R/funciones.R")


library(tidyverse)
library(bpa)
library(sf)
library("rnaturalearth")

# Bajamos el datasets de Empleo y Género
url <- "https://raw.githubusercontent.com/cienciadedatos/datos-de-miercoles/c8983df96111941319d9e50975d8f514a7003293/datos/2019/2019-10-16/empleo_genero.csv"
datos <- read_csv(url)

#Capturamos la información por si deja de andar el link
# write_csv(datos, "data/empleogenero.csv")
# 
# datos <- read_csv("data/empleogenero.csv")

# Busco cuantos valores hay informados para el primer año
var_1970_na <- datos %>% 
  filter(!is.na(`1970`))

# Paso a Tidy Data el dataset
datos_ordenados <- datos %>% 
pivot_longer(cols = is_double, names_to  = "año",
             values_to = "valor")

# Guardo las variables existentes
variables <- datos %>% 
  dplyr::select (variable) %>% 
  unique()

# Chequeamos que los años tienen el formato correcto
unique(basic_pattern_analysis(datos_ordenados$año))

# Nos fijamos si todos los valores tiene formato de porcentaje
unique(basic_pattern_analysis(datos_ordenados$valor))
# Varios no lo tienen

# Cargamos un mapeo entre nombre de paises en castellano e inglés para poder vincular con matriz existente
correccion_pais_anglo <- data.frame(
  pais_region = c("Brasil","República Dominicana","Panamá","México",
                  "Perú","Haití"),
  pais_anglo = c("Brazil","Dominican Republic","Panama","Mexico",
                 "Peru","Haiti")
                )
# Bajamos la base con todos los países del mundo
world <- ne_countries(scale = "medium", returnclass = "sf")

# Me quedo con los paises del mundo en inglés
 paises_world <- world %>% 
   dplyr::select(admin) %>% 
 unique()

 #Matriz que vincula paises anglo y español
paises_codigos <- datos_ordenados %>% 
  filter(!codigo_pais_region %in% c("LCN","ESP","WLD")) %>% 
  dplyr::select(pais_region,codigo_pais_region) %>% 
  unique() %>% 
  left_join(correccion_pais_anglo) %>% 
  mutate(admin = ifelse(is.na(pais_anglo), pais_region, pais_anglo)) 


datos_ordenados <- 
  datos_ordenados %>% 
  #corregimos los valores mayores a 100
  mutate(valor=
           unlist(map(datos_ordenados$valor,
                      corregir_porcentaje) )) %>% 
#transformamos el año en numérico
  mutate(year=as.integer(año))

# Corroborando <- datos_ordenados %>% 
#   filter(variable == "empleadores_hombres",
#          pais_region == "República Dominicana",
#          año == 2017)

# Corroborando <- datos_ordenados %>% 
#   filter(
#          pais_region == "Brasil",
#          año == 1976)

#Cuento cantidad de indicadores por país y por año
indicadores_anio_pais <- datos_ordenados %>% 
  # filter(! is.na(valor)) %>% 
  right_join(paises_codigos) %>% 
  group_by(año,admin) %>% 
  summarise(cantidad_indicadores =sum(!(is.na(valor)))) %>% 
  arrange(año)

#agrego paises que no se encuentran en el dataset original para completar
otros <- data.frame(año = as.character(c(1970:2018)),admin=c("Suriname"),
                    cantidad_indicadores=0) %>% 
  bind_rows(
    data.frame(año = as.character(c(1970:2018)),admin=c("Guyana"),
               cantidad_indicadores=0)  )

indicadores_anio_pais <- indicadores_anio_pais %>% 
  bind_rows(otros)

# cantidad de indicadores sin sumarizar por país
na_anio_indicador <- datos_ordenados %>% 
  # filter(! is.na(valor)) %>% 
  group_by(año,variable) %>% 
  summarise(cantidad_indicadores =sum((is.na(valor)))) %>% 
  arrange(año)
 
