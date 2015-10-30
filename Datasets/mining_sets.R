#! /usr/bin/Rscript

## Luis Manuel Román García
## Rutinas para minar el contenido de
## http://busca.datos.gob.mx/#/conjuntos
##-----------------------------
## Librerias utilizadas
suppressPackageStartupMessages(library(XML))
suppressPackageStartupMessages(library(stringr))
suppressPackageStartupMessages(library(RCurl))
suppressPackageStartupMessages(library(rjson))
suppressPackageStartupMessages(library(plyr))
suppressPackageStartupMessages(library(httr))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(stringr))
suppressPackageStartupMessages(library(data.table))
suppressPackageStartupMessages(library(tidyr))
suppressPackageStartupMessages(library(lubridate))
##-----------------------------
source("get_plans.R")

get.edit.inv <- function(url){
    ## Obtiene la informacion de la ultima modificacion al inventario de datos
    ## de cada institucion.
    ## IN
    ## url: la direccion de la pagina de donde se quiere obtener la informacion
    ## OUT
    ## data.frame con la informacion de la ultima modificacion al inventario
    ## de datos de cada institucion.
    inst      <- str_split(url,"/")[[1]][5]
    url_cat   <- get.links.inv(url)[1]
    if(!is.na(url_cat)){
        page      <- getURL(url)
        tree      <- htmlParse(page)
        values    <- xpathApply(tree,
                                path = "//table[@class='expanded-table inventories-list']//td",
                                fun  = xmlValue)
        values    <- unlist(values)
        last.mod  <- data.frame(inst      = inst,
                                url       = url,
                                url_cat   = url_cat,
                                fecha     = to.date(values[3],values[4]),
                                conjuntos = values[5],
                                recursos  = values[6])
    }else{
        last.mod  <- data.frame(inst      = inst,
                                url       = url,
                                url_cat   = NA,
                                fecha     = NA,
                                conjuntos = NA,
                                recursos  = NA)
    }
    last.mod
}


## Instrucción API
## ---------------------------------------
## http://catalogo.datos.gob.mx/api/3/action/
## ---------------------------------------
## Valores recientes
## recently_changed_packages_activity_list
## ---------------------------------------
## Todos los paquetes
## package_list
## ---------------------------------------
## Todos los paquetes todos los recursos
## current_package_list_with_resources
## ---------------------------------------
## Búsqueda personalizada
## resource_search?query=name:District%20Names
## ---------------------------------------
## ---------------------------------------
## ---------------------------------------
## Lectura de datos
## ---------------------------------------
## ---------------------------------------
## ---------------------------------------

#########################################
#########################################
## get_recent_data
#########################################
## Obtiene los datos referenetes a las
## últimas actualizaciones en ckan.
#########################################
get_recent_data <- function(){
    ## Generamos la url de la búsqueda.
    base_url <- "http://catalogo.datos.gob.mx/api/3/action/"
    activity <- "recently_changed_packages_activity_list"
    ## Descargamos en formato JSON.
    recent_act <- fromJSON(getURL(paste0(base_url,activity)))
    ## Almacenamos los resultados en un formato
    ## tipo tabla.
    results <- recent_act$result
    package   <- laply(results,function(t)t <- t$data$package$name)
    modified  <- laply(results,function(t)t <- t$data$package$metadata_modified)
    url       <- laply(results,function(t)t <-
                                            {if(length(t$data$package$url) > 0){t$data$package$url}else{NA}})
    data.frame(title = package, date = modified, url = url)
}


#########################################
#########################################
## get_all_data
#########################################
## Obtiene los datos referenetes a todos
## los conjuntos y todos sus recursos.
#########################################
get_all_data <- function(){
    ## Generamos la url de la búsqueda.
    base_url <- "http://catalogo.datos.gob.mx/api/3/action/"
    activity <- "current_package_list_with_resources"
    ## Descargamos en formato JSON.
    recent_act <- getURL(paste0(base_url,activity))
    recent_act_json <- RJSONIO::fromJSON(recent_act)
    ## Almacenamos los resultados en un formato
    ## tipo tabla.
    results <- recent_act$result
    package   <- laply(results,function(t)t <- t$data$package$name)
    modified  <- laply(results,function(t)t <- t$data$package$metadata_modified)
    url       <- laply(results,function(t)t <-
                                            {if(length(t$data$package$url) > 0){t$data$package$url}else{NA}})
    data.frame(title = package, date = modified, url = url)
}


#########################################
#########################################
## process_all_data
#########################################
## Obtiene los datos referenetes a todos
## los conjuntos y todos sus recursos.
#########################################
process_all_data <- function(){
    file <- str_trim(readLines("set_data.txt"))
    ## Institución
    inst <- file[str_detect(file,"Inst:")] %>%
        str_replace("Inst: ","")
    ## Conjunto
    dataset <- file[str_detect(file,"Dataset:")] %>%
        str_replace("Dataset: ","")
    ## Descripción
    desc <- file[str_detect(file,"Desc:")] %>%
        str_replace("Desc: ","")
    ## Fecha
    date <- file[str_detect(file,"Published:")] %>%
        str_replace("Published: ","")
    ## Recursos
    rec <- file[str_detect(file,"resources:")] %>%
        str_replace("resources: ","")

    ## Juntar resultados.
    data.frame(Inst = inst, Conjunto = dataset, Recursos = rec,  Desc = desc, fecha = date)
}

#####
## Obtención datos
#####
data <- process_all_data()

write.csv(data,
          paste0("info_datos_gob-",
                       today(),
                      ".csv"),
          row.names = FALSE)

#####
## statistics
#####
data$Recursos <- extract_numeric(data$Recursos)
data_table    <- data.table(data)
conjuntos     <- nrow(data_table)
print(paste0( "Conjuntos: ",conjuntos ))
recursos      <- data_table[,sum(Recursos)]
print(paste0( "Recursos: ", recursos ))
print("###################################################")
#####
data_table    <- filter(data_table, Inst != "null")
top_conjuntos <- data_table[,.N, by = Inst]
top_conjuntos <- top_conjuntos[order(top_conjuntos$N,
                                    decreasing = TRUE),
                              ]
print("10 instituciones con más conjuntos")
print(head(top_conjuntos,10))
top_recursos <- data_table[,sum(Recursos), by = Inst]
top_recursos <- top_recursos[order(top_recursos$V1,
                                    decreasing = TRUE),
                            ]
print("10 instituciones con más recursos")
print(head(top_recursos,10))
print("###################################################")
all_plans <- filter(all_plans, dep != "adela-mxabierto")
print(paste0("Planse de apertura: ", length(unique(all_plans$dep))))
