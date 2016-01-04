###################################
###### Librerias utilizadas #######
###################################
library(RCurl)
library(stringr)
library(plyr)
library(tidyr)
library(R.utils)

###################################
### Obtencion de URLs
###################################
data <- read.csv("https://raw.githubusercontent.com/lromang/MiningDatosGob/master/Datasets/MAT.csv",
                stringsAsFactors = FALSE)

urls <- data$rec_url

writeLines(urls, "urls.txt")
###################################
### Verificacion de codigo
###################################
urlstatus <- list()
for(i in 1:length(urls)){
    instr  <- paste0("curl -k -o /dev/null --silent --head --write-out '%{http_code}' ",
                    urls[i])
    ## Try with timeout
    tryCatch({
        evalWithTimeout({
            code   <- system(instr)
        }, timeout = 2);
    }, TimeoutException = function(ex) {
        code <- "000"
        cat("Timeout. Skipping.\n");
})
    urlstatus[[i]] <- c(urls[i], code)
    print(i)
}


###################################
### Clasificación de ligas
###################################
urls_code <- read.csv("urlstatus.csv", stringsAsFactors = FALSE,
                     header = FALSE)
names(urls_code) <- c("url","code")

## Función para clasificar una liga
change_lable <- function(lable){
    class <- "no disponible"
    if(str_detect(lable, "20[0-3]") |
       str_detect(lable, "406") |
       str_detect(lable, "300")     |
       str_detect(lable, "30[2-7]") |
       str_detect(lable, "50.")
       ){
        class <- "disponible"
    }
    class
}

## Función para clasificar una columna de ligas
change_lable_mult <- function(col){
    laply(col, function(t) t <- change_lable(t))
}

## Clasificacion de ligas
urls_code$status <- change_lable_mult(urls_code$code)

###################################
### Escribir resultados
###################################
write.csv(urls_code, "urls_class.csv", row.names = FALSE)
