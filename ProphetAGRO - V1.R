

#########################################################################################
#####                       Algoritmo Prophet para predecir                         #####
#####                       el comportamiento de los precios                        #####
#####                             en Productos AGRO                                 #####
#####  Desarrollado por: Daniel Mauricio Rojas - Equipo Machine Learning de la DDD  #####
#####                 Departamento Nacional de Planeación - 2017                    #####
#########################################################################################



### Instalacion de Paquetes y posterior Carga de estos ###
install.packages(c("readxl", "ggplot2", "prophet", "reshape2", "dplyr")) #Instalación de librerías 
library(readxl) #Carga de Libreria Readxl
library(prophet) #Carga de Libreria Prophet
library(ggplot2) #Carga de Libreria Ggplot2
library(reshape2) #Carga de Libreria Reshape2
library(dplyr)    #Carga de Libreria Dplyr


### Se establece directorios de trabajo principales y carga de base de datos
setwd("~/") 
carpeta <- "FORECASTING_AGRO"
dir.create(carpeta, showWarnings = FALSE) 
mainDir <- paste("~/",carpeta, sep="") 
setwd(mainDir)
dataset <- read_excel("~/DATASET.xlsx") 


#### Se garantiza que las variables precio y fecha esten bajo formato numerico y POSIX 
dataset$Precio_kg <- as.numeric(dataset$Precio_kg)
dataset$Fecha <- as.POSIXct(dataset$Fecha, format="%Y-%m-%d")
ciudades <- unique(dataset$Central)

#### Se crea la funcion prediccion, encarga de recibir el vector precio de un producto, 
#### el nombre del producto, la central, la lista de precios y el corte de entrenamiento

prediccion <- function(vector_prod, nom_prod, ciudad, precioscsv, por){
#### Se Crean 2 datasets, el de entrenamiento y el de testeo del modelo
  dp <- data.frame(Fecha = precioscsv[1:round((length(precioscsv$Fecha))*por,0),"Fecha"], Precio = precioscsv[1:round((length(precioscsv$Fecha))*por,0),nom_prod])
  dr <- data.frame("Fecha" = precioscsv$Fecha, "Precio" = vector_prod)
  
#### Se homologan los nombre de los vectores para que sean iguales en ambas bases  
  colnames(dp) <- c("Fecha", "Precio")
  colnames(dr) <- c("Fecha", "Precio")
#### Se indica que si hay valores en 0 los elimine y los deje como Not Available 
  dp$Precio[dp$Precio==0] <- NA
  dr$Precio[dr$Precio==0] <- NA

#### Se crean los dos vectores con los nombres necesarios para iniciar el modelado
  ds <- dp$Fecha
  y <- dp$Precio
  df <- data.frame(ds, y)
##  Inicio de la prediccion del modelo con estacionalidad anual activada
  x <- prophet(df, yearly.seasonality=TRUE)
## Se crea un data set con las fechas que no se encuentran dentro del set de entrenamiento 
## hasta la fecha que indica el sistema es el dia de ejecucion del codigo
## Se procede a predecir los datos del precio con base al modelo generado para las fechas creadas en future
  future <- make_future_dataframe(x, periods = (as.numeric(Sys.Date() - x$history.dates[length(x$history.dates)]))) ##131
  forecast <- predict(x, future)
  df2 <- df
  colnames(df2) <- c("Fecha", "Precio")
  dr2 <- dr 
  datos <- rbind(df2, dr2)
  forecast$ds2 <- as.POSIXct(forecast$ds, format="%Y-%m-%d")
### Se procede a graficar la serie de tiempo
  file_name = paste("Producto - ", nom_prod, ".png", sep = "") ##717
  titulo <- paste(nom_prod,"en", ciudad)
  ggplot(aes(x = Fecha, y =  Precio), data = datos)  + ggtitle(titulo) + geom_point() +
    geom_line(aes(x = ds2, y = yhat),stat = "identity", data = forecast, color = c(rep("red", (round((length(precioscsv$Fecha))*por,0)+1)), rep("blue", nrow(forecast) - (round((length(precioscsv$Fecha))*por,0)+1)))) +  
    geom_line (aes(x = ds2, y = yhat_upper), colour='grey', linetype="dotted", data = forecast, stat='identity')  +
    geom_line (aes(x = ds2, y = yhat_lower), colour='grey', linetype="dotted", data = forecast, stat='identity')  + 
    theme_bw()
  
### Se Guarda el resultado de la grafica
  file_name <- gsub("\\*", "", file_name)
  ggsave(file_name)
}
### Se establece el porcentaje de entrenamiento del modelo
por <- 0.8  

### Se inicia el ciclo por ciudades
for(j in 1:length(ciudades)){
### Se crea la carpeta por cada ciudad
  setwd(mainDir)
  ciudad <- ciudades[j]
  subDir <- ciudad
  dir.create(file.path(mainDir, subDir), showWarnings = FALSE)
  setwd(file.path(mainDir, subDir))
## Se genera un pivot table organizando por ciudad los productos y sus precios por cada fecha
  precioscsv <- dataset %>% filter(Central == ciudad) %>% dcast(Fecha ~ Producto, sum, value.var= "Precio_kg", na.rm=TRUE )
## Se saca la lista de nombres de cada columna (es decir cada producto)  
  zx <- colnames(precioscsv)
  zx <- zx[2:length(zx)]
## Se inicia el ciclo para llamar a la funcion creada por cada producto
  for(i in 1:length(zx)){
    try(prediccion(precioscsv[,i + 1], zx[i], ciudad, precioscsv, por), FALSE)
  }  
}

## Se repite el proceso anterior pero con el promedio de todo el pais
setwd(mainDir)
ciudad <- "Todo el País"
subDir <- ciudad
dir.create(file.path(mainDir, subDir), showWarnings = FALSE)
setwd(file.path(mainDir, subDir))
preciosce <- dataset %>% dcast(Fecha ~ Producto, mean, value.var= "Precio_kg", na.rm=TRUE )
zx <- colnames(preciosce)
zx <- zx[2:length(zx)]
for(i in 1:length(zx)){
  ciudad
  zx[i]
  try(prediccion(preciosce[,i + 1], zx[i], ciudad, preciosce, por), FALSE)
}  


## Fin del Codigo ##
## Version 1.0 -> Ultima Modificación 2 de Junio de 2017 
