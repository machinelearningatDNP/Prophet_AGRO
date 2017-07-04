#install.packages(c("readxl", "ggplot2", "prophet", "reshape2", "dplyr"))
library(readxl)
library(prophet)
library(ggplot2)
library(reshape2)
library(dplyr)
mainDir <- "C:/Users/darojas/Documents/AGRO"
setwd(mainDir)
#dataset <- read_excel("C:/Users/darojas/Documents/AGRO/DATA.xlsx")
dataset$Precio_kg <- as.numeric(dataset$Precio_kg)
ciudades <- unique(dataset$Central)
productos <- unique(dataset$Producto)
por <- 0.95 
tiem <- 90
rta <- (readline(prompt = 'Digite 1 para organizar las preddicones por ciudades o 2 por productos: '))
i=1
j=1
vector_prod <- precioscsv[,i + 1]
nom_prod <- zx[i]
grafico <- function(vector_prod, nom_prod, ciudad, precioscsv, por, tiem){
  dp <- data.frame(Fecha = precioscsv[1:round((length(precioscsv$Fecha))*por,0),"Fecha"], Precio = precioscsv[1:round((length(precioscsv$Fecha))*por,0),nom_prod])
  dr <- data.frame("Fecha" = precioscsv$Fecha, "Precio" = vector_prod)
  colnames(dp) <- c("Fecha", "Precio")
  colnames(dr) <- c("Fecha", "Precio")
  ##qplot(Fecha, Precio , data = dr)
  ##qplot(Fecha, Precio , data = dp)
  dp$Precio[dp$Precio==0] <- NA
  dr$Precio[dr$Precio==0] <- NA
  ds <- dp$Fecha
  y <- dp$Precio
  df <- data.frame(ds, y)
  #qplot(ds, y, data=df)
  x <- prophet(df, yearly.seasonality=TRUE, weekly.seasonality = TRUE, interval.width = por )
  future <- make_future_dataframe(x, periods = (as.numeric(Sys.Date() - x$history.dates[length(x$history.dates)]))+tiem) ##131
  forecast <- predict(x, future)
  #tail(forecast[c('ds','yhat','yhat_lower','yhat_upper')])
  #plot(x, forecast)
  #plot(dr)
  df2 <- df
  colnames(df2) <- c("Fecha", "Precio")
  dr2 <- dr 
  datos <- rbind(df2, dr2)
  forecast$ds2 <- as.POSIXct(forecast$ds, format="%Y-%m-%d")
  #qq<- prophet_plot_components(x, forecast)
#savePlot(filename=nom_prod, type="png")
file_name = paste("Producto - ", nom_prod, ".png", sep = "") ##717
titulo <- paste(nom_prod,"en", ciudad)
ggplot(aes(x = Fecha, y =  Precio), data = datos)  + ggtitle(titulo) + geom_point() +
  geom_line(aes(x = ds2, y = yhat),stat = "identity", data = forecast, color = c(rep("red", (round((length(precioscsv$Fecha))*por,0)+1)), rep("blue", nrow(forecast) - (round((length(precioscsv$Fecha))*por,0)+1)))) +  
  geom_line (aes(x = ds2, y = yhat_upper), colour='grey', linetype="dotted", data = forecast, stat='identity')  +
  geom_line (aes(x = ds2, y = yhat_lower), colour='grey', linetype="dotted", data = forecast, stat='identity')  + 
  theme_bw()
file_name <- gsub("\\*", "", file_name)
ggsave(file_name)
}

if (rta=="1"){
  setwd(mainDir)
  dir.create(file.path(mainDir,"Ciudades"), showWarnings = FALSE)
  setwd(file.path(mainDir,"Ciudades"))
  mainDir <- paste(mainDir, "/Ciudades/", sep="")
  for(j in 1:length(ciudades)){
    setwd(mainDir)
    ciudad <- ciudades[j]
    dir.create(file.path(mainDir, ciudad), showWarnings = FALSE)
    setwd(file.path(mainDir, ciudad))
    precioscsv <- dataset %>% filter(Central == ciudad) %>% dcast(Fecha ~ Producto, sum, value.var= "Precio_kg", na.rm=TRUE )
    zx <- colnames(precioscsv)
    zx <- zx[2:length(zx)]
    for(i in 1:length(zx)){
      try(grafico(precioscsv[,i + 1], zx[i], ciudad, precioscsv, por, tiem), FALSE)
    }  
  }
}else {
  setwd(mainDir)
  dir.create(file.path(mainDir,"Productos"), showWarnings = FALSE)
  setwd(file.path(mainDir,"Productos"))
  mainDir <- paste(mainDir, "/Productos/", sep="")
  for(j in 1:length(productos)){
    setwd(mainDir)
    produc <- productos[j]
    subDir <- produc
    dir.create(file.path(mainDir, subDir), showWarnings = FALSE)
    setwd(file.path(mainDir, subDir))
    precioscsv <- dataset %>% filter(Producto == produc) %>% dcast(Fecha ~ Central, sum, value.var= "Precio_kg", na.rm=TRUE )
    zx <- colnames(precioscsv)
    zx <- zx[2:length(zx)]
    for(i in 1:length(zx)){
      try(grafico(precioscsv[,i + 1], zx[i], produc, precioscsv, por, tiem), FALSE)
    }  
  }
}
todcon <- function(dataset, por, tiem, rta, ciudades){
setwd(mainDir)
ciudad <- "Todo el País"
dir.create(file.path(mainDir, ciudad), showWarnings = FALSE)
setwd(file.path(mainDir, ciudad))
preciosce <- dataset %>% dcast(Fecha ~ Producto, mean, value.var= "Precio_kg", na.rm=TRUE )
zx <- colnames(preciosce)
zx <- zx[2:length(zx)]
for(i in 1:length(zx)){
  ciudad
  zx[i]
  try(grafico(preciosce[,i + 1], zx[i], ciudad, preciosce, por, tiem), FALSE)
}  
}
