library(rvest)
library(readxl)
library(gdata)
library(WriteXLS)
mainDir <- "C:/Users/darojas/Documents/AGRO"
setwd(mainDir)
url <- "http://www.dane.gov.co/index.php/estadisticas-por-tema/agropecuario/sistema-de-informacion-de-precios-sipsa/componente-precios-mayoristas"
page <- read_html(url)
a <- html_nodes(page, 'div .t3-content a')
aa <-html_attr(a, "href")
l <-c()
ll <- c()
fie <- c()
urlb <- "http://www.dane.gov.co"
chars <- ".xls"
con<- grepl(chars, aa)
for (j in 1:length(aa)){
  if (con[j]==TRUE){
    l[j] <- paste(urlb,aa[j],sep="")
  }
}
l <- l[!is.na(l)]
dataset <- read_excel("C:/Users/darojas/Documents/AGRO/DATA.xlsx")
dates <- dataset$Fecha
meses <- c("enero","febrero", "marzo", "abril", "mayo", "junio", "julio", "agosto", "septiembre", "octubre", "noviembre", "diciembre")
md <- (as.numeric(format(dates, "%Y%m%d")))
lastime <- as.Date(dates[which.max(md)])+1
today <- Sys.Date()
misingdays <- as.Date(as.Date(lastime):as.Date(today), origin="1970-01-01")
misingdays <- misingdays[order(as.Date(misingdays, format="%Y-%m-%d"), decreasing = TRUE)]
fds <- misingdays[grepl("(sábado|domingo)", weekdays(misingdays))]
misingdays <- misingdays[!(misingdays %in% fds)]

for (i in 1:length(misingdays)){
  fd <- misingdays[i]
  md <- meses[as.numeric(substr(fd, 6, 7))]
  dd <- as.numeric(substr(fd, 9, 10))
  ad <- as.numeric(substr(fd, 1, 4))
  comparacion <- paste(md,dd,ad,sep="_")
  ll[i] <- as.Date(fd, origin="1970-01-01")
  for (j in 1:length(l)){
    re=l[j]
    con<- grepl(comparacion, re)
    if (con==TRUE){
      urlxsl <- re
      fecha <- paste(comparacion,".xls",sep="")
      download.file(urlxsl, destfile=fecha, mode="wb")
      fie[i] <- fecha
    }else{
      next
    }  
  }
}
deef <- data.frame(Fecha=as.Date(character()), 
                   Central=character(), 
                   Producto=character(), 
                   Precio_kg=as.numeric(character())
                   
)
fie <- fie[!is.na(fie)]
centrales <- unique(dataset$Central)
productos <- unique(dataset$Producto)
for (j in 1:length(fie)){
  data <- read.xls(fie[j], verbose=FALSE, perl="C:\\Perl64\\bin\\perl.exe", skip=1, header= TRUE)
  coll <- colnames(data)
  for (i in 1:length(coll)){
    comparacion <- grepl("(X)", coll[i])
    if (comparacion==T){
      data[coll[i]] <- NULL
    }
  }
  data <- data[-c(1), ]
  for (ii in 2:nrow(data)){
    for (jj in 2:length(data)){
      producto <- data$Precio...Kg[ii]
      central <- colnames(data)
      zzzz<- data.frame(ll[j], central[jj], producto, data[ii,jj])
      names(zzzz)<-c("Fecha", "Central", "Producto", "Precio_kg")
      deef <- rbind(deef, zzzz)
    }
  }
}

deef$Fecha <- as.POSIXct(as.Date(deef$Fecha, origin="1970-01-01"))
deef$Central <- as.character(deef$Central)
deef$Producto <- as.character(deef$Producto)
deef$Precio_kg <- gsub(pattern = ",", replacement = "",x = deef$Precio_kg)
deef$Precio_kg <- as.numeric(deef$Precio_kg)

deef<- deef[-which(is.na(deef$Precio_kg)), ]
for (i in 1:length(fie)){
  if (file.exists(fie[i])) file.remove(fie[i])
}
for (i in 1:length(deef$Producto)){
  comparacion <- grepl(deef$Producto[i], unique(dataset$Producto))
  poss <- match(TRUE, comparacion)
  deef$Producto[deef$Producto == deef$Producto[i]] <- unique(dataset$Producto)[poss]
  comparacion <- grepl(deef$Central[i], unique(dataset$Central))
  poss <- match(TRUE, comparacion)
  deef$Central[deef$Central == deef$Central[i]] <- unique(dataset$Central)[poss]
}
dataset <- rbind(dataset, deef)