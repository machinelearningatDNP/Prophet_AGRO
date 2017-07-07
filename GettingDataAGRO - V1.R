#install.packages(c("rvest","readxl","gdata","WriteXLS","stringi"))
library(rvest)
library(readxl)
library(gdata)
library(WriteXLS)
library(stringi)
mainDir <- "C:/Users/darojas/Documents/AGRO"
setwd(mainDir)
url <- "http://www.dane.gov.co/index.php/estadisticas-por-tema/agropecuario/sistema-de-informacion-de-precios-sipsa/componente-precios-mayoristas"
page <- read_html(url)
a <- html_nodes(page, 'div .t3-content a')
aa <-html_attr(a, "href")
l <-c()
ll <- c()
fie <- c()
dis <- c()
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
  dis[i] <- paste(comparacion, ".xls", sep="")
  ll[i] <- fd 
  for (j in 1:length(l)){
    re=l[j]
    con<- grepl(comparacion, re)
    if (con==TRUE){
      fecha <- paste(comparacion,".xls",sep="")
      download.file(re, destfile=fecha, mode="wb")
      fie[i] <- fecha
      
    }else{
      next
    }  
  }
}
ll <- as.Date(ll, origin="1970-01-01")
deef <- data.frame(Fecha=as.Date(character()), 
                   Central=character(), 
                   Producto=character(), 
                   Precio_kg=as.numeric(character())
)
fie <- fie[!is.na(fie)]
dis <- dis[dis != "julio_3_2017.xls"]
dis <- dis[dis != "junio_26_2017.xls"]
dis <- dis[dis != "junio_19_2017.xls"]
fie <- dis
for (j in 1:length(fie)){
  data <- read.xls(fie[j], verbose=FALSE, perl="C:\\Perl64\\bin\\perl.exe", skip=1, header= TRUE)
  coll <- colnames(data)
  comparacion <- grepl("(X)", coll)
  for (i in 1:length(comparacion)){
    if (comparacion[i]==T){
      data[coll[i]] <- NULL
    }
  }
  data <- data[-c(1), ]
  for (ii in 2:nrow(data)){
    for (jj in 2:length(data)){
      producto <- data$Precio...Kg[ii]
      central <- colnames(data)
      prueba <- fie[j]
      prueba <- unlist(strsplit(prueba, "[_]"))
      mes <- match(prueba[1],meses)
      dia <- prueba[2]
      year <- substr(prueba[3],1,4)
      ffinal <- paste(year,mes,dia,sep="-")
      z<- data.frame(ffinal, central[jj], producto, data[ii,jj])
      names(z)<-c("Fecha", "Central", "Producto", "Precio_kg")
      deef <- rbind(deef, z)
    }
  }
}

deef$Fecha <- as.POSIXct(as.character(deef$Fecha), format= "%Y-%m-%d")
deef$Central <- as.character(deef$Central)
deef$Producto <- as.character(deef$Producto)
deef$Precio_kg <- gsub(pattern = ",", replacement = "",x = deef$Precio_kg)
deef$Precio_kg <- as.numeric(deef$Precio_kg)

deef<- deef[-which(is.na(deef$Precio_kg)), ]

deef$Central <- stri_trans_general(deef$Central,"Latin-ASCII")
deef$Producto <- stri_trans_general(deef$Producto,"Latin-ASCII")
dataset$Central <- stri_trans_general(dataset$Central,"Latin-ASCII")
dataset$Producto <- stri_trans_general(dataset$Producto,"Latin-ASCII")
for (i in 1:length(deef$Producto)){
  poss <- match(TRUE, grepl(substr(deef$Producto[i],1,4),substr(unique(dataset$Producto),1,4)))
  deef$Producto[deef$Producto == deef$Producto[i]] <- unique(dataset$Producto)[poss]
  poss2 <- match(TRUE, grepl(substr(deef$Central[i],1,4),substr(unique(dataset$Central),1,4)))
  deef$Central[deef$Central == deef$Central[i]] <- unique(dataset$Central)[poss2]
}

dataset <- rbind(dataset, deef)
save(dataset,file="dataset.Rda")
#for (i in 1:length(fie)){
#  if (file.exists(fie[i])) file.remove(fie[i])
#}