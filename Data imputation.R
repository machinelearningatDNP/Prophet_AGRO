load("dataset.Rda")
df <- dataset
df$Precio_kg <- as.numeric(df$Precio_kg)
df$Precio_kg <- round(df$Precio_kg, 0)
df_p <- dcast(df, formula = Central + Fecha ~ Producto, mean, value.var = "Precio_kg", na.rm = TRUE)
#df_p$`Limon comun` <- NULL
#df_p$`Limon Comun` <- NULL
productos <- colnames(df_p)
productos <- productos[-c(1:2)]
centrales <- unique(df_p$Central)

for (product in productos){
  dff <- df_p[,c("Central", "Fecha", product)]
  dff <- dcast(dff, formula = Fecha ~ Central, mean, value.var = product)
  for (central in centrales){
    mv <- (sum(is.na(dff[central]))/nrow(dff[central]))*100
    if (mv>=40){
      dff[central] <- NULL
    }else{
      dff[central] <- try(as.numeric(round(na.interpolation(ts(dff[central], frequency = 1)),0)))
    }
  }
  assign(product, dff)
  centr_df <- colnames(dff)
  centr_df <- centr_df[-1]
  len = length(centr_df)
  disctances = matrix(nrow = len, ncol = len, dimnames = list(centr_df))
  colnames(disctances) = centr_df
  for (p1 in centr_df){
    for (p2 in centr_df){
      jdf <- dff[,c(p1, p2)]
      jdf <- jdf[complete.cases(jdf),]
      calculation <- EuclideanDistance(jdf[,1], jdf[,2])
      disctances[p1, p2] = calculation
    }
  }
  m <- disctances
  new.palette=colorRampPalette(c("black","red","yellow","white"),space="rgb") 
  png(file=paste(product,".png",sep=""))
  plot <- levelplot(m[1:ncol(m),ncol(m):1],col.regions=new.palette(20), scales=list(x=list(rot=90, cex=1),y=list(cex=1)), xlab=list(cex=0), ylab=list(cex=0),  main = product)
  print(plot)
  dev.off()
  
}






