### xNriyB3ufAxdRmZgpq_p  ##
#ab1

#limpiar el enviroment
rm(list = ls())

# Los  0s aceptados anetes de expresar una cifra en notacion científica
options("scipen" = 1000,"digits" = 4)

# Librerias a utilizar

suppressMessages(library(plotly))
suppressMessages(library(Quandl)) #descarga de precios

suppressMessages(library(PortfolioAnalytics)) #Teoria modenar de portafolio 
suppressMessages(library(ROI)) #optimización para ele portafolio 
suppressMessages(library(knitr))
suppressMessages(library(kableExtra)) #tablas en html 
options(knitr.table.fromat ="html")

Quandl.api_key("xNriyB3ufAxdRmZgpq_p") # cargamos el appi key

Bajar_precios  <- function(Columns, Tickers,Fecha_In,Fecha_Fin) {
  Columns<-cs 
  Tickers<-tk[1]
  Fecha_In <- fs[1]
  Fecha_Fn<-fs[2]
  
  # peticion de descarga de datos
  
  Datos <- Quandl.datatable("WIKI/PRICES",qopts.columns = Columns, Ticker = Tickers,date.gte = Fecha_In, date.lte  = Fecha_Fin)


return(Datos) 
}

tk <-c("TSLA","BBY","HD")
cs<- c("date", "adj_close")
 #fechas
 fs = c("2015-08-01","2016-08-01") # por la c es vector de caracteres 

 
 #Capital inicial 
 
 Cap_inicial <- 100000
 Comision <- .005
 
 Datos <- list()
 
 for (i in 1:length(tk)){
   Datos[[i]] <- Bajar_precios(Columns = cs, Ticker = tk[i], Fecha_In = fs[1],Fecha_Fin = fs[2]) 
   
  
 }
 
 names(Datos)<-tk
 
 for(i in 1:length(tk))
   Datos[[i]]$adj_close_r <- c(0, diff(log(Datos[[i]]$adj_close)))
 
 Rends <- xts(x = cbind(Datos[[1]]$adj_close_r, Datos[[2]]$adj_close_r, Datos[[3]]$adj_close_r),
              order.by = Datos[[1]]$date)[-1]
 names(Rends) <- tk
 