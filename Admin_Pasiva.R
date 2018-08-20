### xNriyB3ufAxdRmZgpq_p  ##
#ab1

#limpiar el enviroment
rm(list = lst())

# Los  0s aceptados anetes de expresar una cifra en notacion cient�fica
options("scipen" = 1000,"digits" = 4)

# Librerias a utilizar

suppressMessages(library(plotly))
suppressMessages(library(Quandl)) #descarga de precios

suppressMessages(library(PortfolioAnalytics)) #Teoria modenar de portafolio 
suppressMessages(library(ROI)) #optimizaci�n para ele portafolio 
suppressMessages(library(knitr))
suppressMessages(library(kableExtra)) #tablas en html 
options(knitr.table.fromat ="html")

Quandl.api_key("xNriyB3ufAxdRmZgpq_p") # cargamos el appi key

Bajarprecios  <- function(Columns, Tickers,Fecha_In,Fecha_Fin) {
  
  # peticion de descarga de datos
  Datos<- Quandl.datatable(code="WIKI/PRICES",qopts.columns = Columns, ticker = Tickers,date.gte = Fecha_In,date.lte  = Fecha_Fin)
}

return(Datos) 

tk <-c("TSLA","BBY","HD")
cs<- c("date", "adj_close")
