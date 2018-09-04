### xNriyB3ufAxdRmZgpq_p  ##
#ab1

#limpiar el enviroment
rm(list = ls())

# los 0s aceptados antes de expresas una cifra en notaci?n cient?fica
options("scipen"=100, "digits"=4)

###librerias
suppressMessages(library(plotly)) # 
suppressMessages(library(Quandl)) # Descarga de Precios
suppressMessages(library(PortfolioAnalytics)) ##Teoria modenar de portafolio
suppressMessages(library(ROI)) # optimización para ele portafolio 

suppressMessages(library(knitr))  
suppressMessages(library(kableExtra)) # Tablas en HTML
options(knitr.table.format = "html") 

# Cargar el token de QUANDL
Quandl.api_key("xNriyB3ufAxdRmZgpq_p")
df
# Funcion para descagar precios
Bajar_Precios <- function(Columns, Tickers, Fecha_In, Fecha_Fn) {
  
  # Peticion para descargar precios
  Datos <- Quandl.datatable("WIKI/PRICES", qopts.columns=Columns, ticker=Tickers,
                            date.gte=Fecha_In, date.lte=Fecha_Fn)
  return(Datos)
}

# Tickers de accciones y datos a solicitar a QUANDL
tk <- c("TSLA", "BBY", "HD")
cs <- c("date", "adj_close")
Capital_Inicial <- 10000
fs = c("2015-08-01","2016-08-01") # por la c es vector de caracteres 

# Descargar Precios y Calcular rendimientos
Datos <- list()

for(i in 1:length(tk))
  Datos[[i]] <- Bajar_Precios(Columns=cs, Ticker=tk[i], Fecha_In=fs[1], Fecha_Fn=fs[2])

names(Datos) <- tk

for(i in 1:length(tk))
  Datos[[i]]$adj_close_r <- c(0, diff(log(Datos[[i]]$adj_close)))

Rends <- xts(x = cbind(Datos[[1]]$adj_close_r, Datos[[2]]$adj_close_r, Datos[[3]]$adj_close_r),
             order.by = Datos[[1]]$date)[-1]
names(Rends) <- tk
Port1 <- portfolio.spec(assets=tk)
# Agregando restricciones

Port1 <- add.constraint(portfolio=Port1, type="full_investment")
Port1 <- add.constraint(portfolio=Port1, type="box", min=c(0.01, 0.01, 0.01), max=c(0.7, 0.7, 0.7))
Port1 <- add.objective(portfolio = Port1, type = "return", name = "mean")
Port1 <- optimize.portfolio(R=Rends, portfolio = Port1, optimize_method = "random", trace = TRUE, search_size =5000)
Portafolios <- vector("list", length = length(Port1$random_portfolio_objective_results))

for (i in 1:length(Port1$random_portfolio_objective_results)){
  Portafolios[[i]]$Pesos <- Port1$random_portfolio_objective_results[[i]]$weights #para indexar listas se utliza [[]], ese elemento del portafolio se va creando desde el for
  Portafolios[[i]]$Medias <- Port1$random_portfolio_objective_results[[i]]$objective_measures$mean
  Portafolios[[i]]$Vars <- var.portfolio(R = Port1$R, weights = Portafolios[[i]]$Pesos)
  names(Portafolios[[i]]$Medias) <- NULL
}

# creamos un dataframe para poder almacenar todo tipo de valores 
  df_Portafolios <- data.frame(matrix(nrow=length(Port1$random_portfolio_objective_results),ncol=3, data=0))
  colnames(df_Portafolios) <- c("Rend", "Var", "Clase")
  for (i in 1:length(Port1$random_portfolio_objective_results)){
    df_Portafolios$Rend[i] <- round(Portafolios[[i]]$Medias*252,4)
    df_Portafolios$Var[i] <- round(sqrt(Portafolios[[i]]$Vars)*sqrt(252),4)
    df_Portafolios$Clase[i] <- "No Frontera"
    for(k in 1:length(tk)){
      df_Portafolios[i,paste("Peso_",tk[k],sep="")] <- Portafolios[[i]]$Pesos[k]
      df_Portafolios[i,paste("Titulos_ini_", tk[k], sep="")] <-
      (Capital_Inicial*Portafolios[[i]]$Pesos[k])%/%Datos[[k]]$adj_close[1]
  }
}