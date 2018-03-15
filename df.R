#Librerias
library(shiny)
library(rhandsontable)
library(DT)
library(plotly)
library(dplyr)
library(tidyr)
library(reshape2)

#Datos
precios <- read.csv("precios.csv",stringsAsFactors = FALSE)
tasa_libre <- read.csv("tasa.csv",stringsAsFactors = FALSE)
precios$X <-  NULL
fondos_industria <- unique(precios$id)
fondos_propios <- c("51-+CIGUB-A","51-+CIGUMP-A","51-+CIGULP-A","51-+CIPLUS-A","52-+CIBOLS-A",
            "52-+CIEQUS-A","51-+CIUSD-A","52-NAVIGTR-A","52-AXESEDM-A","52-CRECE+-A")
porcentaje <- runif(length(fondos_propios), min = 0, max = 1)
porcentaje <- round(porcentaje/sum(porcentaje),digits = 3)
df <- data.frame(Fondos=fondos_propios,Porcentaje=porcentaje,stringsAsFactors = FALSE)
df2 <- data.frame(Fondos=c("51-+CIGUB-A",rep("",length(fondos_propios)-1)),
                  Porcentaje=c(1,rep(0,length(fondos_propios)-1)),stringsAsFactors = FALSE)

#Funciones
rendimiento_portafolios <- function(monto,p1,p2,fecha_inicio,fecha_fin){
  #¿Hay algo en el portafolio 2?
  indices_2 <- unique(p2$Fondos %in% fondos_industria)
  if(TRUE %in% indices_2){
    serie2 <- data.frame(rendimiento_instrumentos(monto,p2,p2$Porcentaje[which(p2$Fondos %in% fondos_industria)],
                                                  fecha_inicio,fecha_fin))
    serie1 <- data.frame(rendimiento_instrumentos(monto,p1,p1$Porcentaje,fecha_inicio,fecha_fin))
    series <- cbind(serie1,serie2[-1])
    colnames(series) <- c("Fecha","Portafolio_CI_Estrategias","Portafolio_Comparativo")
  } else {
    series <- data.frame(rendimiento_instrumentos(monto,p1,p1$Porcentaje,fecha_inicio,fecha_fin))
    colnames(series) <- c("Fecha","Portafolio_CI_Estrategias")
  }
  df <- melt(series ,  id.vars = 'Fecha', variable.name = 'series')
  return(df)
}

rendimiento_instrumentos <- function(monto,portafolio,pesos,fecha_inicio,fecha_fin){
  prices <- precios[precios$id %in% portafolio$Fondos,] %>%
    mutate(fecha = as.Date(fecha)) %>%
    spread(id, Precio_sucio) %>%
    filter(fecha %in% seq(fecha_inicio,fecha_fin,by = "1 day")) %>%
    data.frame(check.names = FALSE)
  if(length(colnames(prices)) > 2){
    returns <- data.frame(matrix(unlist(apply(prices[,-1], 1, function(x) x/prices[1,-1] )),
                                 ncol = length(colnames(prices))-1,byrow = TRUE))
    returns <- sweep(returns,2,pesos,`*`)
    retornos <- rowSums(returns)
  } else {
    retornos <- prices[,-1]/prices[1,-1]
  }
  rend_portfolio <- data.frame(prices$fecha,monto*retornos)
  return(rend_portfolio)
}

max_min_rendimiento <- function(portafolio,pesos,fecha_inicio,fecha_fin){
  prices <- precios[precios$id %in% portafolio$Fondos,] %>%
    mutate(fecha = as.Date(fecha)) %>%
    spread(id, Precio_sucio) %>%
    filter(fecha %in% seq(fecha_inicio,fecha_fin,by = "1 month")) %>%
    data.frame(check.names = FALSE)
  
  returns <- 100*(prices[-1,-1]/prices[-length(prices$fecha),-1]-1)
  if(length(colnames(prices)) > 2){
    returns <- sweep(returns,2,pesos,`*`)
    retornos <- rowSums(returns)
  } else {
    retornos <- returns
  }
  
  datos <- paste0(round(c(min(retornos), max(retornos), mean(retornos)),digits = 2),"%")
  datos <- data.frame(Datos=datos)
  return(datos)
}

riesgo_rendimiento <- function(portafolio,pesos,fecha_inicio,fecha_fin){
  prices <- precios[precios$id %in% portafolio$Fondos,] %>%
    mutate(fecha = as.Date(fecha)) %>%
    spread(id, Precio_sucio) %>%
    filter(fecha %in% seq(fecha_inicio,fecha_fin,by = "1 day")) %>%
    data.frame(check.names = FALSE)
  
  tasa_libre_riesgo <- tasa_libre %>% filter(as.Date(fecha) %in% seq(fecha_inicio,fecha_fin,by = "1 day"))
  
  returns <- 100*(prices[-1,-1]/prices[-length(prices$fecha),-1]-1)
  numerador_sortino <- 100*(prices[length(prices$fecha),-1]/prices[1,-1]-1)
  numerador_sortino <- sum(numerador_sortino*pesos) - mean(as.numeric(tasa_libre_riesgo$nivel))
  if(length(colnames(prices)) > 2){
    returns <- sweep(returns,2,pesos,`*`)
    retornos <- rowSums(returns)
  } else {
    retornos <- returns
  }
  
  volatilidad <- sqrt(360)*sd(ifelse(retornos > 0,0,retornos),na.rm = TRUE)
  if(volatilidad < 1e-5){
    sortino <- "-"
  } else {
    sortino <- numerador_sortino / volatilidad
    sortino <- round(sortino,digits = 2)
  }
  datos <- c(paste0(round(volatilidad,digits = 2),"%"), sortino)
  datos <- data.frame(Datos=datos)
  return(datos)
}

estadisticas_portafolios <- function(portafolio,fecha_inicio,fecha_fin){
  estadisticos1 <- max_min_rendimiento(portafolio,portafolio$Porcentaje[which(portafolio$Fondos %in% fondos_industria)],
                                         fecha_inicio,fecha_fin)
  estadisticos2 <- riesgo_rendimiento(portafolio,portafolio$Porcentaje[which(portafolio$Fondos %in% fondos_industria)],
                                        fecha_inicio,fecha_fin)
  estadisticos <- rbind(estadisticos1,estadisticos2)
}