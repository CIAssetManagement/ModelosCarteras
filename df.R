#Librerias
library(shiny)
library(rhandsontable)
library(DT)
library(plotly)
library(dplyr)
library(tidyr)
library(reshape2)
options(scipen=999)

#Datos
precios <- read.csv("precios.csv",stringsAsFactors = FALSE)
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
    series <- merge(serie1,serie2,by.x = 1,by.y = 1,all.x = TRUE, all.y = TRUE)
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

rendimiento_dolareseuros <- function(tipo,monto,portafolio,pesos,fecha_inicio,fecha_fin){
  fondos <- c(portafolio$Fondos,ifelse(tipo == "dolares","*CSP-MXPUSDS-V48","*CSP-MXPEUR-V48"))
  prices <- precios[precios$id %in% fondos,] %>%
    mutate(fecha = as.Date(fecha)) %>%
    spread(id, Precio_sucio) %>%
    filter(fecha %in% seq(fecha_inicio-5,fecha_fin,by = "1 day")) %>%
    data.frame(check.names = FALSE)
  
  moneda_extranjera <- prices[-length(prices$fecha),sort(fondos)[1]]
  fecha <- prices$fecha[-1]
  if(length(colnames(prices)) > 3){
    prices <- sweep(prices[-1,-c(1,which(colnames(prices) == sort(fondos)[1]))],1,moneda_extranjera,`/`)
  } else {
    prices <- data.frame(Serie =prices[-1,-c(1,which(colnames(prices) == sort(fondos)[1]))]/moneda_extranjera)
  }
  prices <- cbind(fecha,prices)
  prices <- prices %>% filter(fecha %in% seq(fecha_inicio,fecha_fin,by = "1 day"))
  
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

estadisticas_portafolios <- function(monto,portafolio,fecha_inicio,fecha_fin){
  prices <- precios[precios$id %in% portafolio$Fondos,] %>%
    mutate(fecha = as.Date(fecha)) %>%
    spread(id, Precio_sucio) %>%
    filter(fecha %in% seq(fecha_inicio,fecha_fin,by = "1 month")) %>%
    data.frame(check.names = FALSE)
  
  pesos <- portafolio$Porcentaje
  returns <- 100*(prices[-1,-1]/prices[-length(prices$fecha),-1]-1)
  
  if(length(colnames(prices)) > 2){
    returns <- sweep(returns,2,pesos,`*`)
    retornos <- rowSums(returns)
  } else {
    retornos <- returns
  }
  
  datos1 <- c(min(retornos), max(retornos), mean(retornos)) %>%
    round(digits = 2)%>%
    paste0("%")
  
  saldofinal <- rendimiento_instrumentos(monto,portafolio,pesos,fecha_inicio,fecha_fin)
  saldofinal <- saldofinal[length(saldofinal[,2]),2]
  datos2 <- c(monto, saldofinal) %>%
    round(digits = 2) %>%
    format(big.mark   = "," ,big.interval = 3L) %>%
    paste0("$",.)
  
  datos <- data.frame(c(datos1,datos2))
  return(datos)
}

rend_divisas <- function(portafolio,fecha_inicio,fecha_fin){
  monto <- 100000
  
  #Rendimiento en pesos
  indices_mex <- as.character(sapply(portafolio$Fondos,function(x) strsplit(x,"-")[[1]][2])) %in% c("+CIGUB","+CIGUMP","+CIGULP","+CIPLUS","+CIBOLS","CRECE+")
  portafolio_mex <- portafolio[indices_mex,]
  pesos <- portafolio_mex$Porcentaje/sum(portafolio_mex$Porcentaje)
  rend_mex <- rendimiento_instrumentos(monto,portafolio_mex,pesos,fecha_inicio,fecha_fin)
  rend_mex <- 100*(rend_mex[length(rend_mex[,2]),2]/monto-1)
  rend_mex <- c(rend_mex,360*rend_mex/as.numeric(fecha_fin-fecha_inicio))
  
  #Rendimiento en dólares
  indices_us <- as.character(sapply(portafolio$Fondos,function(x) strsplit(x,"-")[[1]][2])) %in% c("+CIEQUS","+CIUSD","NAVIGTR")
  portafolio_us <- portafolio[indices_us,]
  pesos <- portafolio_us$Porcentaje/sum(portafolio_us$Porcentaje)
  rend_us <- rendimiento_dolareseuros("dolares",monto,portafolio_us,pesos,fecha_inicio,fecha_fin)
  rend_us <- 100*(rend_us[length(rend_us[,2]),2]/monto-1)
  rend_us <- c(rend_us,360*rend_us/as.numeric(fecha_fin-fecha_inicio))
  
  #Rendimiento en euros
  indices_eur <- as.character(sapply(portafolio$Fondos,function(x) strsplit(x,"-")[[1]][2])) %in% c("AXESEDM")
  portafolio_eur <- portafolio[indices_eur,]
  pesos <- portafolio_eur$Porcentaje/sum(portafolio_eur$Porcentaje)
  rend_eur <- rendimiento_dolareseuros("euros",monto,portafolio_eur,pesos,fecha_inicio,fecha_fin)
  rend_eur <- 100*(rend_eur[length(rend_eur[,2]),2]/monto-1)
  rend_eur <- c(rend_eur,360*rend_eur/as.numeric(fecha_fin-fecha_inicio))
  
  rend_tot <- rendimiento_instrumentos(monto,portafolio,portafolio$Porcentaje,fecha_inicio,fecha_fin)
  rend_tot <- 100*(rend_tot[length(rend_tot[,2]),2]/monto-1)
  rend_tot <- c(rend_tot,360*rend_tot/as.numeric(fecha_fin-fecha_inicio))
  
  datos <- c(rend_mex,rend_us,rend_eur,rend_tot) %>%
    round(digits = 2) %>%
    paste0("%") %>% 
    ifelse(.=="NaN%","-",.)
  
  return(datos)
}