#Librerias
library(readxl)
library(shiny)
library(rhandsontable)
library(DT)
library(plotly)
library(dplyr)
library(tidyr)
library(reshape2)
library(lpSolveAPI)
options(scipen=999)

#Funciones
serie <- function(monto,tipo = 'C'){
  #CIGUB, CIGUMP, CIGULP, CIUSD, CIEQUS, CIBOLS
  if(tipo == 'C' | tipo == 'BE'){
    if(tipo == 'C'){
      series <- c("C-0","C-1","C-2","C-3","C-4")
    } else {
      series <- c("BE-0","BE-1","BE-2","BE-3","BE-4")
    }
    montos <- c(100000000,30000000,5000000,1000000,10000)
    
    if(monto >= montos[1]){return(series[1])}
    if(between(monto,montos[2],montos[1])){return(series[2])}
    if(between(monto,montos[3],montos[2])){return(series[3])}
    if(between(monto,montos[4],montos[3])){return(series[4])}
    if(between(monto,montos[5],montos[4])){return(series[5])}
    if(monto < montos[5]){return(series[5])}
  }
  #CIPLUS
  if(tipo == 'BF'){
    return("A")
  }
  if(tipo == 'F' | tipo == 'M'){
    if(tipo == 'F'){
      series <- c("F1","F3")
    } else {
      series <- c("M1","M3")
    }
    montos <- c(100000,9999999)
    
    if(monto <= montos[2]){return(series[1])}
    if(monto > montos[2]){return(series[2])}
  }
}

seriesfondos <- function(monto,portafolio){
  tv <- as.character(lapply(portafolio$Fondos,function(x) strsplit(x,"-")[[1]][1]))
  fondos <- as.character(lapply(portafolio$Fondos,function(x) strsplit(x,"-")[[1]][2]))
  tipos <- substr(fondos,2,2)
  portafolio1 <- portafolio[tipos %in% c("C","X"),]
  montos <- monto*portafolio1$Porcentaje
  tiposerie <- tipos[tipos %in% c("C","X")]
  series <- mapply(serie,montos,ifelse(tiposerie == "X","F",tiposerie))
  fondosportafolio <- paste0(tv[tipos %in% c("C","X")],"-",fondos[tipos %in% c("C","X")],"-",series)
  fondosportafolio <- c(fondosportafolio,portafolio$Fondos[!(tipos %in% c("C","X"))])
  return(fondosportafolio)
}

porcentajes_optimos <- function(perfil,portafolio, fecha_inicio, fecha_fin){
  if(length(colnames(portafolio)) > 1){
    portafolio <- portafolio[order(portafolio$Fondos),]
  } else {
    portafolio$Fondos <- portafolio[order(portafolio$Fondos),]
  }
  prices <- precios[precios$id %in% portafolio$Fondos,] %>%
    mutate(fecha = as.Date(fecha)) %>%
    spread(id, Precio_sucio) %>%
    filter(fecha %in% c(fecha_inicio,fecha_fin)) %>%
    data.frame(check.names = FALSE)
  
  returns <- data.frame(Fondos = t(100*(prices[2,-1]/prices[1,-1]-1)),
                        Limites=limites_lp[,which(colnames(limites_lp) == perfil)])
  
  #Modelo lineal
  modelo_pl <- make.lp(0,length(returns$Limites))
  lp.control(modelo_pl,sense='max')
  dummy <- length(returns$Limites)
  for(i in seq(1,length(returns$Limites),1)){
    add.constraint(modelo_pl, xt = 1, type = 1, indices = i, rhs = returns$Limites[i])
    dummy <- dummy + length(returns$Limites)
  }
  add.constraint(modelo_pl, xt = rep(1,length(returns$Limites)), type = 3, indices = seq(1,length(returns$Limites)), rhs = 1)
  set.objfn(modelo_pl, returns$X2)
  solve(modelo_pl)
  porcentajes <- get.variables(modelo_pl)
  
  return(porcentajes)
}

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
    filter(fecha %in% sapply(seq(fecha_inicio,fecha_fin,by = "1 day"),diah)) %>%
    data.frame(check.names = FALSE)
  
  pesos <- pesos[order(portafolio$Fondos)]
  
  if(length(colnames(prices)) > 2){
    returns <- data.frame(matrix(unlist(apply(prices[,-1], 1, function(x) x/prices[1,-1] )),
                                 ncol = length(colnames(prices))-1,byrow = TRUE))
    returns <- sweep(returns,2,pesos,`*`)
    indices <- colSums(returns,na.rm = TRUE) == 0
    if(TRUE %in% unique(is.na(returns[,indices]) == TRUE))
      returns[,indices] <- data.frame(matrix(0,nrow = length(returns[,1]),ncol = sum(indices == TRUE)))
      
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
    filter(fecha %in% sapply(seq(fecha_inicio-5,fecha_fin,by = "1 day"),diah)) %>%
    data.frame(check.names = FALSE)
  
  #En este orden o el deploy no funciona
  if(tipo == "dolares")
    pesos <- pesos[order(c("+CIUSD","+CIEQUS","NAVIGTR"))]
  
  moneda_extranjera <- prices[-length(prices$fecha),sort(fondos)[1]]
  fecha <- prices$fecha[-1]
  if(length(colnames(prices)) > 3){
    prices <- sweep(prices[-1,-c(1,which(colnames(prices) == sort(fondos)[1]))],1,moneda_extranjera,`/`)
  } else {
    prices <- data.frame(Serie=prices[-1,-c(1,which(colnames(prices) == sort(fondos)[1]))]/moneda_extranjera)
  }
  prices <- cbind(fecha,prices)
  prices <- prices %>% filter(fecha %in% sapply(seq(fecha_inicio,fecha_fin,by = "1 day"),diah))
  
  if(length(colnames(prices)) > 2){
    returns <- data.frame(matrix(unlist(apply(prices[,-1], 1, function(x) x/prices[1,-1] )),
                                 ncol = length(colnames(prices))-1,byrow = TRUE))
    returns <- sweep(returns,2,pesos,`*`)
    retornos <- rowSums(returns)
  } else {
    retornos <- pesos*prices[,-1]/prices[1,-1]
  }
  
  rend_portfolio <- data.frame(prices$fecha,monto*retornos)
  return(rend_portfolio)
}

estadisticas_portafolios <- function(estadistica,monto,portafolio,fecha_inicio,fecha_fin){
  
  if(estadistica == 2){
    portafolio <- portafolio[portafolio$Porcentaje > 0,]
    portafolio1 <- portafolio
  } else {
    portafolio1 <-  portafolio
  }
  
  prices <- precios[precios$id %in% portafolio$Fondos,] %>%
    mutate(fecha = as.Date(fecha)) %>%
    spread(id, Precio_sucio) %>%
    filter(fecha %in% sapply(seq(fecha_inicio,fecha_fin,by = "1 month"),diah)) %>%
    data.frame(check.names = FALSE)
  
  pesos <- portafolio$Porcentaje[order(portafolio$Fondos)]
  returns <- 100*(prices[-1,-1]/prices[-length(prices$fecha),-1]-1)
  
  if(length(colnames(prices)) > 2){
    returns <- sweep(returns,2,pesos,`*`)
    retornos <- rowSums(returns, na.rm = TRUE)
  } else {
    retornos <- returns
  }
  
  datos1 <- c(min(retornos), max(retornos), mean(retornos)) %>%
    round(digits = 2)%>%
    paste0("%")
  
  saldos <- data.frame(rendimiento_portafolios(monto,portafolio1,NULL,fecha_inicio,fecha_fin))
  saldofinal <- as.numeric(saldos[length(saldos[,1]),3])
  datos2 <- c(monto, saldofinal) %>%
    round(digits = 2) %>%
    format(big.mark   = "," ,big.interval = 3L) %>%
    paste0("$",.)
  
  datos <- c(datos1,datos2)
  return(datos)
}

rend_divisas <- function(monto, portafolio,fecha_inicio,fecha_fin){
  
  #Rendimiento en pesos
  indices_mex <- as.character(sapply(portafolio$Fondos,function(x) strsplit(x,"-")[[1]][2])) %in% c("+CIGUB","+CIGULP","+CIGUMP","+CIPLUS","+CIBOLS","CRECE+")
  portafolio_mex <- portafolio[indices_mex,]
  pesos <- portafolio_mex$Porcentaje/sum(portafolio_mex$Porcentaje)
  rend_mex <- rendimiento_instrumentos(monto,portafolio_mex,pesos,fecha_inicio,fecha_fin)
  rend_mex <- rend_mex[length(rend_mex[,2]),2]
  rend_mex <- ifelse(rend_mex == 0,NA,100*(rend_mex/monto-1))
  rend_mex <- c(rend_mex,360*rend_mex/as.numeric(fecha_fin-fecha_inicio))
  
  #Rendimiento en dólares
  indices_us <- as.character(sapply(portafolio$Fondos,function(x) strsplit(x,"-")[[1]][2])) %in% c("+CIUSD","+CIEQUS","NAVIGTR")
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
    ifelse(.=="NaN%","-",.) %>%
    ifelse(.=="NA%","-",.)
  
  return(datos)
}

diah <-  function(fecha){
  fechabase0 <- as.Date("2017-08-06")
  entero <- as.integer(fecha - fechabase0 )
  if(entero %% 7 == 6 | entero %% 7 == 0){
    return(diah(fecha-1))
  } else {
    if(as.character(fecha) %in% as.character(festivos$dias)){
      return(diah(fecha-1))
    } else {return(fecha)}
  }
}

#Datos
load("precios.rds")
precios$X <-  NULL
festivos <- read.csv("festivos.csv")
limites_lp <- read.csv("limites_lp.csv",stringsAsFactors = FALSE)
fondos_industria <- unique(precios$id)

#En este orden o el deploy no funciona
fondos_propios <- c("51-+CIGUB-C-4","51-+CIGULP-C-4","51-+CIGUMP-C-4","51-+CIPLUS-C-4","51-+CIUSD-C-4",
                    "52-+CIBOLS-C-4","52-AXESEDM-F3","52-+CIEQUS-C-4","52-CRECE+-B-5","52-NAVIGTR-BF3")
porcentaje <- porcentajes_optimos("Conservador",data.frame(Fondos = fondos_propios),diah(Sys.Date()-180),
                                  diah(Sys.Date()-1))
df <- data.frame(Fondos=fondos_propios,Porcentaje=porcentaje,stringsAsFactors = FALSE)
df2 <- data.frame(Fondos=c("51-+CIGUB-A",rep("",length(fondos_propios)-2)),
                  Porcentaje=c(1,rep(0,length(fondos_propios)-2)),stringsAsFactors = FALSE)
