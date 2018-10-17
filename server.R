shinyServer(function(input, output) {
  
  values <- reactiveValues()
  
  observe({
    #Primer portafolio
    if(!is.null(input$primerportafolio)){
      values[["previousprimer"]] <- isolate(values[["df"]])
      df <-  hot_to_r(input$primerportafolio)
    } else {
      if(is.null(values[["df"]]))
        df <- df
      else
        df <- values[["df"]]
    }
    values[["df"]] <- df
    
    #Segundo portafolio
    if(!is.null(input$segundoportafolio)){
      values[["previoussegundo"]] <- isolate(values[["df2"]])
      df2 <-  hot_to_r(input$segundoportafolio)
    } else {
      if(is.null(values[["df2"]]))
        df2 <- df2
      else
        df2 <- values[["df2"]]
    }
    values[["df2"]] <- df2  
  })
  
  observeEvent(input$perfil_inversion, {

    portafolio1 <- values[["df"]]
    portafolio1$Porcentaje <- porcentajes_optimos(input$perfil_inversion, portafolio1,
                                                  input$rangofechas[1], input$rangofechas[2])
    values[["df"]] <- portafolio1

  })
  
  # observeEvent(input$monto_inversion, {
  # 
  #   monto <- input$monto_inversion
  #   portafolio1 <- values[["df"]]
  #   portafolio1$Fondos <- seriesfondos(monto,portafolio1)
  #   values[["df"]] <- portafolio1
  # 
  # })
  
  observeEvent(input$calcular,{
    
    output$primerportafolio <- renderRHandsontable({
      df <- values[["df"]]
      rhandsontable(df, rowHeaders = NULL, stretchH = "all") %>%
        hot_col("Fondos", type = "dropdown", source = fondos_industria,strict = FALSE) %>%
        hot_col("Porcentaje", format = "0.0%")
    })
    
    output$segundoportafolio <- renderRHandsontable({
      df2 <- values[["df2"]]
      rhandsontable(df2, rowHeaders = NULL, stretchH = "all") %>%
        hot_col("Fondos", type = "dropdown", source = fondos_industria,strict = FALSE) %>%
        hot_col("Porcentaje", format = "0.0%")
    })
    
    output$primer100 <- reactive({
      portafolio1 <- values[["df"]]
      ifelse(as.character(sum(portafolio1$Porcentaje)) == "1","","El porcentaje no suma 100% ")
    })
    
    output$segundo100 <- reactive({
      portafolio2 <- values[["df2"]]
      ifelse(as.character(sum(portafolio2$Porcentaje)) == "1","","El porcentaje no suma 100%")
    })

    output$primerpie <- renderPlotly({
      portafolio1 <- values[["df"]]
      portafolio1 <- portafolio1 %>% filter(Fondos %in% fondos_industria & Porcentaje > 0)
      portafolio1$Fondos <- as.character(lapply(portafolio1$Fondos,function(x){strsplit(x,"-")[[1]][2]}))
      values[["ci_pie"]] <- plot_ly(portafolio1, labels = ~Fondos, values = ~Porcentaje, type = 'pie', textinfo = 'label+percent',
                                    textposition = "outside", marker = list(line = list(color = '#FFFFFF', width = 1))) %>%
        layout(hovermode = FALSE, title = '',xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    })
    
    output$segundopie <- renderPlotly({
      portafolio2 <- values[["df2"]]
      portafolio2 <- portafolio2 %>% filter(Fondos %in% fondos_industria & Porcentaje > 0)
      portafolio2$Fondos <- as.character(lapply(portafolio2$Fondos,function(x){strsplit(x,"-")[[1]][2]}))
      values[["otro_pie"]] <- plot_ly(portafolio2, labels = ~Fondos, values = ~Porcentaje, type = 'pie', textinfo = 'label+percent',
                                      textposition = "outside", marker = list(line = list(color = '#FFFFFF', width = 1))) %>%
        layout(hovermode = FALSE, title = '',xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    })
    
    output$grafica <- renderPlotly({
      portafolio1 <- values[["df"]]
      portafolio2 <- values[["df2"]]
      if(input$comparativo){
        rendimientos <- data.frame(rendimiento_portafolios(input$monto_inversion,portafolio1,portafolio2,input$rangofechas[1],input$rangofechas[2]))
        values[["rendimiento"]] <- rendimientos
      } else {
        rendimientos <- data.frame(rendimiento_portafolios(input$monto_inversion,portafolio1,NULL,input$rangofechas[1],input$rangofechas[2]))
        values[["rendimiento"]] <- rendimientos
      }
      values[["grafica_rendimiento"]] <- plot_ly(rendimientos, x = ~Fecha, y = ~round(value,digits = 2), color = ~series, type = 'scatter', mode = 'lines',
              linetype = ~series, connectgaps = TRUE) %>%
        layout(hovermode = 'compare', title = '',xaxis = list(title = ''),
               yaxis = list(title = '',tickformat = "$,3.2"))
      
    })
    
    output$primerestadistica <- renderDataTable({
      portafolio1 <- values[["df"]]
      
      estadisticas1 <- estadisticas_portafolios(1,input$monto_inversion,portafolio1,input$rangofechas[1],input$rangofechas[2]) %>%
        data.frame(row.names = c("Mes Mínimo","Mes Máximo","Mes Promedio","Saldo Inicial","Saldo Final"))
      colnames(estadisticas1) <- c("Estadísticas")
      values[["estadisticas1"]] <- estadisticas1
      
      datatable(estadisticas1,options = list(dom = 't', pageLength = 100))
    })
    
    output$segundaestadistica <- renderDataTable({
      portafolio2 <- values[["df2"]]
      estadisticas2 <- estadisticas_portafolios(2,input$monto_inversion,portafolio2,input$rangofechas[1],input$rangofechas[2])
      estadisticas2 <- data.frame(estadisticas2,row.names = c("Mes Mínimo","Mes Máximo","Mes Promedio","Saldo Inicial","Saldo Final"))
      colnames(estadisticas2) <- c("Estadísticas")
      values[["estadisticas2"]] <- estadisticas2
      datatable(estadisticas2,options = list(dom = 't', pageLength = 100))
    })
    
    output$primerrendimiento <- renderDataTable({
      portafolio1 <- values[["df"]]
      #Rendimientos en diferentes divisas
      rendimiento1 <- rend_divisas(input$monto_inversion,portafolio1,input$rangofechas[1],input$rangofechas[2])
      values[["rendimiento1"]] <- rendimiento1
      
      rendimiento2 <- porcentajes_portafolio(portafolio1)
      values[["proporcion"]] <- rendimiento2
      
      datatable(rendimiento1,options = list(dom = 't', pageLength = 100))
    })
    
    output$monto <- reactive({
      monto <- input$monto_inversion
      return(paste0("$",format(monto,big.mark = ",")))
    })
    
    observe({
      
      #Rendimiento de los fondos de renta fija pesos
      portafolio <- values[["df"]]
      nombres <-  c()
      if(portafolio[1,2] != 0 || portafolio[3,2] != 0 || portafolio[2,2] != 0 || portafolio[4,2] != 0){
        rendimientos <- c()
        if(portafolio[1,2] != 0){
          monto <- input$monto_inversion * portafolio[1,2]
          rendimientos <- data.frame(rendimiento_portafolios(monto,portafolio[1,],NULL,input$rangofechas[1],input$rangofechas[2]))
          rendimientos$series <- NULL
          colnames(rendimientos) <- c("Fecha","+CIGUB") 
          nombres <- c(nombres,"+CIGUB")
        }
        
        if(portafolio[3,2] != 0){
          monto <- input$monto_inversion * portafolio[3,2]
          rendimientos2 <- data.frame(rendimiento_portafolios(monto,portafolio[3,],NULL,input$rangofechas[1],input$rangofechas[2]))
          rendimientos2$series <- NULL
          colnames(rendimientos2) <- c("Fecha","+CIGUMP")
          if(length(rendimientos[,1]) > 0){
            rendimientos <- merge(rendimientos,rendimientos2,by = 1)
          } else {
            rendimientos <- rendimientos2
          }
          nombres <- c(nombres,"+CIGUMP")
        }
        
        if(portafolio[2,2] != 0){
          monto <- input$monto_inversion * portafolio[2,2]
          rendimientos3 <- data.frame(rendimiento_portafolios(monto,portafolio[2,],NULL,input$rangofechas[1],input$rangofechas[2]))
          rendimientos3$series <- NULL
          colnames(rendimientos3) <- c("Fecha","+CIGULP")
          if(length(rendimientos[,1]) > 0){
            rendimientos <- merge(rendimientos,rendimientos3,by = 1)
          } else {
            rendimientos <- rendimientos3
          }
          nombres <- c(nombres,"+CIGULP")
        }
        
        if(portafolio[4,2] != 0){
          monto <- input$monto_inversion * portafolio[4,2]
          rendimientos4 <- data.frame(rendimiento_portafolios(monto,portafolio[4,],NULL,input$rangofechas[1],input$rangofechas[2]))
          rendimientos4$series <- NULL
          colnames(rendimientos4) <- c("Fecha","+CIPLUS")
          if(length(rendimientos[,1]) > 0){
            rendimientos <- merge(rendimientos,rendimientos4,by = 1)
          } else {
            rendimientos <- rendimientos4
          }
          nombres <- c(nombres,"+CIPLUS")
        }
        
        rend_mon <- t(rendimientos[length(rendimientos$Fecha),-1])
        vector_rend <- t(100*(rendimientos[length(rendimientos$Fecha),-1]/ rendimientos[1,-1]-1))
        vector_rend <- data.frame(Rendimiento = vector_rend)
        vector_rend[] <- lapply(vector_rend, function(x){paste0(round(x,digits = 2),"%")})
        vector_rend <- cbind(vector_rend,RendimientoMonto = scales::dollar(round(rend_mon,0)))
        rownames(vector_rend) <- nombres
        colnames(vector_rend) <- c("Rendimiento","Monto")
        values[["rf_mex_rend"]] <- vector_rend
        
      } else {
        values[["rf_mex_rend"]] <- NULL
      }
      
      #Rendimiento de los fondos de renta fija dólares
      nombres <-  c()
      rnombres <- c()
      if(portafolio[5,2] != 0 || portafolio[10,2] != 0){
        rendimientos <- c()
        rrendimientos <- c()
        if(portafolio[5,2] != 0){
          monto <- input$monto_inversion * portafolio[5,2]
          monto_dolares <- monto / dolar
          rendimientos <- data.frame(rendimiento_dolareseuros("dolares",monto_dolares,portafolio[c(5,8,10),],c(1,0,0),diah(Sys.Date()-365),diah(Sys.Date()-1)))
          colnames(rendimientos) <- c("Fecha","+CIUSD") 
          nombres <- c(nombres,"*+CIUSD USD")
          
          rrendimientos <- data.frame(rendimiento_portafolios(monto,portafolio[5,],NULL,diah(Sys.Date()-365),diah(Sys.Date()-1)))
          rrendimientos$series <- NULL
          colnames(rrendimientos) <- c("Fecha","+CIUSD") 
          rnombres <- c(rnombres,"+CIUSD MXN")
        }
        
        if(portafolio[10,2] != 0){
          monto <- input$monto_inversion * portafolio[10,2]
          monto_dolares <- monto / dolar
          rendimientos3 <- data.frame(rendimiento_dolareseuros("dolares",monto_dolares,portafolio[c(5,8,10),],c(0,0,1),diah(Sys.Date()-365),diah(Sys.Date()-1)))
          rendimientos3$series <- NULL
          colnames(rendimientos3) <- c("Fecha","NAVIGTR")
          if(length(rendimientos) == 0){
            rendimientos <- rendimientos3
          } else {
            rendimientos <- merge(rendimientos,rendimientos3, by = 1)
          }
          nombres <- c(nombres,"*NAVIGTR USD")
          
          rrendimientos3 <- data.frame(rendimiento_portafolios(monto,portafolio[10,],NULL,diah(Sys.Date()-365),diah(Sys.Date()-1)))
          rrendimientos3$series <- NULL
          colnames(rrendimientos3) <- c("Fecha","NAVIGTR")
          if(length(rrendimientos) == 0){
            rrendimientos <- rrendimientos3
          } else {
            rrendimientos <- merge(rrendimientos,rrendimientos3, by = 1)
          }
          rnombres <- c(rnombres,"NAVIGTR MXN")
        }
        
        rend_mon_dls <- t(rendimientos[length(rendimientos$Fecha),-1])
        vector_rend <- t(100*(rendimientos[length(rendimientos$Fecha),-1]/ rendimientos[1,-1]-1))
        
        vector_rend <- data.frame(Rendimiento = vector_rend)
        vector_rend[] <- lapply(vector_rend, function(x){paste0(round(x,digits = 2),"%")})
        vector_rend <- cbind(vector_rend,RendimientoMonto = scales::dollar(round(rend_mon_dls,0)))
        colnames(vector_rend) <- c("Rendimiento","Monto")
        rownames(vector_rend) <- nombres
        
        rend_mon <- t(rrendimientos[length(rrendimientos$Fecha),-1])
        vector_rendd <- t(100*(rrendimientos[length(rrendimientos$Fecha),-1]/ rrendimientos[1,-1]-1))
        
        vector_rendd <- data.frame(Rendimiento = vector_rendd)
        vector_rendd[] <- lapply(vector_rendd, function(x){paste0(round(x,digits = 2),"%")})
        vector_rendd <- cbind(vector_rendd,RendimientoMonto = scales::dollar(round(rend_mon,0)))
        colnames(vector_rendd) <- c("Rendimiento","Monto")
        rownames(vector_rendd) <- rnombres
        
        espacio <- data.frame(Rendimiento = " ", Monto = " ",row.names = " ")
        vector_rend <- rbind(vector_rend,espacio,vector_rendd)
        
        values[["rf_usd_rend"]] <- vector_rend
        
        # num_graphs <- length(colnames(rendimientos)) - 1
        # rendimientos <- melt(rendimientos,  id.vars = 'Fecha', variable.name = 'series') %>%
        #   transform(id = as.integer(factor(series)))
        # values[["rf_usd_graph"]] <- plot_ly(rendimientos, x = ~Fecha, y = ~round(value,digits = 2), 
        #                                     color = ~series, colors = "Dark2", yaxis = ~paste0("y",id)) %>%
        #   add_lines() %>%
        #   subplot(nrows = num_graphs, shareX = TRUE,titleX = FALSE,titleY = FALSE) %>%
        #   layout(title = 'Gráfica en Dólares',xaxis = list(title = ''),yaxis = list(title = ''))
        
      } else {
        values[["rf_usd_rend"]] <- NULL
      }
      
      #Rendimiento de los fondos de renta variable pesos
      nombres <-  c()
      if(portafolio[6,2] != 0 || portafolio[9,2] != 0){
        rendimientos <- c()
        if(portafolio[6,2] != 0){
          monto <- input$monto_inversion * portafolio[6,2]
          rendimientos <- data.frame(rendimiento_portafolios(monto,portafolio[6,],NULL,diah(Sys.Date()-180),diah(Sys.Date()-1)))
          rendimientos$series <- NULL
          colnames(rendimientos) <- c("Fecha","+CIBOLS") 
          nombres <- c(nombres,"+CIBOLS")
        }
        
        if(portafolio[9,2] != 0){
          monto <- input$monto_inversion * portafolio[9,2]
          rendimientos3 <- data.frame(rendimiento_portafolios(monto,portafolio[9,],NULL,diah(Sys.Date()-180),diah(Sys.Date()-1)))
          rendimientos3$series <- NULL
          colnames(rendimientos3) <- c("Fecha","CRECE+")
          if(length(rendimientos) == 0){
            rendimientos <- rendimientos3
          } else {
            rendimientos <- merge(rendimientos,rendimientos3, by = 1)
          }
          nombres <- c(nombres,"CRECE+")
        }
        rend_mon <- t(rendimientos[length(rendimientos$Fecha),-1])
        vector_rend <- t(100*(rendimientos[length(rendimientos$Fecha),-1]/ rendimientos[1,-1]-1))
        vector_rend <- data.frame(Rendimiento = vector_rend)
        vector_rend[] <- lapply(vector_rend, function(x){paste0(round(x,digits = 2),"%")})
        vector_rend <- cbind(vector_rend,RendimientoMonto = scales::dollar(round(rend_mon,0)))
        rownames(vector_rend) <- nombres
        colnames(vector_rend) <- c("Rendimientos","Monto")
        values[["rv_mex_rend"]] <- vector_rend
        
      } else {
        values[["rv_mex_rend"]] <- NULL
      }
      
      #Rendimiento de los fondos de renta variable dólares
      nombres <-  c()
      rnombres <- c()
      if(portafolio[8,2] != 0){
        monto <- input$monto_inversion * portafolio[8,2]
        monto_dolares <-  monto / dolar
          rendimientos <- data.frame(rendimiento_dolareseuros("dolares",monto_dolares,portafolio[c(5,8,10),],c(0,1,0),input$rangofechas[1],input$rangofechas[2]))
          colnames(rendimientos) <- c("Fecha","+CIEQUS") 
          nombres <- c(nombres,"*+CIEQUS USD")
          
          rrendimientos <- data.frame(rendimiento_portafolios(monto,portafolio[8,],NULL,input$rangofechas[1],input$rangofechas[2]))
          rrendimientos$series <- NULL
          colnames(rrendimientos) <- c("Fecha","+CIEQUS") 
          rnombres <- c(rnombres,"+CIEQUS MXN")
          
          rend_mon_dls <- t(rendimientos[length(rendimientos$Fecha),-1])
          vector_rend <- t(100*(rendimientos[length(rendimientos$Fecha),-1]/ rendimientos[1,-1]-1))
          dias <- as.numeric(input$rangofechas[2] - input$rangofechas[1])
          vector_rend2 <- 360*vector_rend/dias
          
          vector_rend <- data.frame(Rendimiento = vector_rend)
          vector_rend[] <- lapply(vector_rend, function(x){paste0(round(x,digits = 2),"%")})
          vector_rend <- cbind(vector_rend,RendimientoMonto = scales::dollar(round(rend_mon_dls,0)))
          colnames(vector_rend) <- c("Rendimiento","Monto")
          rownames(vector_rend) <- nombres
          
          rend_mon <- t(rrendimientos[length(rrendimientos$Fecha),-1])
          vector_rendd <- t(100*(rrendimientos[length(rrendimientos$Fecha),-1]/ rrendimientos[1,-1]-1))
          
          vector_rendd <- data.frame(Rendimiento = vector_rendd)
          vector_rendd[] <- lapply(vector_rendd, function(x){paste0(round(x,digits = 2),"%")})
          vector_rendd <- cbind(vector_rendd,RendimientoMonto = scales::dollar(round(rend_mon,0)))
          colnames(vector_rendd) <- c("Rendimiento","Monto")
          rownames(vector_rendd) <- rnombres
          
          espacio <- data.frame(Rendimiento = " ", Monto = " ",row.names = " ")
          vector_rend <- rbind(vector_rend,espacio,vector_rendd)
          
          values[["rv_usd_rend"]] <- vector_rend
          
      } else {
        values[["rv_usd_rend"]] <- NULL
      }
      
      #Rendimiento de los fondos de renta variable euros
      nombres <-  c()
      rnombres <- c()
      if(portafolio[7,2] != 0){
        monto <- input$monto_inversion * portafolio[7,2]
        monto_euros <-  monto / euro
        rendimientos <- data.frame(rendimiento_dolareseuros("euros",monto_euros,portafolio[7,],1,input$rangofechas[1],input$rangofechas[2]))
        colnames(rendimientos) <- c("Fecha","AXESEDM") 
        nombres <- c(nombres,"**AXESEDM EUR")
        
        rrendimientos <- data.frame(rendimiento_portafolios(monto,portafolio[7,],NULL,input$rangofechas[1],input$rangofechas[2]))
        rrendimientos$series <- NULL
        colnames(rrendimientos) <- c("Fecha","AXESEDM") 
        rnombres <- c(rnombres,"AXESEDM MXN")
        
        rend_mon_eur <- t(rendimientos[length(rendimientos$Fecha),-1])
        vector_rend <- t(100*(rendimientos[length(rendimientos$Fecha),-1]/ rendimientos[1,-1]-1))
        
        vector_rend <- data.frame(Rendimiento = vector_rend)
        vector_rend[] <- lapply(vector_rend, function(x){paste0(round(x,digits = 2),"%")})
        vector_rend <- cbind(vector_rend,RendimientoMonto = scales::dollar(round(rend_mon_eur,2)))
        colnames(vector_rend) <- c("Rendimiento","Monto")
        rownames(vector_rend) <- nombres
        
        rend_mon <- t(rrendimientos[length(rrendimientos$Fecha),-1])
        vector_rendd <- t(100*(rrendimientos[length(rrendimientos$Fecha),-1]/ rrendimientos[1,-1]-1))
        
        vector_rendd <- data.frame(Rendimiento = vector_rendd)
        vector_rendd[] <- lapply(vector_rendd, function(x){paste0(round(x,digits = 2),"%")})
        vector_rendd <- cbind(vector_rendd,RendimientoMonto = scales::dollar(round(rend_mon,0)))
        colnames(vector_rendd) <- c("Rendimiento","Monto")
        rownames(vector_rendd) <- rnombres
        
        espacio <- data.frame(Rendimiento = " ", Monto = " ",row.names = " ")
        vector_rend <- rbind(vector_rend,espacio,vector_rendd)
        
        values[["rv_eur_rend"]] <- vector_rend
        
      } else {
        values[["rv_eur_rend"]] <- NULL
      }
    })
  })

  
  output$portafolios1 <- downloadHandler(
    
    filename = function() {"propuesta.html"},
    
    content = function(file) {
      tempReport2 <- file.path(tempdir(), "un_portafolio.Rmd")
      file.copy("estrategias.png",file.path(tempdir(), "estrategias.png"), overwrite = TRUE)
      file.copy("un_portafolio.Rmd", tempReport2, overwrite = TRUE)
      params2 <- list(inicio = input$rangofechas[1],
                      fin = input$rangofechas[2],
                      monto = input$monto_inversion,
                      promotor = as.character(input$promotor),
                      nombre = as.character(input$nombre),
                      perfil = as.character(input$perfil_inversion),
                      ci_portafolio = values$df,
                      ci_summary = values$rendimiento1,
                      ci_proporcion = values$proporcion,
                      ci_pie = values$ci_pie,
                      rf_r = values$rf_mex_rend,
                      rfu_r = values$rf_usd_rend,
                      rv_r = values$rv_mex_rend,
                      rvu_r = values$rv_usd_rend,
                      rve_r = values$rv_eur_rend)
      rmarkdown::render(tempReport2, output_file = file,params = params2, envir = new.env(parent = globalenv()))
  })
  
  output$portafolios2 <- downloadHandler(
      
    filename = function() {"comparativo.html"},
    
    content = function(file) {
      
      tempReport1 <- file.path(tempdir(), "dos_portafolios.Rmd")
      file.copy("estrategias.png",file.path(tempdir(), "estrategias.png"), overwrite = TRUE)
      file.copy("dos_portafolios.Rmd", tempReport1, overwrite = TRUE)
      params1 <- list(inicio = input$rangofechas[1],
                     fin = input$rangofechas[2],
                     promotor = as.character(input$promotor),
                     nombre = as.character(input$nombre),
                     perfil = as.character(input$perfil_inversion),
                     ci_portafolio = values$df,
                     otro_portafolio = values$df2,
                     grafica_rendimiento = values$grafica_rendimiento,
                     ci_summary = values$estadisticas1,
                     otro_summary = values$estadisticas2,
                     ci_pie = values$ci_pie,
                     otro_pie = values$otro_pie)
      rmarkdown::render(tempReport1, output_file = file,params = params1, envir = new.env(parent = globalenv()))
  })

})
