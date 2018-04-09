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
  
  observeEvent(input$monto_inversion, {

    monto <- input$monto_inversion
    portafolio1 <- values[["df"]]
    portafolio1$Fondos <- seriesfondos(monto,portafolio1)
    values[["df"]] <- portafolio1

  })
  
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
      values[["ci_pie"]] <- plot_ly(portafolio1, labels = ~Fondos, values = ~Porcentaje, type = 'pie', textinfo = 'label+percent',
                                    textposition = "outside", marker = list(line = list(color = '#FFFFFF', width = 1))) %>%
        layout(hovermode = FALSE, title = '',xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    })
    
    output$segundopie <- renderPlotly({
      portafolio2 <- values[["df2"]]
      portafolio2 <- portafolio2 %>% filter(Fondos %in% fondos_industria & Porcentaje > 0)
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
      estadisticas1 <- estadisticas_portafolios(1,input$monto_inversion,portafolio1,input$rangofechas[1],input$rangofechas[2])
      estadisticas1 <- data.frame(estadisticas1,row.names = c("Mes Mínimo","Mes Máximo","Mes Promedio","Saldo Inicial","Saldo Final"))
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
      rendimiento1 <- data.frame(Rendimientos = rendimiento1,row.names = c("Efectivo pesos", "Anualizado pesos","Efectivo dólares", "Anualizado dólares",
                                                                                       "Efectivo euros", "Anualizado euros", "Efectivo portafolio", "Anualizado portafolio"))
      values[["rendimiento1"]] <- rendimiento1
      datatable(rendimiento1,options = list(dom = 't', pageLength = 100))
    })
    
  })
  
  output$portafolios1 <- downloadHandler(
    
    filename = function() {"propuesta.html"},
    
    content = function(file) {
      tempReport2 <- file.path(tempdir(), "un_portafolio.Rmd")
      file.copy("un_portafolio.Rmd", tempReport2, overwrite = TRUE)
      params2 <- list(inicio = input$rangofechas[1],
                      fin = input$rangofechas[2],
                      promotor = as.character(input$promotor),
                      nombre = as.character(input$nombre),
                      perfil = as.character(input$perfil_inversion),
                      mxpusdeur = values$rendimiento1,
                      ci_portafolio = values$df,
                      grafica_rendimiento = values$grafica_rendimiento,
                      ci_summary = values$estadisticas1,
                      ci_pie = values$ci_pie)
      rmarkdown::render(tempReport2, output_file = file,params = params2, envir = new.env(parent = globalenv()))
  })
  
  output$portafolios2 <- downloadHandler(
      
    filename = function() {"comparativo.html"},
    
    content = function(file) {
      
      tempReport1 <- file.path(tempdir(), "dos_portafolios.Rmd")
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
