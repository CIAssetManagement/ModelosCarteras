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
  
  observe({
    
    values[["fecha"]] <- input$rangofechas[2]
    portafolio1 <- values[["df"]]
    portafolio2 <- values[["df2"]]
    if(!is.null(portafolio1)){
      
      estadisticas1 <- estadisticas_portafolios(portafolio1,input$rangofechas[1],input$rangofechas[2])
      estadisticas1 <- data.frame(estadisticas1, row.names = c("Mínimo","Máximo","Promedio","Volatilidad","Sortino"))
      colnames(estadisticas1) <- c("Estadísticas Mensuales")
      values[["estadisticas1"]] <- estadisticas1
      
      if(!is.null(portafolio2) & input$comparativo == TRUE){
        
        rendimientos <- rendimiento_portafolios(input$monto_inversion,portafolio1,portafolio2,input$rangofechas[1],input$rangofechas[2])
        values[["rendimiento"]] <- data.frame(rendimientos)
        
        estadisticas2 <- estadisticas_portafolios(portafolio2,input$rangofechas[1],input$rangofechas[2])
        estadisticas2 <- data.frame(estadisticas2, row.names = c("Mínimo","Máximo","Promedio","Volatilidad","Sortino"))
        colnames(estadisticas2) <- c("Estadísticas Mensuales")
        values[["estadisticas2"]] <- estadisticas2
        
      } else {
        
        rendimientos <- rendimiento_portafolios(input$monto_inversion,portafolio1,NULL,input$rangofechas[1],input$rangofechas[2])
        values[["rendimiento"]] <- data.frame(rendimientos)
        
      }
    }
    
    output$primerportafolio <- renderRHandsontable({
      df <- values[["df"]]
      rhandsontable(df, rowHeaders = NULL, stretchH = "all") %>%
        hot_col("Fondos", type = "dropdown", source = fondos_industria,strict = FALSE) %>%
        hot_col("Porcentaje", format = "0%")
    })
    
    output$segundoportafolio <- renderRHandsontable({
      df2 <- values[["df2"]]
      rhandsontable(df2, rowHeaders = NULL, stretchH = "all") %>%
        hot_col("Fondos", type = "dropdown", source = fondos_industria,strict = FALSE) %>%
        hot_col("Porcentaje", format = "0%")
    })
    
    output$primerpie <- renderPlotly({
      portafolio1 <- values[["df"]]
      portafolio1 <- portafolio1 %>% filter(Fondos %in% fondos_industria & Porcentaje > 0)
      values[["ci_pie"]] <- plot_ly(portafolio1, labels = ~Fondos, values = ~Porcentaje, type = 'pie', textinfo = 'label+percent',
                   insidetextfont = list(color = '#FFFFFF'), marker = list(line = list(color = '#FFFFFF', width = 1))) %>%
        layout(hovermode = FALSE, title = '',xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    })
    
    output$segundopie <- renderPlotly({
      portafolio2 <- values[["df2"]]
      portafolio2 <- portafolio2 %>% filter(Fondos %in% fondos_industria & Porcentaje > 0)
      values[["otro_pie"]] <- plot_ly(portafolio2, labels = ~Fondos, values = ~Porcentaje, type = 'pie', textinfo = 'label+percent',
                   insidetextfont = list(color = '#FFFFFF'), marker = list(line = list(color = '#FFFFFF', width = 1))) %>%
        layout(hovermode = FALSE, title = '',xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    })
    
    output$grafica <- renderPlotly({
      rendimientos <- values[["rendimiento"]]
      values[["grafica_rendimiento"]] <- plot_ly(rendimientos, x = ~Fecha, y = ~round(value,digits = 2), color = ~series, type = 'scatter', mode = 'lines',
              linetype = ~series, connectgaps = TRUE) %>%
        layout(hovermode = 'compare', title = '',xaxis = list(title = ''),
               yaxis = list(title = '',tickformat = "$,3.2"))
    })
    
    output$primerestadistica <- renderDataTable({
      estadisticas1 <- values[["estadisticas1"]]
      datatable(estadisticas1,options = list(dom = 't', pageLength = 100))
    })
    
    output$segundaestadistica <- renderDataTable({
      estadisticas2 <- values[["estadisticas2"]]
      datatable(estadisticas2,options = list(dom = 't', pageLength = 100))
    })
    
    output$value <- renderText({ input$nombre })
    
  })
  
  output$archivecreator <- downloadHandler(
    
    filename = function() {"comparativo.html"},
    
    if(input$comparativo){
      
      content = function(file) {
        tempReport <- file.path(tempdir(), "dos_portafolios.Rmd")
        file.copy("dos_portafolios.Rmd", tempReport, overwrite = TRUE)
        params <- list(nombre = as.character(input$nombre),
                       ci_portafolio = values$df,
                       otro_portafolio = values$df2,
                       grafica_rendimiento = values$grafica_rendimiento,
                       ci_summary = values$estadisticas1,
                       otro_summary = values$estadisticas2,
                       ci_pie = values$ci_pie,
                       otro_pie = values$otro_pie)
        
        rmarkdown::render(tempReport, output_file = file,params = params, envir = new.env(parent = globalenv()))}
      
    } else {
      
      content = function(file) {
        tempReport <- file.path(tempdir(), "un_portafolio.Rmd")
        file.copy("un_portafolio.Rmd", tempReport, overwrite = TRUE)
        params <- list(nombre = as.character(input$nombre),
                       ci_portafolio = values$df,
                       grafica_rendimiento = values$grafica_rendimiento,
                       ci_summary = values$estadisticas1,
                       ci_pie = values$ci_pie)
        
        rmarkdown::render(tempReport, output_file = file,params = params, envir = new.env(parent = globalenv()))}
      
    }
    )
})
