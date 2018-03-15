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
  
  output$primerportafolio <- renderRHandsontable({
    df <- values[["df"]]
    if(!is.null(df)){
      rhandsontable(df, rowHeaders = NULL, stretchH = "all") %>%
        hot_col("Fondos", type = "dropdown", source = fondos_industria,strict = FALSE) %>%
        hot_col("Porcentaje", format = "0%")
    }
  })
  
  output$segundoportafolio <- renderRHandsontable({
    df2 <- values[["df2"]]
    if(!is.null(df2)){
      rhandsontable(df2, rowHeaders = NULL, stretchH = "all") %>%
        hot_col("Fondos", type = "dropdown", source = fondos_industria,strict = FALSE) %>%
        hot_col("Porcentaje", format = "0%")
    }
  })
  
  observe({
    portafolio1 <- values[["df"]]
    portafolio2 <- values[["df2"]]
    if(!is.null(portafolio1)){
      rendimientos <- rendimiento_portafolios(input$monto_inversion,portafolio1,portafolio2,input$rangofechas[1],input$rangofechas[2])
      values[["rendimiento"]] <- data.frame(rendimientos)
      
      estadisticas1 <- estadisticas_portafolios(portafolio1,fecha_inicio,fecha_fin)
      estadisticas1 <- data.frame(estadisticas1, row.names = c("Mínimo","Máximo","Promedio","Volatilidad","Sortino"))
      colnames(estadisticas1) <- c("Estadísticas Mensuales")
      values[["estadisticas1"]] <- estadisticas1
      if(!is.null(portafolio2)){
        estadisticas2 <- estadisticas_portafolios(portafolio2,fecha_inicio,fecha_fin)
        estadisticas2 <- data.frame(estadisticas2, row.names = c("Mínimo","Máximo","Promedio","Volatilidad","Sortino"))
        colnames(estadisticas2) <- c("Estadísticas Mensuales")
        values[["estadisticas2"]] <- estadisticas2
      }
    }
    
    output$primerpie <- renderPlotly({
      portafolio1 <- values[["df"]]
      portafolio1 <- portafolio1 %>% filter(Fondos %in% fondos_industria & Porcentaje > 0)
      p <- plot_ly(portafolio1, labels = ~Fondos, values = ~Porcentaje, type = 'pie', textinfo = 'label+percent',
                   insidetextfont = list(color = '#FFFFFF'), marker = list(line = list(color = '#FFFFFF', width = 1))) %>%
        layout(hovermode = FALSE, title = '',xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    })
    
    output$segundopie <- renderPlotly({
      portafolio2 <- values[["df2"]]
      portafolio2 <- portafolio2 %>% filter(Fondos %in% fondos_industria & Porcentaje > 0)
      p <- plot_ly(portafolio2, labels = ~Fondos, values = ~Porcentaje, type = 'pie', textinfo = 'label+percent',
                   insidetextfont = list(color = '#FFFFFF'), marker = list(line = list(color = '#FFFFFF', width = 1))) %>%
        layout(hovermode = FALSE, title = '',xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    })
    
    output$grafica <- renderPlotly({
      rendimientos <- values[["rendimiento"]]
      plot_ly(rendimientos, x = ~Fecha, y = ~round(value,digits = 2), color = ~series, type = 'scatter', mode = 'lines',
              linetype = ~series, connectgaps = TRUE) %>%
        layout(hovermode = 'compare', title = '',xaxis = list(title = ''),
               yaxis = list(title = '',tickformat = "$,3.2"))
    })
    
    output$primerestadistica <- renderDataTable({
      datatable(values[["estadisticas1"]],options = list(dom = 't', pageLength = 100))
    })
    
    output$segundaestadistica <- renderDataTable({
      datatable(values[["estadisticas2"]],options = list(dom = 't', pageLength = 100))
    })
  })
  
  output$pdf_creator <- downloadHandler(
    filename = "comparativo.pdf",
    content = function(file) {
      tempReport <- file.path(tempdir(), "archivo.Rmd")
      file.copy("archivo.Rmd", tempReport, overwrite = TRUE)
      
      params <- list(values)
      
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
    }
  )
})
