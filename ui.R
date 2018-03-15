source("df.R",local = FALSE)

shinyUI(fluidPage(

  titlePanel("Portafolio de Fondos CI Estrategias"),

  sidebarLayout(
    sidebarPanel(
      h3("Instrucciones"),
      "1. Escribir los portafolios a comparar o el portafolio del que se busca el rendimiento.",
      br(),
      "2. Elegir el Monto a Invertir y el Plazo.",
      br(),
      "3. Observar el resultado de los cálculos. \n",
      br(), br(),
      wellPanel(h4("Monto a Invertir"),numericInput("monto_inversion","Monto",100000,min=100000)),
      wellPanel(dateRangeInput('rangofechas',label = 'Rango de fechas para el comparativo',start = Sys.Date()-253, 
                     end = Sys.Date()-1,language = "es",separator = "a")),
      wellPanel(downloadButton("pdf_creator", "Generar PDF"))),
    
    mainPanel(
      h3("Portafolios"),
      br(),
      fluidRow(
        column(width = 6, h4("Portafolio de CI Estrategias")),
        column(width = 6, h4("Portafolio Comparativo"))),
      fluidRow(
        column(width = 6, rHandsontableOutput("primerportafolio")),
        column(width = 6, rHandsontableOutput("segundoportafolio"))),
      br(), br(),
      h3("Composición de los Portafolios"),
      br(),
      fluidRow(
        column(width = 6, h4("Portafolio de CI Estrategias")),
        column(width = 6, h4("Portafolio Comparativo"))),
      fluidRow(
        column(width = 6, plotlyOutput("primerpie")),
        column(width = 6, plotlyOutput("segundopie"))),
      br(), br(),
      h3("Gráfica de Rendimientos"),
      br(),
      plotlyOutput("grafica"),
      br(), br(),
      h3("Estadísticas de los Portafolios"),
      br(),
      fluidRow(
        column(width = 6, h4("Portafolio de CI Estrategias")),
        column(width = 6, h4("Portafolio Comparativo"))),
      fluidRow(
        column(width = 6, dataTableOutput("primerestadistica")),
        column(width = 6, dataTableOutput("segundaestadistica"))),
      br(), br())
)))