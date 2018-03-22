source("df.R",local = FALSE)

shinyUI(fluidPage(

  titlePanel("Portafolio de Fondos CI Estrategias"),

  sidebarLayout(
    sidebarPanel(
      h3("Instrucciones"),
      "1. Escribir el nombre del promotor.",
      br(),
      "2. Escribir el nombre del cliente",
      br(),
      "3. Elegir el Monto a invertir.",
      br(),
      "4. Elegir el perfil del cliente.",
      br(),
      "5. Decidir si es un comparativo o una propuesta de un portafolio.",
      br(),
      "6. Elegir el rango de fechas.",
      br(),
      "7. Generar archivo.",
      br(), br(),
      wellPanel(h4("Nombre del Promotor"),textInput("promotor","","")),
      wellPanel(h4("Nombre del Cliente"),textInput("nombre","","")),
      wellPanel(h4("Monto a Invertir"),numericInput("monto_inversion","",100000,min=100000)),
      wellPanel(h4("Perfil del Cliente"),selectInput("perfil_inversion","",c("Conservador", "Moderado","Agresivo"),
                                                   "Conservador")),
      wellPanel(h4("¿Quieres realizar un comparativo de fondos?"),checkboxInput("comparativo", "Sí", FALSE)),
      wellPanel(dateRangeInput('rangofechas',label = 'Rango de fechas para el comparativo',start = Sys.Date()-253, 
                     end = Sys.Date()-1,language = "es",separator = "a")),
      wellPanel(downloadButton("archivecreator", "Generar archivo"))),
    
    mainPanel(
      h3("Portafolios"),
      br(),
      fluidRow(
        column(width = 6, h4("Portafolio de CI Estrategias")),
        conditionalPanel(condition = "input.comparativo",column(width = 6, h4("Portafolio Comparativo")))),
      fluidRow(
        column(width = 6, rHandsontableOutput("primerportafolio")),
        conditionalPanel(condition = "input.comparativo",column(width = 6, rHandsontableOutput("segundoportafolio")))),
      fluidRow(
        column(width = 6, verbatimTextOutput("primer100", placeholder = FALSE)),
        conditionalPanel(condition = "input.comparativo",column(width = 6, verbatimTextOutput("segundo100", placeholder = FALSE)))),
      br(), br(),
      h3("Composición de los Portafolios"),
      br(),
      fluidRow(
        column(width = 6, h4("Portafolio de CI Estrategias")),
        conditionalPanel(condition = "input.comparativo",column(width = 6, h4("Portafolio Comparativo")))),
      fluidRow(
        column(width = 6, plotlyOutput("primerpie")),
        conditionalPanel(condition = "input.comparativo",column(width = 6, plotlyOutput("segundopie")))),
      br(), br(),
      h3("Gráfica de Rendimientos"),
      br(),
      plotlyOutput("grafica"),
      br(), br(),
      h3("Estadísticas de los Portafolios"),
      br(),
      fluidRow(
        column(width = 6, h4("Portafolio de CI Estrategias")),
        conditionalPanel(condition = "input.comparativo",column(width = 6, h4("Portafolio Comparativo")))),
      fluidRow(
        column(width = 6, dataTableOutput("primerestadistica")),
        conditionalPanel(condition = "input.comparativo",column(width = 6, dataTableOutput("segundaestadistica")))),
      br(),
      h3("Rendimiento del Portafolio CI Estrategias"),
      br(),
      fluidRow(
        column(width = 6, h4("Portafolio de CI Estrategias")),
        column(width = 6, h4("Portafolio Comparativo"))
      ),
      fluidRow(
        column(width = 6,dataTableOutput("primerrendimiento")),
        column(width = 6,dataTableOutput(NULL))
      ),
      br(), br())
)))
