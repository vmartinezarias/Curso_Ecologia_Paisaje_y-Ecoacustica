library(shiny)
library(ggplot2)
library(readxl)

# Definir la UI
ui <- fluidPage(
  titlePanel("Visualización de Índices"),
  sidebarLayout(
    sidebarPanel(
      fileInput("archivo", "Cargar archivo Excel", accept = c(".xlsx")),  # Botón para cargar archivo
      uiOutput("puntos_ui"),  # Menú dinámico para Puntos de Muestreo
      uiOutput("indice_ui")   # Menú dinámico para Índices
    ),
    mainPanel(
      plotOutput("indicePlot")  # Salida del gráfico
    )
  )
)

# Definir el servidor
server <- function(input, output, session) {

  # Leer el archivo cargado
  datos <- reactive({
    req(input$archivo)  # Requiere que se haya cargado un archivo
    read_excel(input$archivo$datapath)  # Leer el archivo cargado
  })

  # Generar el menú dinámico para Puntos de Muestreo
  output$puntos_ui <- renderUI({
    req(datos())  # Requiere que los datos estén cargados
    selectInput("puntoMuestreo", "Puntos de Muestreo:",
                choices = unique(datos()$PuntoMuestreo),
                selected = unique(datos()$PuntoMuestreo)[1],
                multiple = TRUE)
  })

  # Generar el menú dinámico para Índices
  output$indice_ui <- renderUI({
    req(datos())  # Requiere que los datos estén cargados
    selectInput("indice", "Índice:",
                choices = colnames(datos())[3:ncol(datos())],
                selected = colnames(datos())[3])
  })

  # Crear el gráfico
  output$indicePlot <- renderPlot({
    req(input$puntoMuestreo, input$indice)  # Asegura que los inputs estén disponibles
    datos_filtrados <- datos()[datos()$PuntoMuestreo %in% input$puntoMuestreo, ]
    ggplot(datos_filtrados, aes_string(x = 'Hora', y = input$indice, color = 'PuntoMuestreo')) +
      geom_line() +
      labs(x = 'Hora', y = input$indice, title = paste(input$indice, "por Punto de Muestreo")) +
      theme_minimal() +
      theme(legend.title = element_blank())
  })
}

# Ejecutar la aplicación
shinyApp(ui = ui, server = server)
