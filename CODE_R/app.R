# Cargar las librerías necesarias
library(shiny)
library(shinythemes)

# Definir la interfaz de usuario (UI)
ui <- fluidPage(
  theme = shinytheme("cerulean"),
  
  # Título de la aplicación
  titlePanel(
    div(
      h1("Descenso de Gradiente Estocástico", style = "display: inline-block;"),
      img(src = "logo.png", height = "60px", style = "float: right; margin-left: 10px;"),
      img(src = "logo2.png", height = "60px", style = "float: right; margin-left: 10px;")
    )
  ),
  
  # Diseño de la página con barra lateral y panel principal
  sidebarLayout(
    sidebarPanel(
      h3("Configuración"),
      numericInput("learning_rate", "Tasa de aprendizaje (\u03b7):", value = 0.1, step = 0.01),
      numericInput("iterations", "Número de iteraciones:", value = 5, min = 1),
      actionButton("run", "Ejecutar", class = "btn btn-primary"),
      hr(),
      helpText("Ingrese los parámetros y haga clic en 'Ejecutar' para observar los resultados.")
    ),
    
    # Panel principal para mostrar resultados, gráficos y descripción del problema
    mainPanel(
      tabsetPanel(
        tabPanel("Descripción del Problema", 
                 h4("Contexto"),
                 p("En la Facultad de Estadística e Informática (FINESI) de la Universidad Nacional del Altiplano, se investiga la relación entre el tiempo dedicado al estudio diario y el rendimiento académico de los estudiantes. Queremos optimizar una función que prediga el rendimiento académico en función del tiempo de estudio."),
                 p("La función de costo que se busca minimizar es:"),
                 tags$pre("J(w) = (1/n) * \u2211 (y_i - w * x_i)^2"),
                 p("Donde:"),
                 tags$ul(
                   tags$li("x_i: Tiempo de estudio diario de un estudiante i (en horas)."),
                   tags$li("y_i: Rendimiento académico del estudiante i (en una escala de 0 a 20)."),
                   tags$li("w: Parámetro a optimizar."),
                   tags$li("n: Número total de datos disponibles.")
                 ),
                 p("Este modelo ayudará a los estudiantes a identificar el impacto del tiempo de estudio en su rendimiento y optimizar sus horarios.")),
        
        tabPanel("Resultados", 
                 h4("Tabla de Resultados"),
                 tableOutput("resultsTable")),
        
        tabPanel("Visualización", 
                 h4("Gráfico de Relación"),
                 plotOutput("resultsPlot")),
        
        tabPanel("Interpretación Final", 
                 h4("Interpretación y Respuesta"),
                 verbatimTextOutput("finalInterpretation"))
      )
    )
  )
)

# Definir la lógica del servidor
server <- function(input, output) {
  
  # Valores reactivos para almacenar los resultados
  results <- reactiveValues(data = NULL, final_w = NULL)
  
  # Observar el botón "Ejecutar"
  observeEvent(input$run, {
    # Datos iniciales
    data <- data.frame(
      Estudiante = 1:5,
      x = c(2, 3, 1.5, 4, 3.5),  # Horas de estudio
      y = c(12, 15, 10, 18, 16)   # Rendimiento académico
    )
    
    # Inicialización de parámetros
    w <- 0
    eta <- input$learning_rate
    iterations <- input$iterations
    
    # Lista para almacenar los resultados
    results_list <- data.frame(Iteración = numeric(), Estudiante = numeric(), w = numeric())
    
    for (i in 1:iterations) {
      for (j in 1:nrow(data)) {
        x_i <- data$x[j]
        y_i <- data$y[j]
        gradient <- -2 * (y_i - w * x_i) * x_i
        w <- w - eta * gradient
        results_list <- rbind(results_list, data.frame(Iteración = i, Estudiante = j, w = w))
      }
    }
    
    # Guardar los resultados en los valores reactivos
    results$data <- results_list
    results$final_w <- w  # Guardar el último valor de w
  })
  
  # Renderizar la tabla de resultados
  output$resultsTable <- renderTable({
    req(results$data)  # Asegurarse de que los datos existan
    results$data
  })
  
  # Renderizar el gráfico
  output$resultsPlot <- renderPlot({
    req(results$data)  # Asegurarse de que los datos existan
    data <- data.frame(
      x = c(2, 3, 1.5, 4, 3.5),
      y = c(12, 15, 10, 18, 16)
    )
    
    plot(data$x, data$y, pch = 16, col = "blue", xlab = "Horas de estudio", ylab = "Rendimiento académico", 
         main = "Relación entre tiempo de estudio y rendimiento", cex = 1.2)
    
    if (nrow(results$data) > 0) {
      abline(a = 0, b = results$final_w, col = "red", lwd = 2)
      legend("topleft", legend = c("Datos", "Línea de ajuste"), 
             col = c("blue", "red"), pch = c(16, NA), lty = c(NA, 1), bty = "n")
    }
  })
  
  # Renderizar la interpretación final
  output$finalInterpretation <- renderText({
    req(results$final_w)  # Asegurarse de que el valor final de w exista
    paste(
      "El modelo ha encontrado un valor óptimo para el parámetro w, que es:", round(results$final_w, 4), ".",
      "Esto significa que por cada hora adicional de estudio, el rendimiento académico esperado aumenta en aproximadamente",
      round(results$final_w, 4), "unidades. Este resultado valida la hipótesis de que el tiempo de estudio tiene un impacto positivo en el rendimiento académico."
    )
  })
}

# Ejecutar la aplicación
shinyApp(ui = ui, server = server)
