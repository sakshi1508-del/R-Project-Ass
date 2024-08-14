library(shiny)
library(ggplot2)
library(plotly)
library(dplyr)
library(readr)

ui <- fluidPage(
  titlePanel("MTCars Data Visualization"),
  sidebarLayout(
    sidebarPanel(
      selectInput("x_var", "Select X-axis Variable:", choices = names(mtcars)),
      selectInput("y_var", "Select Y-axis Variable:", choices = names(mtcars)),
      sliderInput("cyl", "Filter by Number of Cylinders:",
                  min = min(mtcars$cyl), max = max(mtcars$cyl), 
                  value = c(min(mtcars$cyl), max(mtcars$cyl))),
      selectInput("trans", "Transmission Type:", choices = c("All", "Automatic", "Manual")),
      sliderInput("hp", "Horsepower Range:", min = min(mtcars$hp), max = max(mtcars$hp), value = c(min(mtcars$hp), max(mtcars$hp))),
      checkboxInput("add_regression", "Add Regression Line", value = FALSE),
      selectInput("plot_type", "Plot Type:", choices = c("Scatter Plot", "Line Plot", "Bar Plot")),
      selectInput("color_var", "Select Color Variable:", choices = c("None", names(mtcars))),
      sliderInput("point_size", "Point Size:", min = 1, max = 10, value = 3),
      selectInput("transformation", "Transformation:", choices = c("None", "Log", "Square Root")),
      actionButton("stat_test", "Perform Statistical Test"),
      downloadButton("download_data", "Download Filtered Data"),
      downloadButton("download_plot", "Download Plot")
    ),
    mainPanel(
      plotlyOutput("plot"),
      tableOutput("summary_table"),
      verbatimTextOutput("stat_test_result")
    )
  )
)

server <- function(input, output, session) {
  
  filtered_data <- reactive({
    data <- mtcars %>%
      filter(cyl >= input$cyl[1] & cyl <= input$cyl[2])
    
    if (input$trans != "All") {
      data <- data %>%
        filter(am == ifelse(input$trans == "Automatic", 0, 1))
    }
    
    data <- data %>%
      filter(hp >= input$hp[1] & hp <= input$hp[2])
    
    data
  })
  
  output$plot <- renderPlotly({
    data <- filtered_data()
    
    x_var <- switch(input$transformation,
                    "Log" = log(data[[input$x_var]]),
                    "Square Root" = sqrt(data[[input$x_var]]),
                    data[[input$x_var]])
    
    y_var <- switch(input$transformation,
                    "Log" = log(data[[input$y_var]]),
                    "Square Root" = sqrt(data[[input$y_var]]),
                    data[[input$y_var]])
    
    p <- ggplot(data, aes_string(x = x_var, y = y_var)) +
      geom_point(aes_string(color = if(input$color_var != "None") input$color_var else NULL), size = input$point_size) +
      theme_minimal()
    
    if (input$add_regression) {
      p <- p + geom_smooth(method = "lm")
    }
    
    p <- switch(input$plot_type,
                "Scatter Plot" = p,
                "Line Plot" = p + geom_line(),
                "Bar Plot" = p + geom_bar(stat = "identity"))
    
    ggplotly(p, tooltip = "text")
  })
  
  output$summary_table <- renderTable({
    summary(filtered_data())
  })
  
  observeEvent(input$stat_test, {
    data <- filtered_data()
    test_result <- t.test(data[[input$x_var]], data[[input$y_var]])
    output$stat_test_result <- renderPrint({
      test_result
    })
  })
  
  output$download_data <- downloadHandler(
    filename = function() { paste("filtered_data", ".csv", sep = "") },
    content = function(file) {
      write_csv(filtered_data(), file)
    }
  )
  
  output$download_plot <- downloadHandler(
    filename = function() { paste("plot", ".png", sep = "") },
    content = function(file) {
      ggsave(file, plot = last_plot())
    }
  )
}

shinyApp(ui, server)