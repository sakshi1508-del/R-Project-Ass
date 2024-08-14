ibrary(shiny)
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