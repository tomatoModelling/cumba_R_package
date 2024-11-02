library(shiny)
library(ggplot2)
library(dplyr)
library(readr)

# Define UI for application
ui <- fluidPage(
  titlePanel("Tomato Crop Yield and Brix Model (CUMBA)"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("weatherFile", "Upload Weather Data", accept = c(".csv")),
      fileInput("paramFile", "Upload Model Parameters", accept = c(".csv")),
      fileInput("irrigationFile", "Upload Irrigation Schedule", accept = c(".csv")),
      
      checkboxInput("estimateRad", "Estimate Radiation (Hargreaves Model)", value = TRUE),
      checkboxInput("estimateET0", "Estimate ET0 (Hargreaves Model)", value = TRUE),
      
      actionButton("runModel", "Run Model")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Summary", tableOutput("modelSummary")),
        tabPanel("Graphs", plotOutput("yieldPlot")),
        tabPanel("Detailed Output", tableOutput("detailedOutput"))
      )
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  # Reactive values to store uploaded data
  weatherData <- reactive({
    req(input$weatherFile)
    read.csv(input$weatherFile$datapath)
  })
  
  paramData <- reactive({
    req(input$paramFile)
    read.csv(input$paramFile$datapath)
  })
  
  irrigationData <- reactive({
    req(input$irrigationFile)
    read.csv(input$irrigationFile$datapath)
  })
  
  # Run the CUMBA model when the user clicks "Run Model"
  modelResults <- eventReactive(input$runModel, {
    req(weatherData(), paramData(), irrigationData())
    
    # Run the cumba_experiment function
    results <- cumba_experiment(
      weather = weatherData(),
      param = paramData(),
      estimateRad = input$estimateRad,
      estimateET0 = input$estimateET0,
      irrigation_df = irrigationData()
    )
    
    results
  })
  
  # Display a summary table of the results
  output$modelSummary <- renderTable({
    req(modelResults())
    summary <- modelResults() %>%
      summarise(
        TotalYield = sum(fruitFreshWeightAct, na.rm = TRUE),
        AvgBrix = mean(brixAct, na.rm = TRUE)
      )
    summary
  })
  
  # Plot the yield over time
  output$yieldPlot <- renderPlot({
    req(modelResults())
    ggplot(modelResults(), aes(x = doy, y = fruitFreshWeightAct, color = site)) +
      geom_line() +
      labs(title = "Yield Over Time", x = "Day of Year", y = "Fruit Fresh Weight (kg/ha)")
  })
  
  # Display the detailed model output as a table
  output$detailedOutput <- renderTable({
    req(modelResults())
    modelResults()
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)
