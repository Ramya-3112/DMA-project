# Load required libraries
library(shiny)
library(randomForest)
library(ggplot2)
library(readr)

# Load dataset (update path as needed)
data <- read_csv("C:/Users/ramya/Downloads/Food_Delivery_Times.csv")

# Remove missing values
data <- na.omit(data)

# Train Random Forest Model
set.seed(123)
model <- randomForest(
  Delivery_Time_min ~ Distance_km + Weather + Traffic_Level + 
    Time_of_Day + Vehicle_Type + Preparation_Time_min + Courier_Experience_yrs,
  data = data, ntree = 200
)

# Shiny App
ui <- fluidPage(
  titlePanel("Food Delivery Time Prediction Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      numericInput("distance", "Distance (km):", value = 5, min = 1, max = 30),
      selectInput("weather", "Weather:", choices = unique(data$Weather)),
      selectInput("traffic", "Traffic Level:", choices = unique(data$Traffic_Level)),
      selectInput("timeofday", "Time of Day:", choices = unique(data$Time_of_Day)),
      selectInput("vehicle", "Vehicle Type:", choices = unique(data$Vehicle_Type)),
      numericInput("prep", "Preparation Time (min):", value = 15, min = 5, max = 60),
      numericInput("experience", "Courier Experience (yrs):", value = 2, min = 0, max = 20),
      actionButton("predict", "Predict Delivery Time")
    ),
    
    mainPanel(
      h3("Predicted Delivery Time (Minutes):"),
      textOutput("prediction"),
      plotOutput("plot")
    )
  )
)

server <- function(input, output) {
  
  observeEvent(input$predict, {
    newdata <- data.frame(
      Distance_km = input$distance,
      Weather = input$weather,
      Traffic_Level = input$traffic,
      Time_of_Day = input$timeofday,
      Vehicle_Type = input$vehicle,
      Preparation_Time_min = input$prep,
      Courier_Experience_yrs = input$experience
    )
    
    pred <- predict(model, newdata)
    
    output$prediction <- renderText({
      paste(round(pred), "minutes")
    })
    
    output$plot <- renderPlot({
      ggplot(data.frame(Prediction = pred), aes(x = "Prediction", y = Prediction)) +
        geom_col(fill = "steelblue") +
        labs(x = "", y = "Delivery Time (min)") +
        ylim(0, max(data$Delivery_Time_min)) +
        geom_text(aes(label = round(Prediction)), vjust = -0.5, size = 5) +
        theme_minimal()
    })
  })
}

shinyApp(ui = ui, server = server)
