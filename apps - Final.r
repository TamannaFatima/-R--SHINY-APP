library(shiny)
library(ggplot2)
library(dplyr)

# Assuming 'Movies' is your dataset
Movies <- read.csv("Movies.csv")
rawdata <- read.csv("Rawdata.csv")

# Define UI
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      body { 
        background-color: #808080; /* Grey background */
        color: black; /* Black text for contrast */
      }
      .well { 
        background-color: #A9A9A9; /* Lighter grey for sidebar and panels */
        border-color: #A9A9A9;
      }
      .btn-default { 
        color: black; 
        background-color: #D3D3D3; /* Light grey for buttons */
        border-color: black;
      }
      .navbar-default { 
        background-color: #A9A9A9; 
        border-color: #A9A9A9; 
      }
      .nav-tabs > li > a { 
        color: black; /* Black text for tabs */
      }
    "))
  ),
  titlePanel("Movie Data Analysis", windowTitle = "Movie Data Analysis"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("budget_slider", "Budget", min = min(Movies$budget), max = max(Movies$budget), value = c(min(Movies$budget), max(Movies$budget))),
      selectInput("selected_year", "Select Year", choices = unique(Movies$Year), selected = unique(Movies$Year)),
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Data", dataTableOutput("rawdata")),
        tabPanel("Cleaned data", dataTableOutput("data_table")),
        tabPanel("Budget vs. Vote Count", plotOutput("budget_vs_vote_count_plot")),
        tabPanel("Runtime vs Vote Average", plotOutput("runtime_vs_vote_average")),
        tabPanel("Model Results", verbatimTextOutput("model_results"))
      )
    )
  )
)

# Define server logic
server <- function(input, output) {
  observe({
    # Use input$budget_slider, input$revenue_slider, input$selected_year, input$vote_count_input in your data filtering or plot generation
    # For example:
    filtered_data <- Movies %>%
      filter(budget >= input$budget_slider[1] & budget <= input$budget_slider[2],
             Year == input$selected_year)
    # Update the data table
    output$data_table <- renderDataTable({
      filtered_data
    })
    output$rawdata <- renderDataTable({
      rawdata
    })
    # Update the "Budget vs. Vote Count" plot
    output$budget_vs_vote_count_plot <- renderPlot({
      ggplot(filtered_data, aes(x = budget, y = vote_count)) +
        geom_point(color = "blue") +
        labs(title = "Budget vs. Vote Count", x = "Budget", y = "Vote Count")
    })
    # Update the "Runtime vs Vote Average" plot
    output$runtime_vs_vote_average <- renderPlot({
      ggplot(filtered_data, aes(x = runtime, y = vote_average)) +
        geom_point(color = "green") +
        labs(title = "Runtime vs Vote Average", x = "runtime", y = "Vote Average")
    })
    # Render the entire model summary
    output$model_results <- renderPrint({
      summary(lm(vote_count ~ budget + revenue + popularity + vote_average, data = filtered_data))
    })
  })
}

# Run the application
shinyApp(ui = ui, server = server)
