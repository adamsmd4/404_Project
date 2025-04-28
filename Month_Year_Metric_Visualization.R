## install.packages("shiny")
library(shiny)
library(dplyr)
library(ggplot2)


games <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2021/2021-03-16/games.csv') %>%
  filter(!is.na(avg), avg > 0, !is.na(gain), gain > 0, !is.na(peak), peak > 0)


# UI
ui <- fluidPage(
  titlePanel("Top 10 Games by Metric"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("year", "Select Year:", choices = NULL),
      selectInput("month", "Select Month:", choices = NULL),
      selectInput("metric", "Select Metric:", choices = c("avg", "gain", "peak"))
    ),
    
    mainPanel(
      plotOutput("line_plot")
    )
  )
)

# Server
server <- function(input, output, session) {
  
  observe({
    updateSelectInput(session, "year", choices = sort(unique(games$year)))
    updateSelectInput(session, "month", choices = sort(unique(games$month)))
  })
  
  output$line_plot <- renderPlot({
    req(input$year, input$month, input$metric)
    
    top_games <- games %>%
      filter(year == input$year, month == input$month) %>%
      arrange(desc(!!sym(input$metric))) %>%
      slice_head(n = 10)
    
    ggplot(top_games, aes(x = reorder(gamename, !!sym(input$metric)), 
                          y = !!sym(input$metric), 
                          group = 1)) +
      geom_line(aes(color = gamename)) +
      geom_point() +
      labs(
        x = "Game",
        y = input$metric,
        title = paste("Top 10 Games in", input$month, input$year, "by", input$metric)
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
}

# Run the app
shinyApp(ui = ui, server = server)