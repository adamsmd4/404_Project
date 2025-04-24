library(dplyr)
library(ggplot2)
library(shiny)

# downloading and cleaning the data
games <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2021/2021-03-16/games.csv')
games <- games %>%
  mutate(monthNum = match(month, month.name),
         monthYr = as.Date(paste(year, monthNum, "01", sep="-"), format="%Y-%m-%d")) %>%
  filter(
    !is.na(avg) & avg != 0,
    !is.na(gain) & gain != 0,
    !is.na(peak) & peak != 0
  )

str(games)
head(games$gamename)

# plots to make:
# 1. user selects month/year and metric, it displays the top n games with that metric for that month/yr
# 2. selectizeInput() in shiny supports typing and searching; user searches for game, chooses metric,
# then it shows a line chart for that metric over all 12 years.
# could also do a mix of 1 and 2 where user searches for game, picks month / year, 
# and there's a scatterplot with x-axis = average players, y-axis = peak players, and bubble size = gain

## Plot 2: user searches for game, chooses metric (avg, peak, or gain),
# then it shows a line chart for that metric over all 12 years.
ui <- fluidPage(
  titlePanel("Fluctuation of a specific metric for a game over the years"),
  
  sidebarLayout(
    sidebarPanel(
      selectizeInput("gameQuery", "Search for a game:", choices=NULL,
                     options=list(placeholder="search here...", maxItems=1, openOnFocus=T)),
      selectInput("metric", "Select a metric:", 
                  choices = c("avg", "gain", "peak"),
                  selected = "avg")
    ),
    mainPanel(
      textOutput("selectedGame"),
      plotOutput("metricPlot")
    )
  )
)

server <- function(input, output, session) {
  observe({
    updateSelectizeInput(session, "gameQuery", 
                         choices = games$gamename, 
                         server = TRUE)
  })
  observe({
    query <- input$gameQuery
    if (query != "" && query != input$gameQuery) {
      filtered <- games %>% 
        filter(grepl(query, gamename, ignore.case=T)) %>%
        pull(gamename) %>% unique()
      if (length(filtered) > 0)
        updateSelectizeInput(session, "gameQuery", choices = filtered, server = TRUE)
      else
        updateSelectizeInput(session, "gameQuery", choices = games$gamename, server = TRUE)
    } 
  })
  output$selectedGame <- renderText({
    paste("You selected:", input$gameQuery)
  })
  output$metricPlot <- renderPlot({
    # Ensure the game and metric are selected
    req(input$gameQuery, input$metric)
    game <- input$gameQuery
    metric <- input$metric
    
    filteredGameAndMetric <- games %>%
      filter(gamename == game) %>%
      select(monthYr, gamename, all_of(metric))
    
    # Plot the selected metric over the years
    ggplot(filteredGameAndMetric, aes(x = monthYr, y = !!sym(metric))) +
      geom_line(color = "blue") +
      geom_point() +
      labs(title = paste("Fluctuation of", metric, "for", game),
           x = "Month / Year", y = metric)
  })
}
shinyApp(ui, server)