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
  titlePanel("Games Metrics Analysis"),
  
  sidebarLayout(
    sidebarPanel(
      tabsetPanel(id="graphsList", 
        tabPanel(
          "Fluctuation over the years",
          selectizeInput("gameQuery", "Search for a game:", choices=NULL,
                         options=list(placeholder="search here...", maxItems=1, openOnFocus=T)),
          selectInput("metric", "Select a metric:", 
                      choices = c("avg", "gain", "peak"),
                      selected = "avg")
        ),
        tabPanel("Top 10 games by metric", 
          selectInput("year", "Select Year:", choices = NULL),
          selectInput("month", "Select Month:", choices = NULL),
          selectInput("metric", "Select Metric:", choices = c("avg", "gain", "peak"))
        )
      )
    ),
    mainPanel(
      plotOutput("metricPlot")
    )
  )
)

server <- function(input, output, session) {
  
  ## the first two observe calls are for the first plot
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
  
  ## this observe call is for the second plot
  observe({
    updateSelectInput(session, "year", choices = sort(unique(games$year)))
    updateSelectInput(session, "month", choices = unique(games$month))
  })
  
  ## render a different plot based on what tab is selected
  output$metricPlot <- renderPlot({
    if (input$graphsList == "Fluctuation over the years") {
      # ensure the game and metric are selected
      req(input$gameQuery, input$metric)
      filteredGameAndMetric <- games %>%
        filter(gamename == input$gameQuery) %>%
        select(monthYr, gamename, all_of(input$metric))
      
      ggplot(filteredGameAndMetric, aes(x = monthYr, y = !!sym(input$metric))) +
        geom_line(color = "blue") +
        geom_point() +
        labs(title = paste("Fluctuation of", input$metric, "for", input$gameQuery),
             x = "Month / Year", y = input$metric)
      
    } else if (input$graphsList == "Top 10 games by metric") {
      req(input$year, input$month, input$metric)
      top10Games <- games %>%
        filter(year == input$year, month == input$month) %>%
        arrange(desc(!!sym(input$metric))) %>%
        slice_head(n = 10)
      if (nrow(top10Games) == 0) {
        return(ggplot() + 
                 labs(title = "No data available for the selected year/month/metric"))
      }
      ggplot(top10Games, aes(x = reorder(gamename, !!sym(input$metric)), 
                            y = !!sym(input$metric), 
                            group = 1)) +
        geom_line(aes(color = gamename)) +
        geom_point() +
        labs(
          x = "Game",
          y = input$metric,
          title = paste("Top 10 Games in", input$month, input$year, "by", input$metric)
        ) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    }
  })
}
shinyApp(ui, server)