library(shiny)
library(dplyr)
# downloading and cleaning the data
games <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2021/2021-03-16/games.csv')
games <- games %>%
  filter(
    !is.na(avg) & avg != 0,
    !is.na(gain) & gain != 0,
    !is.na(peak) & peak != 0
  )

str(games)
head(games$gamename)
#hist(games$avg, games$year) # this line doesn't work, but since it just makes a histogram,
# its not needed (unless we want to make a histogram)

# plots to make:
# 1. user selects month/year and metric, it displays the top n games with that metric for that month/yr
# 2. selectizeInput() in shiny supports typing and searching; user searches for game, chooses metric,
# then it shows a line chart for that metric over all 12 years.
# could also do a mix of 1 and 2 where user searches for game, picks month / year, 
# and there's a scatterplot with x-axis = average players, y-axis = peak players, and bubble size = gain

## Plot 2: user searches for game, chooses metric,
# then it shows a line chart for that metric over all 12 years.
ui <- fluidPage(
  selectizeInput("gameQuery", "Search for a game:", choices=NULL,
                 options=list(placeholder="search here...", maxItems=1, openOnFocus=T)),
  textOutput("selectedGame")
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
}
shinyApp(ui, server)