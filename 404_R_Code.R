# Program to create a shiny app with two interactable graphs using the games dataset
# Miles Adams and Justin Klein
library(dplyr)
library(ggplot2)
library(shiny)
library(plotly)

# downloading and cleaning the data
games <- readr::read_csv(
  'https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2021/2021-03-16/games.csv'
) |> 
  mutate(
    monthNum = match(month, month.name),
    monthYr = as.Date(paste(year, monthNum, "01", sep="-")),
    gamename = gsub("<U\\+\\w{4}>", "", gamename)
  ) |> 
  filter(
    !is.na(avg) & avg != 0,
    !is.na(gain) & gain != 0,
    !is.na(peak) & peak != 0
  )

# plot 1. user selects month/year and metric, it displays the top n games with that metric for that month/yr
# plot 2: user searches for game, chooses metric (avg, peak, or gain),
# then it shows a line chart for that metric over all 12 years.
ui <- fluidPage(
  titlePanel("Games Metrics Analysis"),
  sidebarLayout(
    sidebarPanel(
      tabsetPanel(id="graphsList", 
        tabPanel(
          "Fluctuation over the years",
          selectizeInput(
            "gameQuery", "Search for a game:",
            choices = NULL,
            options = list(placeholder="search here...", maxItems=1, openOnFocus=T)
          ),
          selectInput("metric","Select a metric:", c("avg","gain","peak"), selected="avg")
        ),
        tabPanel("Top 10 games by metric", 
                 selectInput("year",  "Select Year:",  choices=NULL),
                 selectInput("month", "Select Month:", choices=NULL),
                 selectInput("metric","Select Metric:", c("avg","gain","peak"))
        )
      )
    ),
    mainPanel(
      plotlyOutput("metricPlot")
    )
  )
)

server <- function(input, output, session) {
  
  observe({
    updateSelectizeInput(session,"gameQuery",
                         choices = sort(unique(games$gamename)), server=T
    )
  })
  
  observe({
    updateSelectInput(session, "year",  choices = sort(unique(games$year)))
    months_present <- intersect(month.name, unique(games$month))
    updateSelectInput(session, "month", choices = months_present)
  })
  
  # this observe call is to reset the selected metric - both tabs can't track it at once
  observe({
    if(input$graphsList == "Fluctuation over the years")
      updateSelectInput(session, "metric", selected = "avg")
    if(input$graphsList == "Top 10 games by metric")
      updateSelectInput(session, "metric", selected = "avg")
  })
  
  
  output$metricPlot <- renderPlotly({
    # render a different plot based on what tab is selected
    if (input$graphsList=="Fluctuation over the years") {
      req(input$gameQuery, input$metric)
      filteredGame <- games |> filter(gamename==input$gameQuery)
      plot1 <- ggplot(filteredGame, aes(x=monthYr, y=!!sym(input$metric))) +
        geom_line(color="#C8102E") +
        labs(
          title = paste("Fluctuation of", input$metric, "for", input$gameQuery),
          x="Year", y=input$metric
        ) +
        theme_bw() +
        scale_x_date(date_breaks="1 year", date_labels="%Y") +
        scale_y_continuous(labels=scales::label_comma())
      
      ggplotly(plot1)
      
    } else {
      req(input$year, input$month, input$metric)
      top10 <- games |>
        filter(year==input$year, month==input$month) |>
        arrange(desc(!!sym(input$metric))) |>
        slice_head(n=10)
      
      if (nrow(top10)==0) {
        plot2 <- ggplot() + labs(title="No data available for the selected year/month/metric")
      } else {
        plot2 <- ggplot(top10, aes(
          x = reorder(gamename, !!sym(input$metric)),
          y = !!sym(input$metric),
          fill = gamename
        )) +
          geom_col(show.legend=F) +
          labs(
            x="Game", y=input$metric,
            title=paste("Top 10 Games in", input$month, input$year, "by", input$metric)
          ) +
          theme_bw() +
          theme(axis.text.x=element_text(angle=45, hjust=1)) +
          scale_y_continuous(labels=scales::label_comma()) + 
          theme(legend.position = "none")
      }
      
      ggplotly(plot2)
    }
  })
}

shinyApp(ui, server)
