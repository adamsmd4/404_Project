# downloading and cleaning the data
remotes::install_github("thebioengineer/tidytuesdayR")
tuesdata <- tidytuesdayR::tt_load('2021-03-16')
games <- tuesdata$games

games <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2021/2021-03-16/games.csv')
games <- games %>%
  filter(
    !is.na(avg) & avg != 0,
    !is.na(gain) & gain != 0,
    !is.na(peak) & peak != 0
  )

str(games)
hist(games$avg, games$year) # this line doesn't work, but since it just makes a histogram,
# its not needed (unless we want to make a histogram)

# plots to make:
# 1. user selects month/year and metric, it displays the top n games with that metric for that month/yr
# 2. selectizeInput() in shiny supports typing and searching; user searches for game, chooses metric,
# then it shows a line chart for that metric over all 12 years.
# could also do a mix of 1 and 2 where user searches for game, picks month / year, 
# and there's a scatterplot with x-axis = average players, y-axis = peak players, and bubble size = gain
