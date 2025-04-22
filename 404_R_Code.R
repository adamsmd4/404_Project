# Get the Data

# Read in with tidytuesdayR package 
# Install from CRAN via: 
install.packages("tidytuesdayR")
# This loads the readme and all the datasets for the week of interest

# Either ISO-8601 date or year/week works!

tuesdata <- tidytuesdayR::tt_load('2021-03-16')
tuesdata <- tidytuesdayR::tt_load(2021, week = 12)

games <- tuesdata$games

# Or read in the data manually

games <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2021/2021-03-16/games.csv')
games
head(games)

download.file('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2021/2021-03-16/games.csv', "C:/Users/miles/Downloads/myfile.csv")

games <- games |>
  filter(
    !is.na(avg) & avg != 0,
    !is.na(gain) & gain != 0,
    !is.na(peak) & peak != 0
  )

str(games)
hist(games$avg, games$year)
