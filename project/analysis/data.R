# Import libraries

library(dplyr)
library(lubridate)

# Read the data files

df_country <- read.csv("../data/country.csv")


euros_csv <- list.files(
  path = "../data/euro",
  pattern = "\\.csv$",
  full.names = TRUE
)

read_csv <- function(file) {
  df <- read.csv(file)

  df[] <- lapply(df, as.character)
  return(df)
}

df_euros <- bind_rows(lapply(euros_csv, read_csv))

glimpse(df_euros)

df_euros <- df_euros %>% mutate(
  id_match = as.integer(id_match),
  home_score = as.integer(home_score),
  away_score = as.integer(away_score),
  home_penalty = as.integer(home_penalty),
  away_penalty = as.integer(away_penalty),
  home_score_total = as.integer(home_score_total),
  away_score_total = as.integer(away_score_total),

  winner_reason = as.factor(winner_reason),

  year = as.integer(year),
  date = ymd(date),
  date_time = ymd_hms(date_time),
  utc_offset_hours = as.integer(utc_offset_hours),
  group_name = as.factor(group_name),
  matchday_name = as.factor(matchday_name),

  type = as.factor(type),
  round = as.factor(round),
  round_mode = as.factor(round_mode),

  # Fix more columns if needed
)

glimpse(df_euros)

# Get unique countries that participated

unique_countries_h <- unique(df_euros$home_team)
unique_countries_a <- unique(df_euros$away_team)
unique_countries <- union(unique_countries_h, unique_countries_a)

unique_countries <- gsub("Türkiye", "Turkiye", unique_countries)

df_country_euro <- df_country %>%
  filter(Country.Name %in% unique_countries)

# Filter the euros data to after 1992 (All the coutnries that exist now)
df_euros_modern <- df_euros %>%
  filter(year >= 1992)

