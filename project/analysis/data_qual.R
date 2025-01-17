# Import libraries

library(dplyr)
library(lubridate)
library(tidyr)

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

## Load Qualifiers Data
df_euros_qual <- read.csv("../data/euro_qual.csv")

df_euros_qual <- df_euros_qual %>% mutate(
  across(everything(), as.character)
)

df_euros <- bind_rows(lapply(euros_csv, read_csv))
df_euros <- bind_rows(df_euros, df_euros_qual)

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

## Load qualifiers data
df_euros_qual <- read.csv("../data/euro/euro_qual.csv")

# Get unique countries that participated

unique_countries_h <- unique(df_euros$home_team)
unique_countries_a <- unique(df_euros$away_team)
unique_countries <- union(unique_countries_h, unique_countries_a)

unique_countries <- gsub("Türkiye", "Turkiye", unique_countries)
df_euros$home_team <- gsub("Türkiye", "Turkiye", df_euros$home_team)
df_euros$away_team <- gsub("Türkiye", "Turkiye", df_euros$away_team)
df_euros$winner <- gsub("Türkiye", "Turkiye", df_euros$winner)

df_country_euro <- df_country %>%
  filter(Country.Name %in% unique_countries)

# Filter the euros data to after 1992 (All the coutnries that exist now)
df_euros_modern <- df_euros %>%
  filter(year >= 1992)

# Create a new dataframe with countries and gdp per year
games_played <- df_euros_modern %>%
  pivot_longer(cols = c(home_team, away_team), values_to = "country") %>%
  group_by(year, country) %>%
  summarise(total_games = n(), .groups = "drop")

wins <- df_euros_modern %>%
  group_by(year, winner) %>%
  summarise(wins = n(), .groups = "drop") %>%
  rename(country = winner)

euro_win_stats <- left_join(games_played, wins, by = c("year", "country")) %>%
  mutate(
    wins = replace_na(wins, 0),
    # Fill NA with 0 for countries that never won
    win_percentage = (wins / total_games) * 100
  ) %>%
  arrange(year, desc(win_percentage))

glimpse(euro_win_stats)

#--------------------------------------------#

# Step 1: Reshape GDP data to long format (country, year, gdp)

df_country_gdp <- df_country_euro %>% filter(
  Series.Name == "GDP per capita, PPP (current international $)"
)

colnames(df_country_gdp) <- gsub("X([0-9]{4})..YR\\1.", "\\1", colnames(df_country_gdp))

df_country_gdp <- df_country_gdp %>%
  rename(country = Country.Name)

# **Step 1: Reshape GDP dataframe to long format (country, year, gdp)**
gdp_long <- df_country_gdp %>%
  select(-Series.Name, -Country.Code, -Series.Code) %>%  # Remove non-year columns
  pivot_longer(cols = -country, names_to = "year", values_to = "gdp_per_capita") %>%
  mutate(year = as.integer(year))  # Convert year to integer safely

euro_win_stats <- euro_win_stats %>%
  mutate(year = as.integer(year))

final_df <- left_join(euro_win_stats, gdp_long, by = c("country", "year"))
# Step 2: Merge GDP data with win_stats based on country and year

#------

final_df <- final_df %>%
  filter(year != 2024)

tail(final_df)


total_goals <- df_euros_modern %>%
  pivot_longer(cols = c(home_team, away_team), names_to = "team_type", values_to = "country") %>%
  mutate(
    goals = if_else(team_type == "home_team", home_score, away_score)  
  ) %>%
  group_by(year, country) %>%
  summarise(total_goals = sum(goals, na.rm = TRUE), .groups = "drop") 


final_df <- left_join(final_df, total_goals, by = c("year", "country"))


final_df <- final_df %>%
  mutate(
    goals_per_game = total_goals / total_games
  )

View(final_df)

write.csv(final_df, "../data/raw_qual2.csv")
