# Project Guidelines

dt <- read.csv("results.csv")

library(tidyverse)
glimpse(dt)
View(dt)
summary(dt)
row.names(dt)
colnames(dt)


Euro <- dt[dt$tournament %in% c("UEFA Euro", "UEFA Euro qualification"), ]
View(Euro)

library(dplyr)


Euro <- Euro %>%
  mutate(
    
    win_rating = case_when(
      home_score > away_score ~ 1,   # Home team wins
      home_score < away_score ~ 0,   # Away team wins
      TRUE ~ 2                       # Tie
    ),
    
    match_result = case_when(
      home_score > away_score ~ home_team,  # Home team wins
      home_score < away_score ~ away_team,  # Away team wins
      TRUE ~ "tie"                          # Tie
    )
  )


View(Euro)

EuroMatch <- dt[dt$tournament %in% c("UEFA Euro"), ]
View(EuroMatch)
