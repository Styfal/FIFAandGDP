library(ggplot2)
library(stargazer)
library(dplyr)

#-----------------------------------#

# Simple Scatter Plot + Regression

df_raw <- read.csv("../data/raw_qual2.csv")
df_raw <- na.omit(df_raw)


df_raw <- df_raw %>% mutate(
  gdp_per_capita = (as.numeric(gdp_per_capita)),
  goals_per_game = as.numeric(goals_per_game),
  win_percentage = as.numeric(win_percentage),
  total_goals = as.numeric(total_goals),
)

ggplot(
  df_raw, aes(x = gdp_per_capita, y = total_goals)
) + geom_point(
) + geom_smooth(
  method = "loess",
  se = FALSE
)

ggplot(
  df_raw, aes(x = gdp_per_capita, y = win_percentage)
) + geom_point(
) + geom_smooth(
  method = "lm",
  formula = y ~ log(x),
  se = FALSE
)

#-----------------------------------#


df_raw <- read.csv("../data/raw_qual.csv")
df_raw <- na.omit(df_raw)


df_raw <- df_raw %>% mutate(
  gdp = (as.numeric(gdp)) / 1e12 * 100,
  goals_per_game = as.numeric(goals_per_game),
  win_percentage = as.numeric(win_percentage),
  total_goals = as.numeric(total_goals),
)

ggplot(
  df_raw, aes(x = gdp, y = total_goals)
) + geom_point(
) + geom_smooth(
  method = "loess",
  se = FALSE
)

ggplot(
  df_raw, aes(x = gdp, y = win_percentage)
) + geom_point(
) + geom_smooth(
  method = "lm",
  formula = y ~ log(x),
  se = FALSE
)

#-----------------------------------#

# Time? Series

coefs <- df_raw %>%
  group_by(year) %>%
  summarize(slope = coef(lm(win_percentage ~ gdp, data = cur_data()))[2])

fit_time <- lm(slope ~ year, data = coefs)

ggplot(coefs, aes(x = year, y = slope)) +
  geom_point() +  # Scatter plot of coefficients
  geom_smooth(method = "lm", se = FALSE, color = "blue") +  # Regression line
  labs(title = "Regression of Coefficients Over Time",
       x = "Year",
       y = "Slope Coefficient (win_rate ~ gdp)") +
  theme_minimal()


# Actual Models


fit <- lm(goals_per_game ~ gdp + total_games, data = df_raw)
stargazer(fit, type = "text")

fit2 <- lm(win_percentage ~ gdp + total_games, data = df_raw)
stargazer(fit2, type = "text")

fit3 <- lm(win_percentage ~ gdp * total_games, data = df_raw)
stargazer(fit3, type = "text")

fit4 <- lm(win_percentage ~ log(gdp) + total_games, data = df_raw)
stargazer(fit4, type = "text")

fit5 <- lm(win_percentage ~ I(gdp^2), data = df_raw)
stargazer(fit5, type = "text")

stargazer(fit, fit2, fit3, fit4, type = "text")
