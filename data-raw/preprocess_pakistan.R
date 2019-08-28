
library(tidyverse)
library(devtools)

df <- readr::read_csv("pakistan.csv")
df <- df[-which(df$City %in% c("Tor Ghar")), ]
df <- df %>% na.omit()

cols <- c(
  "Education score", "Toilet", "Population", "School infrastructure score", "Total number of schools", "Primary Schools with single teacher", "Primary Schools with single classroom",
  "Pakistan Economic Growth", "Number of secondary schools", "Electricity", "No Facility",
  "City", "Global Terrorism Index - Pakistan", "Complete Primary Schools", "Building condition satisfactory",
  "Drone attacks in Pakistan", "Drinking water", "Boundary wall", "Bomb Blasts Occurred", "% Complete Primary Schools", "% Boys Enrolled"
)

df <- df[, cols]
percent_boys <- df %>% pull(names(df)[1])
df[, 1] <- sapply(gsub("%", "", percent_boys), as.numeric)


cols <- df %>%
  Filter(f = is.character) %>%
  names()
df <- df %>%
  mutate_at(cols, funs(as.factor)) %>%
  mutate_at(cols, funs(as.integer))


df <- df %>%
  mutate_at("City", funs(as.factor)) %>%
  mutate_at("City", funs(as.integer))
pakistan <- data.frame(df)

devtools::use_data(pakistan, overwrite = TRUE)
