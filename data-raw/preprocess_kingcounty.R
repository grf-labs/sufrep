
library(tidyverse)
library(devtools)

df <- readr::read_csv("kc_house_data.csv")
df <- df %>%
  na.omit() %>%
  dplyr::select(-date) %>%
  dplyr::select(-id)
df <- df %>%
  mutate_at("zipcode", funs(as.factor)) %>%
  mutate_at("zipcode", funs(as.integer))

kingcounty <- data.frame(df)
devtools::use_data(kingcounty)
