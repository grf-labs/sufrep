
library(tidyverse)
library(devtools)

df <- readr::read_csv("ames.csv")
cols <- df %>%
  Filter(f = is.character) %>%
  names()
df[which(is.na(df$Alley)), "Alley"] <- 0
df[which(is.na(df$LotFrontage)), "LotFrontage"] <- 0
df <- df %>%
  mutate_at(cols, funs(as.factor)) %>%
  mutate_at(cols, funs(as.integer))

df <- df %>%
  mutate_at("Neighborhood", funs(as.factor)) %>%
  dplyr::select(-PoolQC) %>%
  dplyr::select(-GarageQual) %>%
  dplyr::select(-GarageYrBlt)
df <- df[-which(df$Neighborhood == 2), ]
df <- data.frame(df)
df[is.na(df)] <- 0
ames <- df

devtools::use_data(ames)
