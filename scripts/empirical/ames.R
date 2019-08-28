
rm(list = ls())

library(tidyverse)
library(grf)
library(caret)
library(glmnet)
library(xgboost)
library(sufrep)
library(sparsepca)
library(here)

source(here::here("scripts/simulations/dgp.R"))
source(here::here("scripts/utils/utils.R"))

model <- "regression_forest"
folds <- 4
y_var <- "SalePrice"
target_categorical <- "Neighborhood"

dataset <- sufrep::ames
dataset$Y <- dataset[, y_var]
dataset <- dataset[, -which(colnames(dataset) %in% c(y_var))]


start_time <- Sys.time()
eval.df <- evaluate_method(df = dataset, response = "Y", categorical = target_categorical, k = folds, model = model)
filename <- paste(model, "_AMES_", time_seed(), ".rds", sep = "")
saveRDS(eval.df, file = filename)
end_time <- Sys.time()
print(end_time - start_time)
