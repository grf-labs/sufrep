
## Running Observational Experiments

Below is approximately equivalent to what is in Ames.R, Pakistan.R, and Kingcounty.R. This runs each encoding method on the Ames Housing dataset.

```R
library(tidyverse)
library(xgboost)
library(glmnet)
library(sufrep)

source("dgp.R")
source("utils.R")

model <- "regression_forest"
folds <- 4
seeds <- 1
y_var <- "SalePrice"
target_categorical <- "Neighborhood"

dataset <- sufrep::ames
dataset$Y <- dataset[, y_var]
dataset <- dataset[, -which(colnames(dataset) %in% c(y_var))]

eval.df <- evaluate_method(df = dataset, response = "Y", 
                           categorical = target_categorical, k = folds, 
                           seeds = seeds, model = model)
```
