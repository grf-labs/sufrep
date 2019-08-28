## Running Simulation Experiments

Below is a basic example of how to evaluate one iteration of the simulation data sets for a given encoding method and model.

```R
library(tidyverse)
library(xgboost)
library(glmnet)
library(sufrep)

source("dgp.R")
source("utils.R")

model <- "regression_forest"
sim_data <- make_dataset(
  setup = "linear", n = 5000, p = 20,
  rho = 0.25, size_l = 500, size_a = 10,
  lambda = 0.5, snr = 0.5, comp_percent = 0.9,
  seed = time_seed(), num_interactions = 1,
  test = TRUE
)

train <- data.frame(sim_data$TRAIN %>% dplyr::mutate_at("A", fix_factors))
test <- data.frame(sim_data$TEST %>% dplyr::mutate_at("A", fix_factors))
mse <- evaluate(method = "one_hot", train = train, test = test, categorical = "A", response = "Y", model = model)
```

In evaluation.R, we have code that runs each encoding method for a set of parameters and seed.
