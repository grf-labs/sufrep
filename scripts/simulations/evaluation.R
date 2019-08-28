library(sufrep)
library(grf)
library(caret)
library(glmnet)
library(xgboost)
library(tidyverse)
library(here)

source(here::here("/scripts/simulations/create_data.R"))

time_seed <- function() {
  as.integer((as.numeric(Sys.time()) * 1e+07) %% 1e+07)
}

model <- "regression_forest"

encoding_methods <- c("means") # "mnl")
method <- sample(encoding_methods, 1)
n <- 10000
p <- 20
k <- 2
ngl <- 100 # sample(c(50, 100), 1)
pl <- .9
type <- "nonlinear" # "nonlinear"#"latent"#"hermite"#"latent"#"global"
filename <- paste0(method, "_", time_seed(), ".csv", collapse = "")

num_comp <- c(5, 10, 15)

start <- Sys.time()
for (i in seq(1)) {
  print(i)
  try({
    data <- create_data(n, p, k, ngl = ngl, pl = pl, type = type)
    train_test <- createDataPartition(factor(data$g), p = 0.8)

    train <- list(
      x = data$x[train_test$Resample1, ],
      g = data$g[train_test$Resample1],
      y = data$y[train_test$Resample1]
    )

    test <- list(
      x = data$x[-train_test$Resample1, ],
      g = data$g[-train_test$Resample1],
      y = data$y[-train_test$Resample1]
    )

    if (method %in% c("fisher")) {
      enc_method <- make_encoder(method, X = train$x, G = train$g, Y = train$y)
      x_enc <- enc_method(train$x, train$g)
      x_test_enc <- enc_method(test$x, test$g)
    }
    else if (method %in% c("multi_permutation")) {
      enc_method <- make_encoder(method, X = train$x, G = train$g, num_permutations = p)
      x_enc <- enc_method(train$x, train$g)
      x_test_enc <- enc_method(test$x, test$g)
    }
    else if (method %in% c("low_rank", "sparse_low_rank")) {
      folds <- createFolds(factor(train$g), k = 3, returnTrain = T)
      cv_mses <- c()
      for (ii in 1:length(num_comp)) {
        cmp <- num_comp[ii]
        fold_mses <- c()
        for (iii in 1:3) {
          # make encoder function based on training subset
          enc_method_CV <- make_encoder(method, X = train$x[folds[[iii]], ], G = train$g[folds[[iii]]], num_components = cmp)

          # CV train & test
          x_enc_CV <- enc_method_CV(train$x[folds[[iii]], ], train$g[folds[[iii]]])

          x_test_enc_CV <- enc_method_CV(train$x[-folds[[iii]], ], train$g[-folds[[iii]]])
          if (model == "regression_forest") {
            forest_enc_CV <- regression_forest(x_enc_CV, train$y[folds[[iii]]])
            mse_enc_CV <- mean((predict(forest_enc_CV, x_test_enc_CV)$predictions - train$y[-folds[[iii]]])^2, na.rm = TRUE)
          } else {
            mse_enc_CV <- get_xgboost_mse(
              train = cbind(x_enc_CV,
                Y = train$y[folds[[iii]]]
              ),
              test = cbind(x_test_enc_CV, test$y[-folds[[iii]]])
            )
          }

          fold_mses <- c(fold_mses, mse_enc_CV)
          print(paste0("CV ... ", iii))
        }
        cv_mses <- c(cv_mses, mean(fold_mses))
        print(paste0("Checking - ", ii))
      }
      enc_method <- make_encoder(method, X = train$x, G = train$g, num_components = which.min(cv_mses))
      x_enc <- enc_method(train$x, train$g)
      x_test_enc <- enc_method(test$x, test$g)
    } else if (method %in% c("fisher")) {
      enc_method <- make_encoder(method, X = train$x, G = train$g,Y = train$y)
      x_enc <- enc_method(train$x, train$g)
      x_test_enc <- enc_method(test$x, test$g)
    }
    else {
      enc_method <- make_encoder(method, X = train$x, G = train$g)
      x_enc <- enc_method(train$x, train$g)
      x_test_enc <- enc_method(test$x, test$g)
    }
    enc_onehot <- make_encoder("one_hot", X = train$x, G = train$g)
    x_onehot <- enc_onehot(train$x, train$g)
    x_test_onehot <- enc_onehot(test$x, test$g)


    if (model == "regression_forest") {
      forest_enc <- regression_forest(x_enc, train$y)
      forest_onehot <- regression_forest(x_onehot, train$y)

      mse_enc <- mean((predict(forest_enc, x_test_enc)$predictions - test$y)^2, na.rm = TRUE)
      mse_onehot <- mean((predict(forest_onehot, x_test_onehot)$predictions - test$y)^2, na.rm = TRUE)
    }
    else {
      mse_enc <- get_xgboost_mse(
        train = cbind(x_enc, data.frame(Y = train$y)),
        test = cbind(x_test_enc, data.frame(Y = test$y))
      )
      mse_onehot <- get_xgboost_mse(
        train = cbind(x_onehot, data.frame(Y = train$y)),
        test = cbind(x_test_onehot, data.frame(Y = test$y))
      )
    }
    rsq <- cor(test$y, predict(forest_enc, x_test_enc)$predictions)^2
    config <- cbind(n, p, k, ngl, pl, method, model, type, other = NA, mse_enc, mse_onehot, rsq)
    write.table(config, file = filename, append = T, col.names = F, sep = ",")
  })
  end <- Sys.time()
  print(end - start)
}
