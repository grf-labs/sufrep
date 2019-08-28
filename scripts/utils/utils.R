evaluate <- function(train, test,
                     categorical, response,
                     method = "one_hot",
                     model = "regression_forest", num_permutations = NULL,num_components = NULL,Y = NULL) {
  remove_response <- which(colnames(train) %in% c(response))

  if (method %in% c("fisher")) {
    train.X <- train
    Y <- train.X[,response]
  } else if(method %in% c("multi_permutation")){
    train.X <- train[,-remove_response]
    if(is.null(num_permutations)){
      num_permutations <- ncol(train.X)
    }
  } else {
    train.X <- train[, -remove_response]
    response <- NULL
  }

  enc <- make_encoder(X = train.X[,-which(colnames(train.X) %in% c(categorical))], G = as.factor(train.X[,categorical]), method = method, num_permutations=num_permutations,num_components=num_components,Y=Y)
  train <- enc(X=train[,-which(colnames(train) %in% c(categorical))],G=as.factor(train[,categorical]))
  test <- enc(X=test[,-which(colnames(test) %in% c(categorical))],G=as.factor(test[,categorical]))


  if (model == "regression_forest") {
    mse <- get_forest_mse(train, test)
  }
  else {
    mse <- get_xgboost_mse(train, test)
  }
  return(mse)
}


evaluate_method <- function(df, categorical, response, k = 10, model = "regression_forest", stratify = TRUE) {
  output <- c()
  randomized_df <- df[sample(nrow(df)), ]
  methods <- c(
    "one_hot", "multi_permutation", "means", "low_rank",
    "sparse_low_rank", "mnl", "permutation", "difference",
    "deviation", "repeated_effect", "helmert", "fisher",
    "simple_effect"
  )
  total_indices <- 1:nrow(df)

  categ <- c(randomized_df %>% dplyr::mutate_at(categorical, as.character)
    %>% dplyr::mutate_at(categorical, as.numeric)
    %>% dplyr::select(categorical))

  fold_cat <- category_stratify(categ[[1]], num_folds = k)

  for (j in 1:k) {
    testIndexes <- fold_cat[[j]]
    testData <- randomized_df[testIndexes, ]
    trainData <- randomized_df[-testIndexes, ]
    mses <- c()
    for (q in 1:13) {
      print(paste0("STARTING ",methods[q]))
      if (methods[q] %in% c("low_rank", "sparse_low_rank")) {
        cv_vals <- c(5, 10, 15)
        folds <- 3
        cv_mses <- c()
        randomized_df2 <- trainData[sample(nrow(trainData)), ]
        rownames(randomized_df2) <- NULL
        fold_cat2 <- category_stratify(randomized_df2[, categorical], num_folds = folds)
        for (ii in 1:length(cv_vals)) {
          print("Running CV...")
          cv_mse <- c()
          for (jj in 1:folds) {
            testIndexes2 <- fold_cat2[[jj]]
            testData2 <- randomized_df2[testIndexes2, ]
            trainData2 <- randomized_df2[-testIndexes2, ]

            train.X2 <- trainData2[, -which(colnames(trainData2) %in% c(categorical,response))]
            enc <- make_encoder(method = methods[q],X = train.X2, G = as.factor(trainData2[,categorical]), num_components = cv_vals[ii])

            trainData2 <- enc(X=trainData2,G=as.factor(trainData2[,categorical]))
            testData2 <- enc(X=testData2,G=as.factor(testData2[,categorical]))
            if (model == "regression_forest") {
              cv_mse <- c(mse, get_forest_mse(trainData2, testData2))
            }
            else {
              cv_mse <- c(mse, get_xgboost_mse(trainData2, testData2))
            }
          }
          cv_mses <- c(cv_mses, mean(cv_mse))
        }
        mx <- which(cv_mses == min(cv_mses))[1]
        num_components <- cv_vals[mx]
        mse <- evaluate(
          method = methods[q], train = trainData, test = testData,
          categorical = categorical, response = response,
          model = model, Y = NULL, num_components = num_components
        )
      }
      else {
        if(methods[q] %in% c("fisher")){
          mse <- evaluate(
            method = methods[q], train = trainData, test = testData,
            categorical = categorical, response = response,
            model = model, Y = response
          )
        } else{
          mse <- evaluate(
            method = methods[q], train = trainData, test = testData,
            categorical = categorical, response = response,
            model = model, Y = NULL
          )
        }

      }

      print(mse)
      mses <- c(mses, mse)
      print(methods[q])
    }

    new_row <- c(nrow(df), model, j, mses)
    output <- rbind(output, new_row)
    print(paste("Done with -- ", j, sep = ""))
  }
  colnames(output) <- c(
    "file", "model", "fold", "one_hot", "multi_perm",
    "add_means", "add_svd", "add_spca", "add_pax_weight", "perm",
    "difference", "deviation", "repeated", "helmert", "fisher", "simple_effect"
  )
  saveRDS(output, file = paste("Evaluation_", k, i, "_", time_seed(), ".rds", sep = ""))
  return(data.frame(output))
}


get_noise_scale <- function(noiseless, snr) {
  sqrt(var(noiseless) / snr)
}


time_seed <- function() {
  as.integer((as.numeric(Sys.time()) * 1e+07) %% 1e+07)
}


category_stratify <- function(categories, num_folds = 4) {
  indices <- 1:length(categories)
  size_per_fold <- as.integer(length(categories) / num_folds)
  folds <- list()
  unique_categories <- unique(categories)
  tmpcat <- categories
  for (j in 1:num_folds) {
    vals <- c()
    for (i in 1:length(unique_categories)) {
      available <- which(tmpcat == unique_categories[i])
      total_available <- which(categories == unique_categories[i])
      num_select <- as.integer(length(total_available) / num_folds)
      if (length(available) <= num_select) {
        added_indices <- available
      }
      else {
        added_indices <- sample(available, num_select, replace = FALSE)
      }
      vals <- c(vals, added_indices)
    }
    folds[[j]] <- vals
    tmpcat[vals] <- rep(-99999, length(vals))
    indices[vals] <- rep(-99999, length(vals))
  }
  return(folds)
}


get_regression_forest_prediction <- function(train_data, test_data, ...) {
  train_X <- train_data %>% dplyr::select(-Y)
  forest <- grf::regression_forest(X = train_X, Y = train_data$Y, num.threads = 1, ...)
  yhat <- predict(forest,
    newdata = test_data %>% dplyr::select(-Y),
    estimate.variance = FALSE
  )$predictions
  return(yhat)
}


get_xgboost_mse <- function(train, test, ...) {
  train_Y <- train %>% dplyr::pull(Y)
  train_X <- as.matrix(train %>% dplyr::select(-Y))
  test_Y <- test %>% dplyr::pull(Y)
  test_X <- as.matrix(test %>% dplyr::select(-Y))

  xgb_grid_1 <- expand.grid(
    nrounds = c(20, 50, 100),
    max_depth = c(3, 6, 9, 12),
    colsample_bytree = c(0.5, 0.7, 0.9),
    eta = c(0.1, 0.3, 0.5),
    gamma = c(0, 0.1),
    min_child_weight = c(1, 5, 10),
    subsample = c(0.5, 0.75, 1.0)
  )

  xgb_trcontrol_1 <- trainControl(
    method = "cv",
    number = 3,
    allowParallel = TRUE
  )

  xgb_train_1 <- train(
    x = train_X,
    y = train_Y,
    trControl = xgb_trcontrol_1,
    tuneGrid = xgb_grid_1,
    method = "xgbTree"
  )


  predictions <- predict(xgb_train_1, test_X)
  mse <- mean((test_Y - predictions)^2)
  return(mse)
}


get_forest_mse <- function(train_data, test_data, ...) {
  yhat <- tryCatch({
    get_regression_forest_prediction(train_data = train_data, test_data = test_data, ...)
  },
  error = function(e) {
    return(rbind(train_data, test_data))
  }
  )
  mse <- mean((test_data$Y - yhat)^2)
  return(mse)
}


fix_factors <- function(x) {
  return(as.numeric(as.character(x)))
}
