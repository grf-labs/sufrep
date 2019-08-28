


make_X <- function(n, p, rho) {
  sigma <- toeplitz(rho^seq(p + 1))
  X <- MASS::mvrnorm(n = n, mu = rep(0, p + 1), Sigma = sigma) %>% magrittr::set_colnames(c("L", paste("X", 1:p, sep = "")))
  return(X)
}

make_AL <- function(X, size_l, size_a, comp_percent, random_exponential = FALSE) {
  X[, "L"] <- dplyr::ntile(X[, "L"], size_l)
  p <- ncol(X[, grep("X", colnames(X))])
  mean_shift <- matrix(0, ncol = p, nrow = size_l)
  for (i in 1:size_l) {
    shifts <- sample(c(-1, 1), replace = TRUE, size = 3)
    indices <- sample(c(1:p), replace = FALSE, size = 3)
    mean_shift[i, indices] <- shifts
  }
  X[, grep("X", colnames(X))] <- X[, grep("X", colnames(X))] + mean_shift[X[, "L"], ]
  A <- matrix(0, nrow = nrow(X), ncol = 1)
  percs_l <- comp_percent / size_l
  percs_a <- (1 - comp_percent) / (size_a - size_l)
  am <- matrix(sample(1:size_a, size = size_a, replace = FALSE), ncol = size_l)
  for (i in 1:size_l) {
    probs <- rep(percs_a, size_a)
    probs[am[, i]] <- percs_l
    group_size <- nrow(X[which(X[, "L"] == i), ])
    A[which(X[, "L"] == i)] <- sample(size_a, size = group_size, prob = probs, replace = TRUE)
  }
  X <- cbind(X, A)
  colnames(X)[ncol(X)] <- "A"
  return(X)
}

make_linear <- function(args) {
  beta <- matrix(c(sample(c(-1, 1), size = ceiling(0.6 * args$p), replace = TRUE), rep(0, floor(0.4 * args$p))), ncol = 1)
  beta <- matrix(sample(beta, size = args$p, replace = FALSE), ncol = 1)
  mu <- args$X[, grep("X", colnames(args$X))] %*% beta
  effect <- make_effect(args$X, mu, args$size_l, args$lambda)

  mu_star <- mu + effect[, "L_i"]
  Y <- mu_star + rnorm(n = nrow(mu_star), mean = 0, sd = sqrt(var(mu_star) / args$snr))
  return(Y)
}

make_interactions <- function(args) {
  X <- args$X[, grepl("X", colnames(args$X))]
  p <- ncol(X[, grepl("X", colnames(X))])
  covs <- floor(sqrt(p))
  j_k <- base::sample(p, size = (2 * covs + 1), replace = TRUE)
  beta <- base::sample(c(-10:-1, 1:10), size = covs, replace = TRUE)
  Y_s <- matrix(0, nrow = nrow(X))
  for (i in 1:covs) {
    Y_s <- Y_s + beta[i] * X[, j_k[2 * i]] * X[, j_k[2 * i + 1]]
  }
  effect <- make_effect(args$X, mu = Y_s, args$size_l, args$lambda, interaction = TRUE, num_interactions = args$num_interactions)
  Y_e <- effect[, "L_i"]
  Y_noiseless <- args$lambda * Y_e + (1 - args$lambda) * Y_s
  Y <- Y_noiseless + rnorm(n = nrow(Y_noiseless), mean = 0, sd = sqrt(var(Y_noiseless) / args$snr))
  return(Y)
}



make_effect <- function(X, mu, size_l, lambda, interactions = FALSE, num_interactions = 1) {
  p <- ncol(X[, grepl("X", colnames(X))])
  var_mu <- var(mu)
  effects <- seq(from = -10 * floor(size_l / 2), to = 10 * ceiling(size_l / 2), by = 1)

  cluster_effects <- base::sample(effects[effects != 0], size = size_l, replace = FALSE)
  latent_effects <- matrix(cluster_effects[X[, "L"]], ncol = 1)
  if (interactions == TRUE) {
    j_k <- base::sample(p, size = num_interactions, replace = TRUE)
    beta <- base::sample(c(-10:-1, 1:10), size = num_interactions, replace = TRUE)
    for (i in 1:num_interactions) {
      if (i == 1) {
        latent_effects <- beta[i] * latent_effects * X[, j_k[i]]
      } else {
        latent_effects <- latent_effects + beta[i] * latent_effects * X[, j_k[i]]
      }
    }
  }
  latent_effects <- (latent_effects - mean(latent_effects)) / sd(latent_effects) * sqrt(var_mu[1, 1] * lambda / (1 - lambda))
  X <- cbind(X, latent_effects)
  colnames(X)[ncol(X)] <- "L_i"
  return(X)
}

make_dataset <- function(setup, n, p, rho, size_l, size_a, comp_percent, lambda, snr, num_interactions = 1, seed = NULL, test = TRUE) {
  if (!is.null(seed)) {
    set.seed(seed)
  }

  n_val <- n
  if (test == TRUE) {
    n_val <- n * 2
  }

  X <- make_X(n = n_val, p = p, rho = rho)
  X <- make_AL(X, size_l = size_l, size_a = size_a, comp_percent = comp_percent)

  args <- list(X = X, size_l = size_l, lambda = lambda, snr = snr, p = p, n = n_val, num_interactions = num_interactions)

  Y <- switch(setup, linear = make_linear(args), interactions = make_interactions(args), step = make_step(args), cluster = make_cluster(args))

  x_cols <- colnames(X)[grepl("X", colnames(X))]

  train <- cbind(Y[1:n], X[1:n, "A"], X[1:n, x_cols])
  train <- train %>% magrittr::set_colnames(c("Y", "A", x_cols))

  if (test == TRUE) {
    test <- cbind(Y[(n + 1):(2 * n)], X[(n + 1):(2 * n), "A"], X[(n + 1):(2 * n), x_cols])
    test <- test %>% magrittr::set_colnames(c("Y", "A", x_cols))
    dataset <- list(TRAIN = as.tibble(train) %>% dplyr::mutate_at("A", funs(as.factor)), TEST = as.tibble(test) %>% dplyr::mutate_at(
      "A",
      funs(as.factor)
    ))
  } else {
    dataset <- list(TRAIN = as.tibble(train) %>% dplyr::mutate_at("A", funs(as.factor)))
  }

  return(dataset)
}
