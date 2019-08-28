library(testthat)
library(sufrep)

test_that("create_data: function respects parameters", {
  n <- 400
  p <- 11
  k <- 7
  ngl <- 5
  data <- create_data(n = n, p = p, k = k, type = "global", pl = 0.9, ngl = ngl)
  expect_equal(length(unique(data$l)), k)
  expect_equal(dim(data$x), c(n, p))
  expect_equal(length(levels(data$g)), k * ngl)
})


test_that("create_data: covariates have different means depending on latent group", {
  n <- 4000
  p <- 4
  k <- 3
  ngl <- 5
  data <- create_data(n = n, p = p, k = k, type = "global", pl = 0.9, ngl = ngl)
  x_mean_pred <- aggregate(data$x, list(data$l), mean)[, 2:(p + 1)]
  x_mean_true <- data$x_mean
  expect_true(all(abs(x_mean_true - x_mean_pred) < 0.1))
})


test_that("create_data: covariates have different means depending on observable group", {
  n <- 20000
  p <- 4
  k <- 3
  ngl <- 2
  data <- create_data(n = n, p = p, k = k, type = "global", pl = 1, ngl = ngl)
  x_mean_pred <- aggregate(data$x, list(data$g), mean)[, 2:(p + 1)]
  x_mean_true <- data$x_mean
  for (l in seq(k)) {
    l_cat <- data$map[l, ] # categories associated with this latent group
    l_xmean <- x_mean_true[l, ] # average x for this latent group
    diff <- apply(x_mean_pred[l_cat, ], 1, function(x) x - l_xmean)
    expect_true(all(abs(diff) < 0.1))
  }
})


test_that("create_data: intercept is different depending on observable group", {
  n <- 20
  p <- 4
  k <- 3
  ngl <- 2
  data <- create_data(n = n, p = p, k = k, type = "global", pl = 1, ngl = ngl)
  alpha_base <- unique(data$alpha)
  # There are as many intercepts as there are latent groups
  expect_equal(k, length(unique(data$alpha)))
  for (l in seq(k)) {
    # Intercepts for observations in one group are the same
    intercepts <- data$alpha[data$l == l]
    expect_true(all(intercepts == intercepts[1]))
  }
})


test_that("create_data: variance of (intercept, x*b, noise) is roughly equal", {
  n <- 20000
  p <- 4
  k <- 3
  ngl <- 2
  data <- create_data(n = n, p = p, k = k, type = "global", pl = 1, ngl = ngl)
  x <- data$x
  expect_true(abs(var(x %*% t(b)) - 1) < 0.2)
  expect_true(abs(var(x %*% t(b)) - 1) < 0.2)
})
