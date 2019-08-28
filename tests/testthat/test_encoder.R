library(sufrep)
library(testthat)


test_that("all encoders have consistent encoding", {
  for (method in methods) {
    enc <- make_encoder(method, X, G, Y = Y)
    X_enc <- enc(X, G)
    expect_true(has_consistent_encoding(X, G, X_enc))
  }
})


test_that("classical encodings have same R-squared", {
  rsquared <- rep(0, length(classical_methods))
  for (i in seq_along(classical_methods)) {
    method <- classical_methods[i]
    enc <- make_encoder(method, X, G)
    X_enc <- enc(X, G)
    ols <- lm(Y ~ X_enc)
    rsquared[i] <- summary(ols)$r.squared
    expect_equal(rsquared[1], rsquared[i], tol = 1e-5)
  }
})


test_that("means encoding correctly takes means", {
  enc <- make_encoder(method = "means", X, G)
  X_enc <- enc(X, G)
  G_levels <- unique(G)
  for (g in seq_along(G_levels)) {
    idx <- G == g
    G_mean1 <- apply(X_enc[idx, (p + 1):dim(X_enc)[2], drop = F], 2, mean)
    G_mean2 <- apply(X[idx, ], 2, mean)
    expect_equal(G_mean1, G_mean2, tol = 1e-8)
  }
})


test_that("low rank encodings have correct number of columns", {
  for (method in c("low_rank", "sparse_low_rank")) {
    for (q in seq(p)) {
      X_enc <- make_encoder(method = method, X, G, num_components = q)(X, G)
      expect_equal(dim(X_enc)[2], p + q)
    }
  }
})


test_that("low rank encodings break if number of components is too large", {
  for (method in c("low_rank", "sparse_low_rank")) {
    expect_error(make_encoder(method = method, X, G, num_components = p + 1))
  }
})


test_that("multi_permutation method has correct number of columns", {
  for (q in seq(p)) {
    X_enc <- make_encoder(method = "multi_permutation", X, G, num_permutations = q)(X, G)
    expect_equal(dim(X_enc)[2], p + q)
  }
})
