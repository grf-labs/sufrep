library(sufrep)
library(testthat)

test_that("continuous variable names are kept untouched", {
  Xc <- X
  colnames(Xc) <- letters[1:p]
  Xd <- as.data.frame(Xc)
  Xm <- as.matrix(Xc)

  for (method in methods) {
    X_enc_d <- make_encoder(method, X = Xd, G = G, Y = Y, num_permutations = 2)(Xd, G)
    X_enc_m <- make_encoder(method, X = Xm, G = G, Y = Y, num_permutations = 2)(Xm, G)
    expect_equal(colnames(X_enc_d)[1:p], letters[1:p])
    expect_equal(colnames(X_enc_m)[1:p], letters[1:p])
  }
})


test_that("output type matches input X type", {
  Xd <- as.data.frame(X)
  Xm <- as.matrix(X)
  for (method in methods) {
    X_enc_d <- make_encoder(method, X = Xd, G = G, Y = Y, num_permutations = 2)(Xd, G)
    X_enc_m <- make_encoder(method, X = Xm, G = G, Y = Y, num_permutations = 2)(Xm, G)
    expect_equal(class(X_enc_m), "matrix")
    expect_equal(class(X_enc_d), "data.frame")
  }
})


test_that("raises an error if test data has an unseen level", {
  Gnew <- sample(c(levels(G), "newlevelzz"), replace = T, size = dim(X)[1])
  for (method in methods) {
    enc <- make_encoder(method, X = X, G = G, Y = Y, num_permutations = 2)
    expect_error(enc(Xd, Gnew))
  }
})
