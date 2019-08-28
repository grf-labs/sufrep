one_hot_encode <- function(num_categ) {
  CM <- diag(num_categ)[, 1:(num_categ - 1)]
  return(CM)
}

helmert_encode <- function(num_categ) {
  CM <- stats::contr.helmert(num_categ)
  return(CM)
}

deviation_encode <- function(num_categ) {
  CM <- stats::contr.sum(num_categ)
  return(CM)
}

repeated_effect_encode <- function(num_categ) {
  TH <- matrix(0, nrow = num_categ, ncol = num_categ)
  TH <- matrix((num_categ - col(TH)) / num_categ, nrow = num_categ, ncol = num_categ)
  TH[lower.tri(TH)] <- 0
  BH <- matrix(-col(TH) / num_categ, nrow = num_categ, ncol = num_categ)
  BH[upper.tri(BH)] <- 0
  diag(BH) <- 0
  CM <- TH + BH
  CM <- CM[, 1:(num_categ - 1)]
  return(CM)
}

difference_encode <- function(num_categ) {
  CM <- matrix(0, nrow = num_categ, ncol = num_categ)
  CM <- matrix(-1 / (col(CM) + 1), nrow = num_categ, ncol = num_categ)
  CM[lower.tri(CM)] <- 0
  CM <- CM[, 1:(num_categ - 1)]
  CM[row(CM) == (col(CM) + 1)] <- -apply(CM, 2, sum)
  return(CM)
}

simple_effect_encode <- function(num_categ) {
  CM <- matrix(-1 / num_categ, nrow = num_categ, ncol = num_categ)
  CM <- CM + diag(num_categ)
  CM <- CM[, 1:(num_categ - 1)]
  return(CM)
}

fisher_encode <- function(G, Y) {
  CM <- aggregate(Y, list(G), mean)
  colnames(CM) <- c("label", "X1")
  CM$X1 <- rank(CM$X1)
  ordering <- data.frame(unique(G))
  colnames(ordering) <- "ORD"
  CM <- CM[order(ordering$ORD), ]
  CM <- as.matrix(CM$X1)
  return(CM)
}


means_encode <- function(X, G) {
  p <- dim(X)[2]
  CM <- as.matrix(aggregate(X, list(G), mean)[, 2:(p + 1)])
  colnames(CM) <- NULL
  return(CM)
}


low_rank_encode <- function(X, G, num_components) {
  if (num_components > dim(X)[2]) {
    stop("Argument num_components cannot be larger than number of X columns.")
  }
  CM <- means_encode(X, G)
  decomp <- svd(CM)
  CM <- as.matrix(decomp$u[, 1:num_components])
  return(CM)
}


sparse_low_rank_encode <- function(X, G, num_components) {
  if (num_components > dim(X)[2]) {
    stop("Argument num_components cannot be larger than number of X columns.")
  }
  CM <- means_encode(X, G)
  decomp <- sparsepca::spca(CM, verbose = FALSE)
  U <- decomp$loadings[, 1:num_components]
  CM <- CM %*% U
  return(CM)
}


permutation_encode <- function(num_categ, num_perms) {
  if (is.null(num_perms)) {
    stop("When 'method' is 'multi_permutation', argument 'num_permutations' must not be null.")
  }
  CM <- replicate(num_perms, sample(num_categ, size = num_categ, replace = FALSE))
  return(CM)
}


mnl_encode <- function(X, G) {
  X <- as.matrix(X)
  fit <- glmnet::glmnet(x = X, y = G, family = "multinomial")
  coefs <- coef(fit, s = min(fit$lambda, na.rm = TRUE))
  coef_mat <- as.data.frame(lapply(coefs, as.matrix))
  CM <- t(as.matrix(coef_mat))
  rownames(CM) <- NULL
  colnames(CM) <- NULL
  return(CM)
}


validate_X <- function(X) {
  if (length(dim(X)) != 2) {
    stop("Argument X must be two-dimensional.")
  }
  non_numeric_cols <- rep(F, dim(X)[2])
  for (i in seq_along(dim(X)[2])) {
    non_numeric_cols[i] <- !is.numeric(X[, i])
  }
  if (any(non_numeric_cols)) {
    stop(paste0(
      "Argument X contains columns that are not numeric. ",
      "They are numbered ", which(non_numeric_cols)
    ))
  }
}


validate_G <- function(G) {
  if (!is.factor(G)) {
    stop("Argument G must be of factor type.")
  }
}


validate_Y <- function(method, Y) {
  if ((method == "fisher") && is.null(Y)) {
    stop("Method 'fisher' requires non-null Y.")
  }
}


validate_levels <- function(G, input_levels) {
  new_levels <- setdiff(levels(G), input_levels)
  if (length(new_levels) > 0) {
    stop(paste0("Training data did not contain levels: ", new_levels))
  }
}


# Compute representations for categorical (factor) variables
#
#' @param method The encoding method.
#'               Must be one of: "one_hot", "helmert", "deviation",
#'               "repeated_effect", "difference", "simple_effect",
#'               "fisher", "means", "low_rank", "sparse_low_rank",
#'               "permutation", "multi_permutation", "mnl".
#'               See REFERENCE.md for an explanation of each method.
#' @param X A data.frame or matrix containing only numeric columns.
#' @param G A single vector of factor type containing the categories.
#' @param prefix A prefix to be prepended to encoding column names.
#' @param Y A single vector of numerical type containing the outcome variable.
#'          Used only in method "fisher", ignored otherwise.
#' @param num_components If method is sparse_low_rank, this is the number
#'                       of sparse principal components. If method is low_rank,
#'                       then this corresponds to the number of singular vectors.
#'                       For all other methods, this argument is ignored.
#' @param num_permutations Number of columns to be added by the 'multi_permutation'
#'                         method. Ignored by other methods.
#' @param num_folds Number of cross-validation folds used by mnl method. Ignored by
#'                  other methods.
#' @examples
#' \dontrun{
#' # Fake data
#' n <- 100
#' p <- 10
#' G <- factor(sample(c("a", "b", "c"), replace = T, size = n))
#' X <- apply(matrix(runif(n * p), n, p))
#'
#' # Create the encoding matrix with the means method
#' enc <- make_encoder("means", X = X, G = G)
#'
#' # Compute the actual encoded matrix
#' X_enc <- enc(X, G)
#' }
#'
#' @return A matrix or data.frame that concatenates the original X input columns with
#'         the encoding columns.
#'
#' @references Jonathan Johannemann, Vitor Hadad, Susan Athey, and Stefan Wager.
#'             "Sufficient Representations of Categorical Variables". 2019.
make_encoder <- function(method, X, G,
                         prefix = "ENC",
                         Y = NULL,
                         num_components = dim(X)[2],
                         num_permutations = 1,
                         num_folds = 3) {

  # Type validation
  validate_X(X)
  validate_G(G)
  validate_Y(method, Y)
  input_levels <- levels(G)

  # Compute encoding
  num_categ <- length(levels(G))
  CM <- switch(method,
    one_hot = one_hot_encode(num_categ),
    helmert = helmert_encode(num_categ),
    deviation = deviation_encode(num_categ),
    repeated_effect = repeated_effect_encode(num_categ),
    difference = difference_encode(num_categ),
    simple_effect = simple_effect_encode(num_categ),
    fisher = fisher_encode(G, Y),
    means = means_encode(X, G),
    low_rank = low_rank_encode(X, G, num_components),
    sparse_low_rank = sparse_low_rank_encode(X, G, num_components),
    permutation = permutation_encode(num_categ, 1),
    multi_permutation = permutation_encode(num_categ, num_permutations),
    mnl = mnl_encode(X, G)
  )

  # Create encoding function
  encoding_fun <- function(X, G) {
    validate_X(X)
    validate_G(G)
    validate_levels(G, input_levels)

    # Augment original matrix
    X_aug <- cbind(X, CM[as.integer(G), ])

    # Maintain X type
    if (is.data.frame(X)) {
      X_aug <- as.data.frame(X_aug)
    }

    # Maintain row and column names, if appropriate
    rownames(X_aug) <- rownames(X)
    if (!is.null(colnames(X))) {
      colnames(X_aug) <- c(colnames(X), paste(prefix, 1:dim(CM)[2], sep = ""))
    }

    return(X_aug)
  }

  return(encoding_fun)
}
