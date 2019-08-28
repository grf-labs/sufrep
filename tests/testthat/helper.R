# Creates dummy data.
make_data <- function(n = 200, p = 4, m = 6) {
  cats <- apply(expand.grid(letters, letters), 1, function(s) paste0(s, collapse = ""))[1:m]
  G <- factor(sample(cats, replace = T, size = n))
  G_num <- as.integer(G)
  Y <- G_num + rnorm(n)
  X <- apply(matrix(runif(n * p), n, p), 2, function(x) x + G_num)
  list(X = X, G = G, Y = Y)
}

# Checks that all the encodings for one category are the same,
#  regardless of the method used.
has_consistent_encoding <- function(X, G, X_enc) {
  G_num <- as.integer(G)
  p <- dim(X)[2]
  p_enc <- dim(X_enc)[2]
  is_consistent <- sapply(unique(G_num), function(g) {
    idx <- G_num == g
    encoded <- X_enc[idx, (p + 1):p_enc, drop = F]
    apply(encoded, 2, function(x) {
      all(x == x[1])
    })
  })
  all(is_consistent)
}

methods <- c(
  "one_hot", "helmert", "deviation",
  "repeated_effect", "difference",
  "simple_effect", "fisher", "means",
  "low_rank", "sparse_low_rank",
  "permutation", "multi_permutation", "mnl"
)

classical_methods <- c(
  "one_hot", "helmert", "deviation",
  "repeated_effect", "difference", "simple_effect"
)

n <- 200
p <- 5
data <- make_data(n = n, p = p)
X <- data$X
G <- data$G
Y <- data$Y
