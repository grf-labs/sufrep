sample_latent_group <- function(n, k) {
  latent <- sample.int(k, size = n, replace = T)
  latent
}

# alpha[l] ~ Laplace
sample_alpha <- function(latent) {
  k <- length(unique(latent))
  sgn <- sample(c(-1, 1), replace = T, size = k)
  base_alpha <- sgn * rexp(sqrt(2), n = k)
  alpha <- base_alpha[latent]
  alpha
}


sample_x_mean <- function(k, p, shifts = 3) {
  mu <- matrix(0, nrow = k, ncol = p)
  for (i in 1:k) {
    shift <- sample(c(-1, 1), size = shifts, replace = TRUE)
    indices <- sample(p, size = shifts, replace = FALSE)
    mu[i, indices] <- shift
  }
  mu
}


sample_x <- function(latent, x_mean, rho = 0.5) {
  p <- dim(x_mean)[2]
  sigma <- toeplitz(rho^seq(0, (p - 1)))
  x <- t(apply(x_mean[latent, ], 1, function(mul) MASS::mvrnorm(mu = mul, Sigma = sigma)))
  colnames(x) <- paste("X", 1:p, sep = "")
  x
}


sample_beta_latent <- function(k, p) {
  if (p < 3) stop("Function sample_beta_latent required p > 2.")
  base_vec <- rep(c(1, 0, -1), p)
  beta <- replicate(k, sample(base_vec, size = p, replace = F))
  beta <- t(apply(beta, 2, function(x) x / sqrt(sum(x^2))))
  beta
}


sample_beta_global <- function(p) {
  sample_beta_latent(k = 1, p = p)
}

sample_beta_hermite <- function(k, p) {
  matrix(sample(c(1, -1) / sqrt(p), replace = T, size = k * p), k, p)
}


get_latent_to_observed_map <- function(k, ngl) {
  num_cats <- k * ngl
  all_cats <- seq(num_cats)
  latent_to_obs_cat <- matrix(sample(all_cats, num_cats), k, ngl) # scrambling the map
  latent_to_obs_cat
}


sample_observed_category <- function(latent, map, pl) {
  if (pl <= 0.5) stop("Argument pl must be > 0.5.")
  n <- length(latent)
  all_cats <- c(map)
  own_latent <- rbinom(prob = pl, n = n, size = 1)
  obs_cat <- rep(0, n)
  for (i in 1:n) {
    own_cats <- map[latent[i], ]
    if (own_latent[i]) {
      obs_cat[i] <- sample(own_cats, size = 1)
    } else {
      obs_cat[i] <- sample(setdiff(all_cats, own_cats), size = 1)
    }
  }
  obs_cat
}


linear_global_response <- function(alpha, x, beta) {
  if (dim(beta)[1] > 1) stop("Argument beta must have only one row.")
  n <- length(alpha)
  y <- alpha + x %*% t(beta) + rnorm(n)
  c(y)
}


linear_latent_response <- function(latent, alpha, x, betas) {
  if (dim(betas)[1] == 1) stop("Argument beta must have multiple rows.")
  n <- length(alpha)
  y <- alpha + apply(x * betas[latent, ], 1, sum) + rnorm(n)
  y
}

nonlinear_latent_group <- function(alpha, x, betas) {
  # if (dim(betas)[1] == 1) stop("Argument beta must have multiple rows.")
  n <- dim(x)[1]
  p <- dim(x)[2]
  bp <- length(betas)
  V <- matrix(0, ncol = bp, nrow = p)
  for (i in 1:bp) {
    ind <- sample(p, size = 2, replace = FALSE)
    V[ind, i] <- 1
  }
  xb <- x %*% V %*% matrix(betas, nrow = bp, ncol = 1)
  xb <- apply(xb, 2, function(x) x / sqrt(sum(x^2))) # this is wrong..., doesn't matter, we don't want it
  alpha + x %*% V %*% matrix(betas, nrow = bp, ncol = 1) + rnorm(n)
}

nonlinear_latent_response <- function(latent, alpha, x, betas) {
  num_latents <- length(unique(latent))
  Y <- matrix(0, nrow = dim(x)[1], ncol = 1)
  for (i in 1:num_latents) {
    ind <- which(latent == i)
    Y[ind, ] <- nonlinear_latent_group(alpha[i], x[ind, ], betas[i, ])
  }
  Y
}

dyadic_response <- function(latent, alpha, x, betas) {
  n <- dim(x)[1]
  W <- stats::rbinom(n, size = 1, prob = 0.5)
  B.dyad <- dyadic.basis(x, W, as.integer(n / 2))
  p2 <- dim(B.dyad)[2]
  Y <- matrix(0, nrow = n, ncol = 1)
  for (i in 1:length(unique(latent))) {
    ind <- which(latent == i)
    beta <- matrix(rnorm(p2), ncol = 1, nrow = p2)
    xb <- B.dyad[ind, ] %*% beta
    xb <- apply(xb, 2, function(x) x / sqrt(sum(x^2)))
    xb <- sqrt(length(ind)) * xb
    Y[ind] <- alpha[i] + xb + rnorm(length(ind))
  }
  Y
}

dyadic.basis <- function(A, YA, min.n = 200) {
  indicator <- matrix(1, nrow(A), 1)

  # stop splitting
  if (nrow(A) <= min.n / 2) {
    return(indicator)
  } else if (nrow(A) <= min.n) {
    # just one more greedy split
    improvement <- sapply(1:ncol(A), function(j) {
      med.j <- median(A[, j])
      if (max(A[, j]) == med.j) {
        return(0)
      }
      (mean(YA[A[, j] <= med.j]) - mean(YA[A[, j] > med.j]))^2
    })
    if (max(improvement) == 0) {
      return(indicator)
    }
    # do the greedy split
    j.max <- which.max(improvement)
    med.j.max <- median(A[, j.max])
    is.left <- (A[, j.max] <= med.j.max)
    return(cbind(indicator, as.numeric(is.left), as.numeric(!is.left)))
  }
  sub.basis <- lapply(1:ncol(A), function(j) {
    med.j <- median(A[, j])
    if (max(A[, j]) == med.j) {
      return(rep(1, nrow(A)))
    }
    is.left <- (A[, j] <= med.j)
    left <- dyadic.basis(A[is.left, ], YA[is.left], min.n)
    right <- dyadic.basis(A[!is.left, ], YA[is.left], min.n)
    all <- matrix(0, nrow(A), ncol(left) + ncol(right))
    all[is.left, 1:ncol(left)] <- left
    all[!is.left, ncol(left) + 1:ncol(right)] <- right
    all
  })
  cbind(indicator, Reduce(cbind, sub.basis))
}

generate_basis <- function(X, order = 2) {
  H <- lapply(1:ncol(X), function(j) {
    sapply(1:order, function(k) EQL::hermite(X[, j], k, prob = TRUE) / sqrt(factorial(k)))
  })
  polys <- lapply(1:order, function(r) {
    partitions <- combn(
      r + ncol(X) - 1, ncol(X) - 1,
      function(vec) c(vec, r + ncol(X)) - c(0, vec) - 1
    )
    elems <- sapply(1:ncol(partitions), function(iter) {
      part <- partitions[, iter]
      idx <- which(part > 0)
      elem <- H[[idx[1]]][, part[idx[1]]]
      if (length(idx) > 1) {
        for (id in idx[-1]) {
          elem <- elem * H[[id]][, part[id]]
        }
      }
      elem
    })
    scale(elems) / sqrt(ncol(elems)) / r
  })
  Reduce(cbind, polys)
}




# n: number of observations
# p: number of continuous covariates
# k: number of latent groups
# ngl: number of observable cats per latent group
# pl: probability that observable category belongs to their 'own' latent group
#' @export
create_data <- function(n, p, k, ngl, pl, type = "global") {
  if (p < k) stop("Our methods don't work if p < k.")

  # Draw the latent group L[i] and observable category G[i]
  latent <- sample_latent_group(n, k)
  map <- get_latent_to_observed_map(k, ngl)
  obs_cat <- sample_observed_category(latent, map, pl)

  # Draw continuous covariatex X[i]
  x_mean <- sample_x_mean(k, p)
  x <- sample_x(latent, x_mean)

  # Compute response
  if (type == "global") {
    alpha <- sample_alpha(latent)
    beta <- sample_beta_global(p)
    y <- linear_global_response(alpha, x, beta)
  } else if (type == "latent") {
    alpha <- sample_alpha(latent)
    beta <- sample_beta_latent(k, p)
    y <- linear_latent_response(latent, alpha, x, beta)
  } else if (type == "hermite") {
    alpha <- rep(0, length(latent))
    xt <- generate_basis(x, order = 2)
    active <- sample.int(dim(xt)[2], replace = F, size = p)
    beta <- sample_beta_hermite(k, p)
    xt_active <- xt[, active]
    y <- linear_latent_response(latent, alpha, xt_active, beta)
  } else if (type == "nonlinear") {
    alpha <- sample_alpha(latent)
    beta <- sample_beta_latent(k, 2 * p)
    y <- dyadic_response(latent, alpha, x, beta)
  } else {
    stop("Bad method name.")
  }

  g <- factor(obs_cat)

  # Put it all together
  data <- list(
    x = x, y = y, l = latent, map = map,
    alpha = alpha, g = g, x_mean = x_mean,
    beta = beta
  )
  data
}
