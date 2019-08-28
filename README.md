# sufrep: Sufficient Representations of Categorical Variables

This package implements the methods for providing sufficient representations of categorical variables mentioned in Johannemann et al. (2019).


To install this package, run the following commands in R:

```r
library(devtools)
install_github(grf-labs/sufrep)
```

Example usage:

```r
library(sufrep)

set.seed(12345)
n <- 100
p <- 3

X <- matrix(rnorm(n * p), n, p)
G <- as.factor(sample(5, size = n, replace = TRUE))

# One-hot encoding
onehot_encoder <- make_encoder(X = X, G = G, method = "one_hot")

train.df <- onehot_encoder(X = X, G = G)
print(head(train.df))

#         [,1]    [,2]    [,3] [,4] [,5] [,6] [,7]
# [1,]  0.5855  0.2239 -1.4361    0    0    0    1
# [2,]  0.7095 -1.1562 -0.6293    1    0    0    0
# [3,] -0.1093  0.4224  0.2435    0    0    1    0
# [4,] -0.4535 -1.3248  1.0584    0    0    0    1
# [5,]  0.6059  0.1411  0.8313    0    1    0    0
# [6,] -1.8180 -0.5360  0.1052    0    0    0    0

# "Means" encoding
means_encoder <- make_encoder(X = X, G = G, method = "means")

train.df <- means_encoder(X = X, G = G)
print(head(train.df))

#         [,1]    [,2]    [,3]    [,4]      [,5]     [,6]
# [1,]  0.5855  0.2239 -1.4361  0.1464  0.014645 -0.21528
# [2,]  0.7095 -1.1562 -0.6293  0.1892  0.177011  0.03227
# [3,] -0.1093  0.4224  0.2435 -0.3276 -0.009544  0.06307
# [4,] -0.4535 -1.3248  1.0584  0.1464  0.014645 -0.21528
# [5,]  0.6059  0.1411  0.8313  0.4915  0.159208  0.17173
# [6,] -1.8180 -0.5360  0.1052  0.5474 -0.056997 -0.16276
```

### References

Jonathan Johannemann, Vitor Hadad, Susan Athey, and Stefan Wager. _Sufficient Representations of Categorical Variables_. 2019.
