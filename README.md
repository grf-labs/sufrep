# sufrep: Sufficient Representations of Categorical Variables

This package implements the methods for providing sufficient representations of categorical variables mentioned in Johannemann et al. (2019).


To install this package, run the following command in R (assuming you have installed `devtools`).

```r
devtools::install_github("grf-labs/sufrep")
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

#           [,1]      [,2]      [,3]     [,4]      [,5]       [,6]
# [1,]  0.585529  0.223925 -1.436146 0.103683 -0.187225 -0.1909485
# [2,]  0.709466 -1.156223 -0.629260 0.103683 -0.187225 -0.1909485
# [3,] -0.109303  0.422419  0.243522 0.427721  0.208770  0.0246111
# [4,] -0.453497 -1.324755  1.058362 0.195713 -0.207266  0.1346758
# [5,]  0.605887  0.141084  0.831349 0.195713 -0.207266  0.1346758
# [6,] -1.817956 -0.536048  0.105212 0.195713 -0.207266  0.1346758
```

### References

Jonathan Johannemann, Vitor Hadad, Susan Athey, and Stefan Wager. _Sufficient Representations of Categorical Variables_. 2019.
