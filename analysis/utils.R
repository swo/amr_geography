# Model utilities

library(tidyverse)
library(memoise)

my_memoise <- partial(memoise, cache = cache_filesystem("cache/"))

# Check that:
# - All entries in the transmission matrix are nonnegative
# - All rows add up to 1
all_nearly_equal <- function(x, y, tol = 1e-12) all(abs(x - y) < tol)

check_transmission_matrix <- function(mat, size) {
  stopifnot(all(mat >= 0))
  stopifnot(all_nearly_equal(rowSums(mat), 1.0))
  stopifnot(all(dim(mat) == c(size, size)))
  stopifnot(ncol(mat) == size)
  stopifnot(nrow(mat) == size)
}


# 1- and 2-pop utils --------------------------------------------------

identity_matrix <- function(n) `diag<-`(matrix(0, ncol = n, nrow = n), 1)

epsilon_matrix <- function(eps) {
  stopifnot(between(eps, 0, 0.5))
  matrix(c(1 - eps, eps, eps, 1 - eps), nrow = 2, ncol = 2)
}
