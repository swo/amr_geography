# Model utilities

library(magrittr)
library(tidyverse)
library(memoise)

set.seed(5)

memoise <- partial(memoise::memoise, cache = cache_filesystem("cache_memo/"))

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

# 1- and 2-pop utils

identity_matrix <- function(n) `diag<-`(matrix(0, ncol = n, nrow = n), 1)

epsilon_matrix <- function(eps) {
  stopifnot(between(eps, 0, 0.5))
  matrix(c(1 - eps, eps, eps, 1 - eps), nrow = 2, ncol = 2)
}

# Commuting utils

size <- 25
trans_datasets <- list(
  eu_flights = read_tsv("../db/europe/flights.tsv") %>%
    rename(unit1 = country1, unit2 = country2, n = n),
  us_commuting = read_tsv("../db/us/commuting.tsv") %>%
    rename(unit1 = from_state, unit2 = to_state, n = n_workers)
)

shrink_square_mat <- function(mat, n) {
  # matrix must be square
  n0 <- dim(mat)[1]
  stopifnot(dim(mat)[2] == n0)
  # must be asking for fewer columns
  stopifnot(n <= n0)

  # get indices to subsample
  idx <- sample(1:n0, n)
  mat[idx, idx]
}

scale_rows <- function(mat) {
  t(scale(t(mat), center = FALSE, scale = rowSums(mat)))
}
grow_diagonal <- function(mat, f) `diag<-`(mat, diag(mat) / f)

movement_counts_to_transmission_matrix <- function(tbl, size = NULL, f_internal = NULL) {
  # check that table has the expected columns
  stopifnot(all(names(tbl) == c("unit1", "unit2", "n")))
  # check that table can be symmetrical
  stopifnot(setequal(tbl$unit1, tbl$unit2))
  # get the names
  nms <- sort(unique(tbl$unit1))

  out <- tbl %>%
    arrange(unit1, unit2) %>%
    spread(unit2, n, fill = 0) %T>%
    {
      stopifnot(all(.$unit1 == nms))
      stopifnot(all(names(.) == c("unit1", nms)))
    } %>%
    select(-unit1) %>%
    as.matrix() %>%
    `rownames<-`(nms)

  if (!is.null(size)) out %<>% shrink_square_mat(size)
  if (!is.null(f_internal)) out %<>% grow_diagonal(f_internal)

  scale_rows(out)
}

