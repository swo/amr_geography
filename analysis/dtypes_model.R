source("utils.R")

dtypes_base_parms <- list(
  n_d = 16,
  min_mu = 0.5,
  max_mu = 2.0,
  beta = 2.0,
  c_beta = 1.0,
  c_mu = 1.1,
  k = 15.0
)

dtypes_base_parms$mu_d <- with(
  dtypes_base_parms,
  seq(min_mu, max_mu, length = n_d)
)

# pack the list into a vector
dtypes_pack <- function(lst) {
  stopifnot(setequal(names(lst), c('X_i', 'S_id', 'R_id')))
  stopifnot(all(dim(lst$S_id) == dim(lst$R_id)))
  stopifnot(dim(lst$S_id)[1] == length(lst$X_i))
  c(lst$X_i, as.vector(lst$S_id), as.vector(lst$R_id))
}

# unpack the vector into a list
dtypes_unpack <- function(x, n_pop, n_d) {
  stopifnot(length(x) == n_pop + 2 * (n_d * n_pop))

  x_end <- n_pop
  s_start <- x_end + 1
  s_end <- x_end + n_d * n_pop
  r_start <- s_end + 1
  r_end <- s_end + n_d * n_pop
  stopifnot(r_end == length(x))

  list(
    X_i = x[1:x_end],
    S_id = matrix(x[s_start:s_end], nrow = n_pop, ncol = n_d),
    R_id = matrix(x[r_start:r_end], nrow = n_pop, ncol = n_d)
  )
}

# for initially packing the data frame into the list
dtypes_df_to_list <- function(df) {
  # check we have the right column names
  stopifnot(setequal(names(df), c('population', 'phenotype', 'dtype', 'value')))
  # check we have the right phenotypes
  stopifnot(setequal(df$phenotype, c('S', 'R', 'X')))

  # extract the no. populations and D-types
  n_pop <- length(unique(df$population))
  n_d <- length(unique(df$dtype))

  # there should be S and R for each pop/D-type combo
  stopifnot(nrow(filter(df, phenotype == 'S')) == n_pop * n_d)
  stopifnot(nrow(filter(df, phenotype == 'R')) == n_pop * n_d)
  # but X for only each pop
  stopifnot(nrow(filter(df, phenotype == 'X')) == n_pop)

  list(
    X_i = df %>% filter(phenotype == 'X') %>% pull(value),
    S_id = df %>% filter(phenotype == 'S') %>% pull(value) %>% matrix(nrow = n_pop, ncol = n_d),
    R_id = df %>% filter(phenotype == 'R') %>% pull(value) %>% matrix(nrow = n_pop, ncol = n_d)
  )
}

# for finally unpacking the list into a data frame
dtypes_list_to_df <- function(lst) {
  # check for names
  stopifnot(setequal(names(lst), c('X_i', 'S_id', 'R_id')))

  # get dimensions
  n_pop <- length(lst$X_i)
  n_d <- dim(lst$S_id)[2]

  # check shapes
  stopifnot(all(dim(lst$S_id) == c(n_pop, n_d)))
  stopifnot(all(dim(lst$R_id) == c(n_pop, n_d)))

  X_rows <- tibble(pop = 1:n_pop, phenotype = 'X', dtype = NA, value = lst$X_i)
  S_rows <- crossing(dtype = 1:n_d, pop = 1:n_pop) %>%
    mutate(phenotype = 'S', value = as.vector(lst$S_id))
  R_rows <- crossing(dtype = 1:n_d, pop = 1:n_pop) %>%
    mutate(phenotype = 'R', value = as.vector(lst$R_id))

  bind_rows(X_rows, S_rows, R_rows) %>%
    arrange(pop, phenotype, dtype)
}

dtypes_ode_func <- function(t, state_vector, parms) {
  state <- dtypes_unpack(state_vector, n_pop = parms$n_pop, n_d = parms$n_d)

  with(c(state, parms), {
    stopifnot(length(X_i) == n_pop)
    stopifnot(dim(S_id) == c(n_pop, n_d))
    stopifnot(dim(R_id) == c(n_pop, n_d))

    # rowSums are over D-types (second index)
    v_id <- (1.0 - ((S_id + R_id) / rowSums(S_id + R_id) - 1.0 / n_d)) ** k
    stopifnot(dim(v_id) == c(n_pop, n_d))

    N_i <- X_i + rowSums(S_id + R_id)
    stopifnot(length(N_i) == n_pop)

    # "tau_i * S_id" does sum_i { tau_i S_id }, which is length P vector
    # to multiply by rows, need to do some fancy footwork: "mat %*% diag(row)"
    dS_id <- v_id * (beta_ij %*% (S_id / N_i)) * X_i - tau_i * S_id - S_id %*% diag(mu_d)
    dR_id <- v_id * (beta_ij %*% (R_id / N_i)) * X_i - c_mu * R_id %*% diag(mu_d)
    dX_i <- -rowSums(dS_id + dR_id)

    list(dtypes_pack(list(X_i = dX_i, S_id = dS_id, R_id = dR_id)))
  })
}

dtypes_sim_nomemo <- function(parms) {
  stopifnot(
    setequal(
      names(parms),
      c(names(dtypes_base_parms), "tau_i", "transmission_matrix")
    )
  )
  
  n_pop <- length(parms$tau_i)
  check_transmission_matrix(parms$transmission_matrix, n_pop)
  
  parms$beta_ij <- with(parms, { beta * transmission_matrix })
  parms$n_pop <- n_pop

  n_d <- parms$n_d
  state <- list(
    X_i = rep(0.9 / n_pop, n_pop),
    S_id = matrix(0.05 / (n_pop * n_d), nrow = n_pop, ncol = n_d),
    R_id = matrix(0.05 / (n_pop * n_d), nrow = n_pop, ncol = n_d)
  )

  state_vector <- dtypes_pack(state)
  stopifnot(all.equal(sum(state_vector), 1))

  result <- rootSolve::runsteady(
    state_vector,
    func = dtypes_ode_func,
    parms = parms,
    stol = 1e-8 / n_pop,
    rtol = 1e-6 / n_pop,
    atol = 1e-6 / n_pop
  )

  result$y %>%
    dtypes_unpack(n_pop = n_pop, n_d = n_d) %>%
    dtypes_list_to_df() %>%
    left_join(tibble(pop = 1:n_pop, tau = parms$tau_i), by = 'pop')
}

dtypes_sim <- my_memoise(dtypes_sim_nomemo)

dtypes_epsilon_values <- c(0, 1e-4, 0.001, 0.005, 0.01, 0.0175, 0.025, 0.05, 0.075, 0.1)

dtypes_simplify_results <- function(df) {
  df %>%
    group_by(pop, phenotype) %>%
    summarize(
      value = sum(value),
      tau = unique(tau)
    ) %>%
    ungroup() %>%
    spread(phenotype, value) %>%
    mutate(rho = R / (R + S))
}

dtypes_2pop_sim <- function(tau1, tau2, epsilon) {
  parms <- dtypes_base_parms %>%
    `$<-`("transmission_matrix", epsilon_matrix(epsilon)) %>%
    `$<-`("tau_i", c(tau1, tau2))
  
  dtypes_sim(parms) %>%
    dtypes_simplify_results
}
