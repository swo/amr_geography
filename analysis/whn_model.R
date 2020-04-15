source("utils.R")

# Within-host neutral (WHN) model

whn_base_parms <- list(
  beta = 4.0,
  u = 1.0,
  k = 1.0,
  cost = 0.1683543
)

# WHN single-population model

# The ODE function only takes a vector, so we need to "unpack" the
# vector into a sensible data frame, compute the ODEs, then "pack"
# the data frame back into a vector.
whn_compartments <- c("X", "S", "R", "SR", "RS")

whn_unpack <- function(x) {
  matrix(x, ncol = length(whn_compartments)) %>%
    as_tibble(.name_repair = "minimal") %>%
    setNames(whn_compartments)
}

# data frame to vector
whn_pack <- function(df) {
  stopifnot(all(names(df) == whn_compartments))
  unlist(df)
}

whn_ode_func <- function(time_, state_vector, parms) {
  state <- whn_unpack(state_vector)

  with(c(state, parms), {
    N <- X + S + R + SR + RS # total number of individuals

    Stot <- S + SR
    Rtot <- R + RS
    dS <- (betaij %*% (Stot / N))*X - (u + taui)*S - k*(1 - cost)*(betaij %*% (Rtot / N))*S
    dR <- (1 - cost)*(betaij %*% (Rtot / N))*X - u*R - k*(betaij %*% (Stot / N))*R + taui*(SR + RS)
    dSR <- k*(1 - cost)*(betaij %*% (Rtot / N))*S - (u + taui)*SR
    dRS <- k*(betaij %*% (Stot / N))*R - (u + taui)*RS
    dX <- -(dS + dR + dSR + dRS)

    list(c(dX, dS, dR, dSR, dRS))
  })
}

# Run the simulation
whn_sim <- function(parms) {
  # Check that all the required parameters have been input
  parm_names <- c("beta", "cost", "k", "u", "taui", "transmission_matrix")
  stopifnot(all(parm_names %in% names(parms)))

  n_pop <- length(parms$taui)

  new_parms <- with(parms, {
    # check the transmission matrix is square with right row sums
    check_transmission_matrix(transmission_matrix, n_pop)
    betaij <- transmission_matrix * beta

    # Add transmission matrix beta_ij to the parameter list
    `$<-`(parms, "betaij", betaij)
  })

  # Initial state of the simulation
  state <- tibble(
    X = rep(0.990 / n_pop, n_pop),
    S = rep(0.005 / n_pop, n_pop),
    R = S,
    SR = 0,
    RS = 0
  )

  state_vector <- whn_pack(state)

  # Run the simulation
  result <- rootSolve::runsteady(state_vector, func = whn_ode_func, parms = new_parms)

  # Unpack the output state and clean it up
  result$y %>%
    whn_unpack() %>%
    mutate(
      tau = parms$taui,
      pop = seq_along(parms$taui),
      rho = (R + RS) / (S + SR + R + RS)
    )
}

memo_whn_sim <- memoise(whn_sim)
