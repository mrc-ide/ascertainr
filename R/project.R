##' Augment the time series of cases
##' for the days ahead to be able to
##' project deaths.
##'
##' Augmentation of case data for the
##' next \code{ndays} days involves
##' drawing samples from a distribution
##' (here \code{gamma} distribution)
##' with parameters estimated from the
##' last \code{previous} days.
##'
##' @title Augment the time series of cases
##' @return Matrix of observed and augmented cases. Dimensions of this
##' matrix are (2 * nsim) X (previous) where the observed values are
##' repeated across
##' @author Sangeeta Bhatia
##' @export
##' @importFrom stats rgamma sd
##' @import epitrix
##' @importFrom utils tail
augment_cases <- function(cases, previous, ndays, nsim = 10000) {

  sample_mean <- mean(tail(cases, previous))
  sample_sd <- stats::sd(tail(cases, previous))

  params <- epitrix::gamma_mucv2shapescale(
    mu = sample_mean,
    cv = sample_sd / sample_mean
  )
  ## Stick the observed data with the
  ## augmented data. To do this,
  ## repeat the observed data across
  ## all simulations

  i_old <- matrix(
    utils::tail(cases, previous),
    nrow = nsim,
    ncol = previous,
    byrow = FALSE
  )

  i_aug <- matrix(
    stats::rgamma(
      n = ndays * nsim,
      shape = params$shape,
      scale = params$scale
    ),
    nrow = nsim,
    ncol = ndays
  )

  rbind(i_old, i_aug)
}

##' Project the number of deaths
##'
##'
##'
##' @param augmented_cases
##' @param weights
##' @param trunc
##' @param ndays
##' @param nsim
##' @param deaths_to_cases
##' @return
##' @author Sangeeta Bhatia
##' @export
##' @importFrom stats rbinom
project_deaths <- function(augmented_cases, weights, trunc, ndays, nsim, deaths_to_cases) {

  wtd_incid <- apply(
    augmented_cases,
    2,
    function(x) weighted_incid(matrix(x, ncol = 1), weights, trunc)
  )

  out <- matrix(NA, nrow = nsim, ncol = ndays)
  for (idx in seq_len(ndays)) {
    out[, idx] <- rbinom(
        n = nsim,
        size = round(wtd_incid),
        prob = deaths_to_cases
      )

  }
  out
}
