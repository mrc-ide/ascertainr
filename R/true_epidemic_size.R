##' Estimate size of the epidemic upto the present time minus the mean
##' delay from case to death.
##'
##'
##'
##' @param deaths
##' @param mu_delta
##' @param cfr_distr
##' @return
##' @author Sangeeta Bhatia
##' @export
##' @importFrom stats rnbinom
episize_before_mu <- function(deaths, mu_delta, cfr_distr) {
  nsamples <- length(cfr_distr)
  i_true <- matrix(
    NA,
    nrow = nrow(deaths),
    ncol = nsamples
  )
  mu_delta <- round(mu_delta)
  f <- seq(1, nrow(deaths) - mu_delta)
  for (idx in f) {
    i_true[idx, ] <- deaths[idx + mu_delta, ] +
      stats::rnbinom(
        n = nsamples,
        size = deaths[idx + mu_delta, ],
        prob = cfr_distr
      )
  }
  i_true
}
##' Estimate epidemic size from the present time  minus the mean
##' delay between case and death up to present
##'
##'
##'
##'
##'
##' @param cases
##' @param rho
##' @return
##' @author Sangeeta Bhatia
##' @importFrom stats rnbinom
##' @export
episize_after_mu <- function(cases, rho) {
  nsamples <- ncol(rho)
  i_true <- matrix(
    NA,
    nrow = nrow(cases),
    ncol = nsamples
  )
  for (idx in seq_len(nrow(i_true))) {
    i_true[idx, ] <- cases[idx, ] +
      stats::rnbinom(
        n = nsamples,
        size = cases[idx, ],
        prob = rho[idx, ]
      )
  }

  i_true
}
