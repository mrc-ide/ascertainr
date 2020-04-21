##' Return the incidence time-series weighted by the reporting to
##' death delay
##'
##' Returns
##'
##' @param incid incidence time series as a T X 1 matrix
##' @param weights Discrete probability distribution of the reporting
##' to death delay
##' @return
##' @author Sangeeta Bhatia
weighted_incid <- function(incid, weights, trunc) {
  ndays <- nrow(incid)
  weights <- rev(weights)

  out <- matrix(NA, nrow = ndays, ncol = 1)

  for (idx in seq_len(ndays)) {
    fidx <- max(
      c(1, (idx - trunc))
    )
    out[idx, 1] <- t(incid[fidx:idx, ]) %*%
      weights[((trunc + 1) - (idx - fidx)):(trunc + 1)]
  }
  out
}
