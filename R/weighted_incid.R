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
weighted_incid <- function(incid, weights) {

    ndays <- nrow(incid)
    ## how far back do you want to go.
    trunc <- length(weights)

    if (trunc < ndays) {
        less <- ndays - trunc
        weights <- c(rep(0, less), weights)
    } else {
        weights <- weights[seq_len(ndays)]
    }
    weights <- rev(weights)

    out <- matrix(NA, nrow = ndays, ncol = 1)

    for (idx in seq_len(ndays)) {

        out[idx, ] <- utils::tail(weights, idx) %*% incid[seq_len(idx), , drop = FALSE]
    }

    out



}