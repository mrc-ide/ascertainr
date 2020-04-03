true_episize_past <- function(deaths, mu_delta, cfr_distr) {

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
            rnbinom(
                n = nsamples,
                size = deaths[idx + mu_delta, ],
                prob = cfr_distr
            )
    }
    i_true

}

true_episize_future <- function(cases, rho) {

    nsamples <- ncol(rho)
    i_true <- matrix(
        NA,
        nrow = nrow(cases),
        ncol = nsamples
    )
    for (idx in seq_len(nrow(i_true)) {

        i_true[idx, ] <- cases[idx, ] +
            rnbinom(
                n = nsamples,
                size = cases[idx, ],
                prob = rho[idx, ]
            )
    }

    i_true

}
