ratio_deaths_cases <- function(wtd_incid, deaths, nsamples = 10000, twindow = 7) {

    r_t <- array(
        NA,
        dim = c(nrow(wtd_incid), nsamples)
    )

    for (idx in seq_len(nrow(wtd_incid))) {

        fidx <- seq(max(c(1, idx - twindow)), idx)

        if (sum(wtd_incid[fidx,]) > 0) {

            hpd <- binom::binom.bayes(
                x = sum(deaths[fidx, ]),
                n = sum(wtd_incid[fidx, ]),
                type = "central",
                conf.level = 0.95,
                tol = 1e-9
            )
            r_t[idx, ] <- stats::rbeta(
                n = nsamples,
                shape1 = hpd$shape1,
                shape2 = hpd$shape2
            )

        }
    }

    r_t

}
