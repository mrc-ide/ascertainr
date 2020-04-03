ratio_deaths_cases <- function(wtd_incid, deaths, nsamples, window = 7) {

    for (idx in seq_len(nrow(wtd_incid))) {

        f <- seq(max(c(1, idx - window)), idx)
        r_t <- array(
            NA,
            dim = c(nrow(wtd_incid), nsamples)
        )

        if (sum(wtd_incid[f,]) > 0) {
            hpd <- binom.bayes(
                x = sum(deaths[f, ]),
                n = sum(wtd_incid[f, ]),
                type = "central",
                conf.level = 0.95,
                tol = 1e-9
            )
            r_t[idx, ] <- rbeta(
                n = nsamples,
                shape1 = hpd$shape1,
                shape2 = hpd$shape2
            )

        }
    }

    r_t

}
