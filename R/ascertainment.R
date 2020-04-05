ascertainment <- function(cfr_distr, death_to_case) {

    rho <- array(NA, dim = dim(death_to_case))

    for (idx in seq_len(nrow(death_to_case))) {

        rho[idx, ] <- cfr_distr / death_to_case[idx, ]
        rho[idx, which(rho[idx, ] > 1)] <- 1
    }

    rho
}
