#' Initialize values for MCMC
#'
#' Initializes values for MCMC, namely, the occurrence states of the parasites
#' and hosts.
#'
#' @param data_list list made from \code{make_data_list()}, containing detection
#' matrices for cowbird and hosts
#'
#' @return list, suitable for use in \code{rjags::jags.model()}
#' @export

initialize_values <- function(data_list) {

    X <- data_list$X
    Y <- data_list$Y

    # output from calc_naive_state_() functions is logical
    naive_state_host <- calc_naive_state_host(X) * 1
    naive_state_cow <- calc_naive_state_cowbird(Y) * 1

    list(
        z = naive_state_cow,
        tau = naive_state_host,
        .RNG.name = "base::Marsaglia-Multicarry",
        .RNG.seed = 17
    )

}
