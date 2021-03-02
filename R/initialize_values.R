#' Initialize values for MCMC
#'
#' Initializes values for MCMC, namely, the occurrence states of the parasites
#' and hosts.
#'
#' @param data_list list made from \code{make_data_list()}, containing detection
#' matrices for cowbird and hosts
#'
#' @return a function, suitable for use in \code{rjags::jags.model()}
#' @export

initialize_values <- function(data_list) {

    X <- data_list$X
    Y <- data_list$Y

    naive_state_host <- apply(X, c(1,2), sum, na.rm = T)
    naive_state_host[naive_state_host > 0] <- 1

    naive_state_cow <- apply(Y, 1, sum, na.rm = T)
    naive_state_cow[naive_state_cow > 0] <- 1

    intial_values <- function() {
        list(
            z = naive_state_cow,
            tau = naive_state_host,
            .RNG.name = 'base::Super-Duper',
            .RNG.seed = 17
        )
    }

    intial_values

}
