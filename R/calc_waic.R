#' Calculate elppd
#'
#' Calculate the pointwise expected log predictive posterior density for
#' the model
#'
#' @param host_likelihood three dimensional array of likelihood estimates
#' from \code{\link{calc_likelihood_host}}
#' @param cowbird_likelihood matrix containing likelihood estimates
#' from \code{\link{calc_likelihood_cowbird}}
#'
#' @return numeric. estimate of the pointwise expected log predictive
#' posterior density
#' @seealso See Appendix S1 in \href{https://esajournals.onlinelibrary.wiley.com/doi/abs/10.1890/15-1471.1}{Broms, Hooten, and Fitzpatrick 2016}
#' for information about the formula
#'
calc_elppd <- function(host_likelihood, cowbird_likelihood) {

    cowbird_likelihood$species <- 'SHCO'

    likelihood <- rbind(host_likelihood, cowbird_likelihood)

    expected_loglik <- tapply(
        likelihood$likelihood,
        list(likelihood$site, likelihood$species),
        function(x) log(mean(x))
    )

    elppd = sum(expected_loglik)

    elppd

}

#' Calculate pdwaic
#'
#' Calculate the pD_{WAIC}, an estimate of model complexity
#'
#' @param host_likelihood three dimensional array of likelihood estimates
#' from \code{\link{calc_likelihood_host}}
#' @param cowbird_likelihood matrix containing likelihood estimates
#' from \code{\link{calc_likelihood_cowbird}}
#'
#' @return numeric. estimate of pdwaic
#'
#' @seealso See Appendix S1 in \href{https://esajournals.onlinelibrary.wiley.com/doi/abs/10.1890/15-1471.1}{Broms, Hooten, and Fitzpatrick 2016}
#' for information about the formula
#'
calc_pdwaic <- function(host_likelihood, cowbird_likelihood) {

    cowbird_likelihood$species <- 'SHCO'

    likelihood <- rbind(host_likelihood, cowbird_likelihood)

    log_var <- tapply(
        likelihood$likelihood,
        list(likelihood$site, likelihood$species),
        function(x) stats::var(log(x))
    )

    pdwaic = sum(log_var)

    pdwaic

}

#' Calculate WAIC
#'
#' Calculate WAIC, a measure of model parsimony
#'
#' @param elppd numeric
#' @param pdwaic numeric
#'
#' @return numeric. estimate of WAIC
#'
#' @seealso See Appendix S1 in \href{https://esajournals.onlinelibrary.wiley.com/doi/abs/10.1890/15-1471.1}{Broms, Hooten, and Fitzpatrick 2016}
#' for information about the formula
#'
calc_waic <- function(elppd, pdwaic) -2 * elppd + 2 * pdwaic
