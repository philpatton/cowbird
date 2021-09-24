

#' Calculate the effective sample size
#'
#' Calculates the effective sample sizes for mcmc parameters using
#' \code{\link[coda]{effectiveSize}}
#'
#' @param diag_samples list of mcmc.list objects
#'
#' @return data.frame
#'
#' @export
effective_sample_size <- function(diag_samples) {

    ess <- lapply(diag_samples, coda::effectiveSize)
    ess <- unlist(ess)

    parameter <- names(ess)
    parameter <- sub('.*\\.', '', parameter)

    ess <- data.frame(parameter = parameter, ess = ess)
    row.names(ess) <- NULL

    ess[order(ess$ess, decreasing = F), ]

}
