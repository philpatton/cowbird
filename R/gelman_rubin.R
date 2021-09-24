#' Calculate the gelman rubin statistic
#'
#' Calculates the gelman rubin statistic for mcmc parameters using
#' \code{\link[coda]{gelman.diag}}
#'
#' @param diag_samples list of mcmc.list objects
#'
#' @return data.frame
#'
#' @export
gelman_rubin <- function(diag_samples) {

    gr <- lapply(diag_samples, coda::gelman.diag)

    # select the relevant part of the output
    tmp <- lapply(gr, function(x) `$`(x, 'psrf'))

    # combine into data.frame
    gr <- do.call(rbind, tmp)

    gr

}
