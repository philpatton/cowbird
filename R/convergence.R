#' Convert output from fit_model to \code{\link[coda]{mcmc.list}}
#'
#' Converts mcarrays to \code{\link[coda]{mcmc.list}}, the coda class for mcmc
#' diagnostics and processing
#'
#' @param model_fit output from fit_model
#' @param params list of parameters to diagnose
#'
#' @return list of mcmc.list objects
#'
#' @export
convert_mcmc <- function(model_fit, params = NULL) {

    rjags::jags.version()
    
    if (is.null(params)) {
        params <- c('a', 'b', 'mu_c', 'sd_c', 'mu_d', 'sd_d')
    }

    diag_samples <- model_fit[names(model_fit) %in% params]

    diag_samples <- lapply(diag_samples, coda::as.mcmc.list)

    diag_samples

}

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
