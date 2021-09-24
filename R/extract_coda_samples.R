#' Extract MCMC samples from fit_model for use in coda.
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
extract_coda_samples <- function(model_fit, params = NULL) {

    if (is.null(params)) {
        params <- c('a', 'b', 'mu_c', 'sd_c', 'mu_d', 'sd_d')
    }

    diag_samples <- model_fit[names(model_fit) %in% params]

    diag_samples <- lapply(diag_samples, coda::as.mcmc.list)

    diag_samples

}
