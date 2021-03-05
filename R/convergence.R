#' Convert output from fit_model to ggs
#'
#' Converts mcarrays to ggs, a class for easy MCMC diagnostic plots, tests,
#' etc.
#'
#' @param model_fit output from fit_model
#'
#' @return list of ggs objects
#'
#' @export
make_diagnostic <- function(model_fit) {

    params <- c('a', 'b', 'mu_c', 'sd_c', 'mu_d', 'sd_d')

    diag_params <- model_fit[names(model_fit) %in% params]

    coda_list <- lapply(diag_params, coda::as.mcmc.list)

    diagnostic <- lapply(coda_list, ggmcmc::ggs)

    diagnostic

}
