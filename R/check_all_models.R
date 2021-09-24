#' Check all models
#'
#' Perform posterior predictive checks for all models regarding the number of
#' sites where the parasite and host co-occur
#'
#' @param fit_list list. Output from \code{\link{fit_all_models}}
#' @param data_list list. Output from \code{\link{make_data_list}}
#' @param just_pvals logical. Should the output only include p-values, or
#' should the function additionally return the predictions and observations?
#'
#' @return list
#'
#' @export
check_all_models <- function(fit_list, data_list, just_pvals = TRUE){

    if (just_pvals)  {
        ppc <- sapply(fit_list, check_model, data_list, just_pvals)
    } else {
        ppc <- lapply(fit_list, check_model, data_list, just_pvals)
    }

    ppc

}
