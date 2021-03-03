#' Determine if the species was detected
#'
#' Create a logical indicator of the naive occurrence state
#'
#' @param detections numeric vector of detections
#'
#' @return logical.

is_detected <- function(detections){
    sum(detections, na.rm = T) > 0
}

#' The observed number of sites with cowbird and each host
#'
#' Calculate the number of sites where cowbird and each host were observed.
#' This is used in posterior predictive checking.
#'
#' @param data_list output from \code{\link{make_data_list}}
#'
#' @return numeric vector
sites_with_both_observed <- function(data_list) {

    x_observed <- data_list$X
    y_observed <- data_list$Y

    naive_state_host <- apply(x_observed, c(1, 2), is_detected)
    naive_state_cowbird <- apply(y_observed, 1, is_detected)

    naive_cooccur <- naive_state_host * naive_state_cowbird

    sites_with_both <- apply(naive_cooccur, 2, sum)

    sites_with_both

}

#' The predicted number of sites with cowbird and each host
#'
#' Calculate the number of sites where the cowbird and each host were predicted
#' to co-occur. This is used in posterior predictive checking.
#'
#' @param model_fit output from \code{\link{fit_model}}
#'
#' @return numeric array
sites_with_both_predicted <- function(model_fit) {

    samples <- bundle_mcmc_samples(model_fit)

    x_samples <- samples$X.new_samples
    y_samples <- samples$Y.new_samples

    naive_state_host <- apply(x_samples, c(1, 2, 4), is_detected)
    naive_state_cowbird <- apply(y_samples, c(1, 3), is_detected)

    # reshape the arrays so they can be multiplied
    naive_state_host <- aperm(naive_state_host, c(1, 3, 2))
    naive_state_cowbird <- array(
        naive_state_cowbird,
        dim = dim(naive_state_host)
    )

    naive_cooccur_state <- naive_state_host * naive_state_cowbird

    sites_with_both <- apply(naive_cooccur_state, c(2, 3), sum)

    t(sites_with_both)

}

#' Check a model
#'
#' Perform a posterior predictive check of the number of sites where the
#' parasite and host co-occur
#'
#' @param model_fit list. Output from \code{\link{fit_model}}
#' @param data_list list. Output from \code{\link{make_data_list}}
#' @param just_pvals logical. Should the output only include p-values, or
#' should the function additionally return the predictions and observations?
#'
#' @return if just_pvals, then a vector of p-values. Otherwise, a list.
#'
#' @export
check_model <- function(model_fit, data_list, just_pvals = TRUE){

    has_checking_params <- all(c('X.new', 'Y.new') %in% names(model_fit))

    if (!has_checking_params) {
        stop('model_fit must have samples for "X.new" and "Y.new"')
    }

    observed <- sites_with_both_observed(data_list)

    predicted <- sites_with_both_predicted(model_fit)

    prediction_exceeds_observed <- observed > predicted

    pvals <- apply(prediction_exceeds_observed, 1, mean)

    if (just_pvals) {
        output <- pvals
    } else {
        output <- list(
            naive_cooccur_observed = observed,
            naive_cooccur_predicted = predicted,
            pvals = pvals
        )
    }

    output

}

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

    ppc <- sapply(fit_list, check_model, data_list)

}
