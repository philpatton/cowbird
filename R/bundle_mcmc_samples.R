#' Function to converts MCMC lists to arrays
#'
#' This function is used to make manipulation of the mcmc samples easier.
#' It also only selects the first chain
#'
#' @param model_fit list. output from \code{fit_model()}
#'
#' @return array
#'
bundle_evaluation_samples = function(model_fit) {
    #mcmclist = mcmc list

    theta_samples <- model_fit$theta[ , , , 1]
    psi_samples <- model_fit$psi[ , , 1]
    eta_samples <- model_fit$eta[ , , , 1]
    zeta_samples <- model_fit$zeta[ , , , , 1]

    samples = list(
        theta_samples = theta_samples,
        psi_samples = psi_samples,
        eta_samples = eta_samples,
        zeta_samples = zeta_samples
    )

    samples

}

#' Function to bundles mcmc samples into a list
#'
#' This function is used to make manipulation of the mcmc samples easier.
#' It also reduces the
#'
#' @param model_fit list. output from \code{fit_model()}
#'
#' @return array
#'
bundle_ppc_samples = function(model_fit) {
    #mcmclist = mcmc list

    x_samples <- model_fit$X.new[ , , , , 1]
    y_samples <- model_fit$Y.new[ , , , 1]

    samples = list(
        x_samples = x_samples,
        y_samples = y_samples
    )

    samples

}

#' Function to bundles mcmc samples into a list
#'
#' This function is used to make manipulation of the mcmc samples easier.
#'
#' @param model_fit list. output from \code{fit_model()}
#'
#' @return array
#'
bundle_pred_samples = function(model_fit) {

    prediction_samples <- lapply(model_fit, function(x) `[`(x, , , 1))

    prediction_samples

}
