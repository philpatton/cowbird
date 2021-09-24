#' Fit every cowbird-host co-occurrence model
#'
#' This function is a wrapper for the \code{fit_model} function, which is used
#' to fit the individual models in the paper.
#'
#' @inheritParams fit_model
#'
#' @return list, elements of class \code{rjags::mcarray}
#'
#' @seealso rjags documentation for fitting models with rjags
#' @export
fit_all_models <- function(data_list, params_to_monitor = NULL,
                           n.chains = 1, n.adapt = 100, n.burn = 10,
                           n.iter = 10, n.thin = 1,
                           mcmc_params_paper = FALSE, fix_seed = FALSE) {

    if (is.null(params_to_monitor))  params_to_monitor <- get_hyper_parameters()

    fit1 <- fit_model(
        'model_1',
        data_list = data_list,
        params_to_monitor = params_to_monitor,
        n.chains = n.chains,
        n.adapt = n.adapt,
        n.burn = n.burn,
        n.iter = n.iter,
        n.thin = n.thin,
        mcmc_params_paper = mcmc_params_paper,
        fix_seed = fix_seed
    )

    fit2 <- fit_model(
        'model_2',
        data_list = data_list,
        params_to_monitor = params_to_monitor,
        n.chains = n.chains,
        n.adapt = n.adapt,
        n.burn = n.burn,
        n.iter = n.iter,
        n.thin = n.thin,
        mcmc_params_paper = mcmc_params_paper,
        fix_seed = fix_seed
    )

    fit3 <- fit_model(
        'model_3',
        data_list = data_list,
        params_to_monitor = params_to_monitor,
        n.chains = n.chains,
        n.adapt = n.adapt,
        n.burn = n.burn,
        n.iter = n.iter,
        n.thin = n.thin,
        mcmc_params_paper = mcmc_params_paper,
        fix_seed = fix_seed
    )

    list(fit1 = fit1, fit2 = fit2, fit3 = fit3)

}
