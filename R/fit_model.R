#' Fit the cowbird-host co-occurrence models
#'
#' Fit the cowbird-host co-occurrence models described in Patton et al. (Year
#' TBD) using package rjags.
#'
#' The default MCMC parameters are set for quick model fitting. To use the MCMC
#' parameters described in the papers, use mcmc_params_paper = TRUE.
#'
#' @param model string indicating which model is to be fit.
#' @param data_list list of data from \code{make_data_list()}
#' @param params_to_monitor character vector indicating which parameters
#' should be saved from the MCMC samples
#' @param n.chains integer representing the number of MCMC chains to run
#' @param n.adapt integer representing the number of MCMC iterations to adapt
#' the sampler
#' @param n.burn integer representing the number of MCMC iterations to discard
#' before saving samples
#' @param n.iter integer representing the number of MCMC iterations to save
#' @param n.thin integer representing the number of MCMC iterations to thin
#' @param mcmc_params_paper logical. should we fit the model using the MCMC
#' parameters described in the paper?
#' @param fix_seed logical. should the seed be set for reproducible results?
#'
#' @return list, elements of class \code{rjags::mcarray}
#'
#' @seealso rjags documentation for fitting models with rjags
#' @export

fit_model <- function(model = c('model_1', 'model_2', 'model_3'), data_list,
                      params_to_monitor = NULL, n.chains = 1,
                      n.adapt = 100, n.burn = 10, n.iter = 10,
                      n.thin = 1, mcmc_params_paper = FALSE, fix_seed = FALSE) {

    if (model == 'model_1') {

        model_file <- system.file("models", "model1.txt", package = "cowbird")

    } else if (model == 'model_2') {

        model_file <- system.file("models", "model2.txt", package = "cowbird")

    } else if (model == 'model_3') {

        model_file <- system.file("models", "model3.txt", package = "cowbird")

    } else {

        stop("model not found. try 'model_1', 'model_2', or 'model_3'.")
    }

    if (mcmc_params_paper) {
        n.chains <- 2
        n.adapt <- 40000
        n.burn <- 40000
        n.iter <- 10000
        n.thin <- 1
    }

    # parameters to monitor
    if (is.null(params_to_monitor))  params_to_monitor <- get_hyper_parameters()

    initial_values <- initialize_values(data_list, fix_seed)

    start_time <- Sys.time()

    # fit model
    jags_model <- rjags::jags.model(
        model_file,
        n.adapt = n.adapt,
        inits = initial_values,
        n.chains = n.chains,
        data = data_list
    )

    update(jags_model, n.burn)

    samps <- rjags::jags.samples(
        jags_model,
        thin = n.thin,
        variable.names = params_to_monitor,
        n.iter = n.iter
    )

    end.time <- Sys.time()

    elapsed.time <- round(difftime(end.time, start_time, units='mins'), 2)
    m <- paste0('Posterior simulated in ', elapsed.time, ' minutes')
    message(m)

    samps

}

#' Fit every cowbird-host co-occurrence model
#'
#' Fit every cowbird-host co-occurrence models described in Patton et al. (Year
#' TBD).
#'
#' @inheritParams fit_model
#' 
#' @return list, elements of class \code{rjags::mcarray}
#'
#' @seealso rjags documentation for fitting models with rjags
#' @export
fit_all_models <- function(data_list, params_to_monitor = NULL,
                           n.chains = 1, n.adapt = 100, n.burn = 10,
                           n.iter = 10, n.thin = 1, fix_seed = FALSE) {

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
        fix_seed = fix_seed
    )

    list(fit1 = fit1, fit2 = fit2, fit3 = fit3)

}
