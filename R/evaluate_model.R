#' Function to calculate the  likelihood of the host model
#'
#' Calculate the likelihood of observations at each site given MCMC samples
#' of parameter estimates.
#'
#' @param X three dimensional array of host detection data, number of sites by
#' number of hosts by number of visits (J x N x K)
#' @param eta_samples three dimensional array of MCMC samples for eta parameters,
#' number of sites by number of hosts by number of iterations (J x N x S)
#' @param zeta_samples four dimensional array of MCMC samples for eta parameters,
#' number of sites by number of hosts by number of visits by number of
#' iterations
#'
#' @return three dimensional array containing the  likelihood of the
#' detection data given parameter values at each MCMC iteration
#'
#' @export
calc_likelihood_host = function(X, eta_samples, zeta_samples) {

    # number of sites
    J <- dim(X)[1]
    # number of hosts
    N <- dim(X)[2]
    # number of iterations
    S <- dim(zeta_samples)[4]

    naive_state <- calc_naive_state_host(X)

    # calculate  likelihood for each site, host, and
    site_likelihood <- array(NA, dim = c(J, N, S))

    for (s in 1:S) {
        for (j in 1:J) {
            for (i in 1:N) {

                site_likelihood[j, i, s] <- calc_site_likelihood(
                    pr_detection = zeta_samples[j, i, , s],
                    detection_history = X[j, i, ],
                    pr_occupancy = eta_samples[j, i, s],
                    detected_at_site = naive_state[j, i]
                )

            }
        }
    }

    likelihood_host <- reshape2::melt(
        site_likelihood,
        varnames = c('site', 'species', 'iteration'),
        value.name = 'likelihood'
    )

    likelihood_host

}

#' Function to calculate the likelihood of the cowbird model
#'
#' Calculate the likelihood of observations at each site given MCMC samples
#' of parameter estimates.
#'
#' @param Y matrix of host detection data, number of sites by number of visits
#' (J x K)
#' @param theta_samples matrix of MCMC samples for theta parameters,
#' number of sites by by number of visits by number of iterations (J x K x S)
#' @param psi_samples matrix of MCMC samples for eta parameters,
#' number of sites by number of iterations
#'
#' @return matrix containing the likelihood of the
#' detection data given parameter values at each MCMC iteration
#'
#' @export
calc_likelihood_cowbird = function(Y, theta_samples, psi_samples) {

    # number of sites
    J <- dim(Y)[1]
    # number of iterations
    S <- dim(theta_samples)[3]

    naive_state <- calc_naive_state_cowbird(Y)

    # calculate the -likelihood for each site and iteration
    site_likelihood <- array(NA, dim = c(J, S))

    for (s in 1:S) {
        for (j in 1:J) {

            site_likelihood[j, s] <- calc_site_likelihood(
                pr_detection = theta_samples[j, , s],
                detection_history = Y[j, ],
                pr_occupancy = psi_samples[j, s],
                detected_at_site = naive_state[j]
            )

        }
    }

    cowbird_likelihood <- reshape2::melt(
        site_likelihood,
        varnames = c('site', 'iteration'),
        value.name = 'likelihood'
    )

    cowbird_likelihood

}

#' Function to estimate the WAIC for a model
#'
#' WAIC is used to estimate the parsimony of each model.
#'
#' @param data_list list. output from \code{make_data_list()} function.
#' @param model_fit list. output from \code{fit_model()} function.
#'
#' @return data.frame of estimates for WAIC, elppd, and pdWAIC
#'
#' @export
evaluate_model = function(model_fit, data_list) {

    has_eval_samples <- all(get_evaluation_parameters() %in% names(model_fit))

    if (!has_eval_samples) {
        stop('model_fit must have samples for eta, zeta, psi, and theta')
    }

    samples <- bundle_mcmc_samples(model_fit)

    likelihood_cowbird = calc_likelihood_cowbird(
        Y = data_list$Y,
        theta_samples = samples$theta_samples,
        psi_samples = samples$psi_samples
    )

    likelihood_host = calc_likelihood_host(
        X = data_list$X,
        zeta_samples = samples$zeta_samples,
        eta_samples = samples$eta_samples
    )

    elppd = calc_elppd(likelihood_host, likelihood_cowbird)

    pdwaic = calc_pdwaic(likelihood_host, likelihood_cowbird)

    waic = calc_waic(elppd, pdwaic)

    df = data.frame(WAIC = waic, ELPPD = elppd, PDWAIC = pdwaic)

    df <- signif(df, 4)

    df

}
