#' Extract the mean from the MCMC summary
#'
#' Extract the mean from the MCMC summary
#'
#' @param mcmc_summary list. output from \code{\link{summarize_mcmc}}
#'
#' @return data.frame
get_mcmc_mean <- function(mcmc_summary) {

    means <- lapply(mcmc_summary, function(x) `$`(x, 'statistics'))

    means <- do.call(rbind, means)[, 'Mean']

    means <- unlist(means)

    params <- sub('.*\\.', '', names(means))

    data.frame(parameter = params, mean = means)

}

#' Extract the credible interval from the MCMC summary
#'
#' Extract the credible interval from the MCMC summary
#'
#' @param mcmc_summary list. output from \code{\link{summarize_mcmc}}
#'
#' @return data.frame
get_mcmc_ci <- function(mcmc_summary) {

    quants <- lapply(mcmc_summary, function(x) `$`(x, 'quantile'))

    quants <- do.call(rbind, quants)

    ci <- quants[, c('2.5%', '97.5%')]

    param <- rownames(ci)

    data.frame(parameter = param, lower = ci[, 1], upper = ci[, 2])

}

#' Summarize the MCMC output
#'
#' Use coda summary.mcmc method to summarize MCMC output
#'
#' @param model_fit list. output from \code{\link{fit_model}}
#' @param parameters character vector of parameter names to summarize
#'
#' @return data.frame
summarize_mcmc <- function(model_fit, parameters) {

    mcmc_fit <- convert_mcmc(model_fit, params)

    mcmc_summary <- lapply(mcmc_fit, summary)

    average <- get_mcmc_mean(mcmc_summary)

    ci <- get_mcmc_ci(mcmc_summary)

    df <- merge(average, ci)

    df

}

#' Add the model type (observation, detection) to the table
#'
#' Add the model type (observation, detection) to the table
#'
#' @param mcmc_summary data.frame output from \code{\link{summarize_mcmc}}
#'
#' @return data.frame
add_model <- function(mcmc_summary) {

    site_params <- c(
        'a\\[',
        'c\\[',
        'kappa',
        'rho\\[',
        'e\\[',
        'mu_c',
        'sd_c'
    )

    site_exp <- paste(site_params, collapse = '|')

    visit_params <- c('b\\[', 'd\\[', 'mu_d', 'sd_d')

    visit_exp <- paste(visit_params, collapse = '|')

    mcmc_summary$model <- NA

    is_site_param <- grepl(site_exp, mcmc_summary$parameter)
    is_visit_param <- grepl(visit_exp, mcmc_summary$parameter)

    mcmc_summary$model[is_site_param] <- 'Occurrence'
    mcmc_summary$model[is_visit_param] <- 'Detection'

    mcmc_summary

}

#' Add the species to the table
#'
#' Add the species to the table
#'
#' @param mcmc_summary data.frame output from \code{\link{summarize_mcmc}}
#'
#' @return data.frame
add_species <- function(mcmc_summary) {

    cow_params <- c('a', 'b', 'rho', 'e', 'kappa')
    cow_exp <- paste(cow_params, collapse = '|')

    mcmc_summary$species <- NA

    is_cow <- grepl(cow_exp, mcmc_summary$parameter)
    mcmc_summary$species[is_cow] <- 'SHCO'

    hyper_params <- c('mu_c', 'mu_d', 'sd_c', 'sd_d')
    hyper_exp <- paste(hyper_params, collapse = '|')

    is_hyper <- grepl(hyper_exp, mcmc_summary$parameter)
    mcmc_summary$species[is_hyper] <- 'Host Community'

    # extract first dim, the host id, from matrix params
    # e.g, extrct 4 from c[4,3]
    host_id_exp <-  '\\d,'
    host_id <- regmatches(
        mcmc_summary$parameter,
        regexpr(host_id_exp, mcmc_summary$parameter)
    )

    host_id <- as.integer(gsub(',', '', host_id))

    host <- get_host_names()[host_id]

    is_host <- grepl(host_id_exp, mcmc_summary$parameter)
    mcmc_summary$species[is_host] <- host

    mcmc_summary

}

#' Add the covariate level (e.g., habitat:forest) to the table
#'
#' Add the covariate level to the table
#'
#' @param mcmc_summary data.frame output from \code{\link{summarize_mcmc}}
#'
#' @return data.frame
add_covariate <- function(mcmc_summary) {

    mcmc_summary$covariate <- NA

    # second digit indicates the covariate for each parameter
    cov_id_exp <-  ',\\d|\\[\\d\\]'
    cov_id <- regmatches(
        mcmc_summary$parameter,
        regexpr(cov_id_exp, mcmc_summary$parameter)
    )

    cov_id <- as.integer(gsub(',|\\[|\\]', '', cov_id))

    habitat <- get_habitat_names()
    observer <- c('Intercept', 'Observer B')

    ifelse(
        mcmc_summary$model == 'Detection',
        observer[cov_id],
        habitat[cov_id]
    )

    mcmc_summary$covariate <- ifelse(
        mcmc_summary$model == 'Detection',
        observer[cov_id],
        habitat[cov_id]
    )

    mcmc_summary$covariate[mcmc_summary$covariate == 'Forest'] <- 'Intercept'

    mcmc_summary$covariate[mcmc_summary$parameter == 'a[2]'] <- 'Coffee'

    is_host_cov <- grepl('rho|e', mcmc_summary$parameter)

    host_id <- cov_id[is_host_cov]

    host <- get_host_names()

    mcmc_summary$covariate[is_host_cov] <- host[host_id]

    mcmc_summary

}

#' Add a latex style parameter name to the table
#'
#' Add a latex style parameter name to the table
#'
#' @param mcmc_summary data.frame output from \code{\link{summarize_mcmc}}
#'
#' @return data.frame
add_parm <- function(mcmc_summary) {

    parm <- mcmc_summary$parameter

    parm[grepl('a\\[', parm)] <- '\\alpha'
    parm[grepl('b\\[', parm)] <- '\\beta'
    parm[grepl('c\\[\\d,\\d\\]', parm)] <- '\\gamma'
    parm[grepl('d\\[\\d,\\d\\]', parm)] <- '\\delta'
    parm[grepl('e\\[', parm)] <- '\\epsilon'
    parm[grepl('rho\\[', parm)] <- '\\rho'
    parm[parm == 'kappa'] <- '\\kappa'
    parm[grepl('mu_c\\[', parm)] <- '\\mu_{\\gamma}'
    parm[grepl('mu_d\\[', parm)] <- '\\mu_{\\delta}'
    parm[grepl('sd_c\\[', parm)] <- '\\sigma_{\\gamma}'
    parm[grepl('sd_d\\[', parm)] <- '\\sigma_{\\delta}'

    mcmc_summary$parm <- parm

    mcmc_summary

}

#' Make a table
#'
#' Summarize MCMC output and add relevant descriptions to each parameter
#'
#' @inheritParams summarize_mcmc
#'
#' @return data.frame
make_table <- function(model_fit, params) {

    tab <- summarize_mcmc(model_fit, params)

    tab <- add_model(tab)

    tab <- add_species(tab)

    tab <- add_covariate(tab)

    tab <- add_parm(tab)

    df
}

#' Make table 1
#'
#' Make table 1
#'
#' @param model_fit list. output from \code{\link{fit_model}}
#'
#' @return data.frame
#' @export
make_table_one <- function(model_fit) {

    params <- get_table1_parameters()

    tab <- make_table(model_fit, params)

    tab

}

#' Make table 2
#'
#' Make table 2
#'
#' @param model_fit list. output from \code{\link{fit_model}}
#'
#' @return data.frame
#'
#' @export
make_table_two <- function(model_fit) {

    params <- get_table2_parameters()

    tab <- make_table(model_fit, params)

    tab

}

#' Make table 3
#'
#' Make table 3
#'
#' @param model_fit list. output from \code{\link{fit_model}}
#'
#' @return data.frame
#' @export
make_table_three <- function(model_fit) {

    params <- get_table3_parameters()

    tab <- make_table(model_fit, params)

    tab

}
