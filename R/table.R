sum_fun <- function(x) c(mean(x), stats::quantile(x, c(0.025, 0.975)))

#' Get the model type (observation or occurrence) for a parameter
#'
#' This function is used to build the estimate table.
#'
#' @param sample_summary data.frame. summary of mcmc samples.
#'
#' @return character vector.
get_mod <- function(sample_summary) {

    num_covs <- tapply(
        sample_summary$covariate,
        sample_summary$parameter,
        max
    )
    mod <- ifelse(num_covs == 2, 'Observation', 'Occurrence')

    mod[sample_summary$parameter]

}


#' Get the covariate level (e.g., interecept or observer B.) for a parameter
#'
#' This function is used to build the estimate table.
#'
#' @param sample_summary data.frame. summary of mcmc samples.
#'
#' @return character vector.
get_cov <- function(sample_summary) {

    cov <- ifelse(
        sample_summary$model == 'Occurrence',
        get_habitat_names()[sample_summary$covariate],
        c('Intercept', 'Obs.B')[sample_summary$covariate]
    )

    cov[cov == 'Forest'] <- 'Intercept'

    cov

}


#' Get the full parameter name
#'
#' This function is used to build the estimate table.
#'
#' @param sample_summary data.frame. summary of mcmc samples.
#'
#' @return character vector.
get_parm <- function(sample_summary) {

    parm <- gsub('_samples', '', sample_summary$parameter)

    parm[parm == 'a'] <- '\\alpha'
    parm[parm == 'b'] <- '\\beta'
    parm[parm == 'c'] <- '\\gamma'
    parm[parm == 'd'] <- '\\delta'
    parm[parm == 'mu_c'] <- '\\mu_{\\gamma}'
    parm[parm == 'mu_d'] <- '\\mu_{\\delta}'
    parm[parm == 'sd_c'] <- '\\sigma_{\\gamma}'
    parm[parm == 'sd_d'] <- '\\sigma_{\\delta}'

    parm

}


#' Summarize MCMC samples for vector parameters
#'
#' The JAGS code contains parameters that are either vectors (e.g., alpha) or
#' matrices (e.g., gamma). The latter describe the individual covariate effects
#' for the hosts. The former can be covariate effects from the cowbird model,
#' or the hyper parameters for the host model.
#'
#' @param vector_samples MCMC samples for the vector parameters
#'
#' @return data.frame
summarize_vector_params <- function(vector_samples) {

    names(vector_samples) <- c('covariate', 'iteration', 'value', 'X', 'parameter')
    vector_samples <- vector_samples[, -4]

    vec_df <- stats::aggregate(
        value ~ covariate + parameter,
        vector_samples,
        sum_fun
    )
    vec_df <- do.call(data.frame, vec_df)

    vec_df

}

summarize_host_effect <- function(he_samples) {

    names(he_samples) <- c('host', 'iteration', 'value', 'parameter')

    he_df <- stats::aggregate(
        value ~ host + parameter,
        he_samples,
        sum_fun
    )
    he_df <- do.call(data.frame, he_df)

    he_df

}


#' Summarize MCMC samples for matrix parameters
#'
#' The JAGS code contains parameters that are either vectors (e.g., alpha) or
#' matrices (e.g., gamma). The latter describe the individual covariate effects
#' for the hosts. The former can be covariate effects from the cowbird model,
#' or the hyper parameters for the host model.
#'
#' @param matrix_samples MCMC samples for the matrix parameters
#'
#' @return data.frame
summarize_matrix_params <- function(matrix_samples) {

    names(matrix_samples) <- c('covariate', 'host', 'value', 'iteration', 'parameter')

    mat_df <- stats::aggregate(
        value ~ host + covariate + parameter,
        matrix_samples,
        sum_fun
    )
    mat_df <- do.call(data.frame, mat_df)

    mat_df

}

#' Make MCMC output intelligible
#'
#' The JAGS output is unintelligible without referencing the JAGS model .txt
#' file. This will make the output more intelligible
#'
#' @param sample_sum data.frame containing summary of MCMC output
#'
#' @return data.frame
beautify_df <- function(sample_sum) {

    sample_sum$parameter <- gsub('_samples', '', sample_sum$parameter)

    sample_sum$model <- get_mod(sample_sum)

    sample_sum$level <- get_cov(sample_sum)

    sample_sum$param <- get_parm(sample_sum)

    names(sample_sum)[3:5] <- c('mean', 'lower_95', 'upper_95')

    sample_sum

}

#' Convert the numerical species column to words
#'
#' This makes the MCMC output more intelligible
#'
#' @param vec_sum data.frame containing summaries of the vector parameters
#'
#' @return data.frame
rename_species_vec <- function(vec_sum) {

    vec_sum$species <- NA

    vec_sum$species[grepl('a_', vec_sum$parameter)] <- 'SHCO'
    vec_sum$species[grepl('b_', vec_sum$parameter)] <- 'SHCO'

    vec_sum$species[grepl('mu', vec_sum$parameter)] <- 'Host Community'
    vec_sum$species[grepl('sd', vec_sum$parameter)] <- 'Host Community'

    vec_sum

}

#' Convert the numerical species column to words
#'
#' This makes the MCMC output more intelligible
#'
#' @param mat_sum data.frame containing summaries of the matrix parameters
#'
#' @return data.frame
rename_species_mat <- function(mat_sum) {

    mat_sum$species <- get_host_names()[mat_sum$host]
    mat_sum$host <- NULL

    mat_sum

}

#' Create a table of parameter estimates
#'
#' Creates a table of parameter estimates, e.g., Tables 1
#'
#' @param model_fit output from \code{\link{fit_model}}
#'
#' @return data.frame
#' 
#' @export
make_table_one <- function(model_fit) {

    parameters <- get_hyper_parameters()

    model_fit <- model_fit[names(model_fit) %in% parameters]

    samples <- bundle_mcmc_samples(model_fit)

    tmp <- reshape2::melt(samples)

    is_mat_parm <- tmp$L1 %in% c('c_samples', 'd_samples')

    mat_parms <- tmp[is_mat_parm, ]
    vec_parms <- tmp[!is_mat_parm, ]

    mat_sum <- summarize_matrix_params(mat_parms)
    vec_sum <- summarize_vector_params(vec_parms)

    mat_sum <- rename_species_mat(mat_sum)
    vec_sum <- rename_species_vec(vec_sum)

    mat_sum <- beautify_df(mat_sum)
    vec_sum <- beautify_df(vec_sum)

    tab <- rbind(mat_sum, vec_sum)

    # reorder the columns
    cols <- c(
        'species',
        'model',
        'param',
        'parameter',
        'level',
        'mean',
        'lower_95',
        'upper_95'
    )

    tab <- tab[, cols]

    # sort the rows
    is_cow <- tab$species == 'SHCO'
    is_hyper <- tab$species == 'Host Community'

    row_order <- order(
        is_cow, is_hyper, tab$species, tab$model, tab$param, tab$level
    )

    tab <- tab[row_order, ]

    # round the number columns
    ids <- tab[1:5]
    numbers <- signif(tab[6:8], digits = 3)

    cbind(ids, numbers)

}

#' Create a table of parameter estimates
#'
#' Creates a table of parameter estimates, e.g., Tables 2
#'
#' @param model_fit output from \code{\link{fit_model}}
#'
#' @return data.frame
#' 
#' @export
make_table_two <- function(model_fit) {

    tab <- make_table_one(model_fit)

    samples <- bundle_mcmc_samples(model_fit)

    kappa_sum <- sum_fun(samples$kappa_samples)

    kappa_df <- data.frame(
        species = 'SHCO',
        model = 'Occurrence',
        param = '\\kappa',
        parameter = 'kappa',
        level = 'Host Richness',
        mean = kappa_sum[1],
        lower_95 = kappa_sum[2],
        upper_95 = kappa_sum[3]
    )

    tab <- rbind(tab, kappa_df)

    # sort the rows
    is_cow <- tab$species == 'SHCO'
    is_hyper <- tab$species == 'Host Community'

    row_order <- order(
        is_cow, is_hyper, tab$species, tab$model, tab$param, tab$level
    )

    tab <- tab[row_order, ]

    # round the number columns
    ids <- tab[1:5]
    numbers <- signif(tab[6:8], digits = 3)

    cbind(ids, numbers)

}

#' Create a table of parameter estimates
#'
#' Creates a table of parameter estimates, e.g., Tables 3
#'
#' @param model_fit output from \code{\link{fit_model}}
#'
#' @return data.frame
#' 
#' @export
make_table_three <- function(model_fit) {

    parameters <- get_table3_parameters()

    tab <- make_table_one(model_fit)

    host_effects <- c('e', 'rho')
    is_he <- names(model_fit) %in% host_effects

    he_samples <- bundle_mcmc_samples(model_fit[is_he])
    tmp <- reshape2::melt(he_samples)

    he_sum <- summarize_host_effect(tmp)

    he_sum$level <- get_host_names()[he_sum$host]
    he_sum$host <- NULL

    he_sum$parameter <- gsub('_samples', '', he_sum$parameter)

    names(he_sum)[2:4] <- c('mean', 'lower_95', 'upper_95')

    he_sum$param <- ifelse(he_sum$parameter == 'e', '\\epsilon', '\\rho')

    he_sum$species <- 'SHCO'

    he_sum$model <- 'Occurrence'

    tab <- rbind(tab, he_sum)

    # sort the rows
    is_cow <- tab$species == 'SHCO'
    is_hyper <- tab$species == 'Host Community'

    row_order <- order(
        is_cow, is_hyper, tab$species, tab$model, tab$param, tab$level
    )

    tab <- tab[row_order, ]

    # round the number columns
    ids <- tab[1:5]
    numbers <- signif(tab[6:8], digits = 3)

    cbind(ids, numbers)

}
