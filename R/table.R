
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

    sum_fun = function(x) c(mean(x), quantile(x, c(0.025, 0.975)))

    vec_df <- stats::aggregate(
        value ~ covariate + parameter,
        vector_samples,
        sum_fun
    )
    vec_df <- do.call(data.frame, vec_df)

    vec_df

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

    sum_fun <- function(x) c(mean(x), stats::quantile(x, c(0.025, 0.975)))

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
#' Creates a table of parameter estimates, e.g., Tables 1-3
#'
#' @param model_fit output from \code{\link{fit_model}}
#'
#' @return data.frame
make_estimate_table <- function(model_fit) {

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

    is_cow <- tab$species == 'SHCO'
    is_hyper <- tab$species == 'Host Community'

    row_order <- order(
        is_cow, is_hyper, tab$species, tab$model, tab$param, tab$level
    )

    tab[row_order, cols]

}


write_tables <- function(output){

    # load("mod1_ests.RData")
    # library(coda)
    # summary(output)
    # tab = data.frame(Mean = round(summary(output)$statistics[,1], 2),
    #                  round(summary(output)$quantiles[,c(1,5)], 2))
    # colnames(tab)[c(2,3)] = c('lower', 'upper')
    # xtable(tab)
    # write.csv("mod3_ests.csv", row.names = F)

    # load("mod2_ests.RData")
    # summary(output)
    # tab = data.frame(Mean = round(summary(output)$statistics[,1], 2),
    #                  round(summary(output)$quantiles[,c(1,5)], 2))
    # colnames(tab)[c(2,3)] = c('lower', 'upper')
    # xtable(tab)
    # write.csv(tab, "mod2_ests.csv", row.names = F)

    # I edited the rownames to be in latex/mathjax format,
    # then used online latex table converter.

    # load("mod3_ests.RData")
    # summary(output)
    # tab = data.frame(Mean = round(summary(output)$statistics[,1], 2),
    #                  round(summary(output)$quantiles[,c(1,5)], 2))
    # colnames(tab)[c(2,3)] = c('lower', 'upper')
    # xtable(tab)
    # write.csv(tab, "mod3_ests.csv", row.names = F)

    # I edited the rownames to be in latex/mathjax format,
    # then used online latex table converter.

    ##
    ##
    ## WAIC table
    ## 02/28/17
    ##
    ##

    # tab = read.csv(paste0(getwd(), "/tables/", "WAIC.table.csv"))
    # table = tab[,2:4]
    # rownames(table) = tab[,1]
    # library(xtable)
    # xtable(table)

}
