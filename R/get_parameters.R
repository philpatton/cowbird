#' Generates vector of parameter needed to do posterior predictive checks
#'
#' Generates a vector of names of parameters that are used to calculate Bayesian
#' p-values
#'
#' @return character vector of parameter names
#'
#' @export
get_checking_parameters <- function() {

    params <- c(
        'X.new',
        'Y.new'
    )

    params

}

#' Generates vector of parameter needed to evaluate models
#'
#' @param checking_parameters logical. should 'X.new' and 'Y.new', the
#' parameters used in posterior predictive checking, be included?
#'
#' @return character vector of parameter names
#'
#' @export
get_evaluation_parameters <- function(checking_parameters = FALSE) {

    params <- c(
        'eta',
        'zeta',
        'psi',
        'theta'
    )

    if (checking_parameters) params <- c(params, get_checking_parameters())

    params

}

#' Generates vector of of the hyper parameters
#'
#' @return character vector of parameter names
#'
#' @export
get_hyper_parameters <- function() {

    params <- c(
        'mu_c',
        'sd_c',
        'mu_d',
        'sd_d',
        'a',
        'b',
        'c',
        'd'
    )

    params

}

#' Generates vector of parameters in the Table 1
#'
#' @return character vector of parameter names
#'
#' @export
get_table1_parameters <- function() {

    params <- get_hyper_parameters()

    params

}

#' Generates vector of parameters in the Table 2
#'
#' @return character vector of parameter names
#'
#' @export
get_table2_parameters <- function() {

    params <- c(get_hyper_parameters(), 'kappa')

    params

}

#' Generates vector of parameters in the Table 3
#'
#' @return character vector of parameter names
#'
#' @export
get_table3_parameters <- function() {

    params <- c(get_hyper_parameters(), 'e', 'rho')

    params

}

#' Generates vector of parameter used in model predictions
#'
#' Generates a vector of names of parameters that are used to in the predictions
#' of co-occurrence, number of co-occurring sites, etc.
#'
#' @return character vector of parameter names
#'
#' @export
get_predicition_parameters <- function() {

    params <- c(
        'coc.for',
        'coc.sha',
        'coc.sun',
        'n_both_for',
        'n_both_sha',
        'n_both_sun',
        'n_only_for',
        'n_only_sha',
        'n_only_sun'
    )

    params

}

#' Generates vector of the primary parameters
#'
#' Generates a vector of names of parameters, including hyper parameters,
#' cowbird parameters, prediction parameters, and model evaluation parameters.
#'
#' @return character vector of parameter names
#'
#' @export
get_all_parameters <- function() {

    params <- c(
        get_table3_parameters(),
        'kappa',
        get_evaluation_parameters(checking_parameters = TRUE),
        get_predicition_parameters()
    )

    params

}
