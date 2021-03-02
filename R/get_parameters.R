#' Generates vector of parameter needed to do posterior predictive checks
#'
#' Generates a vector of names of parameters that are used to calculate Bayesian
#' p-values
#'
#' @return character vector of parameter names
#'
#' @export
get_ppc_parameters <- function() {

    params <- c(
        'X.new',
        'Y.new'
    )

    params

}

#' Generates vector of parameter needed to evaluate models
#'
#' @param checking_params logical. should 'X.new' and 'Y.new', the parameters
#' used in posterior predictive checking, be included?
#'
#' @return character vector of parameter names
#'
#' @export
get_evaluation_parameters <- function(checking_params = FALSE) {

    params <- c(
        'eta',
        'zeta',
        'psi',
        'theta'
    )

    if (checking_params) params <- c(params, get_ppc_parameters())

    params

}

#' Generates vector of parameter needed to evaluate models
#'
#' @return character vector of parameter names
#'
#' @export
get_hyper_parameters <- function() {

    params <- c(
        'mu.c',
        'sd.c',
        'mu.d',
        'sd.d',
        'a',
        'b',
        'c',
        'd'
    )

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
get_predicition_params <- function() {

    params <- c(
        'coc.for',
        'coc.sha',
        'coc.sun',
        'nrfo',
        'nrsh',
        'nrsu',
        'nsfo',
        'nssh',
        'nssu'
    )

    params

}
