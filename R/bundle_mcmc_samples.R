select_last <- function(x) {
    # modified from https://stackoverflow.com/a/14502298 by PTP 2020-03-04
    #
    # Create list representing arguments supplied to [
    # bquote() creates an object corresponding to a missing argument
    max_dim <- length(dim(x))
    indices <- rep(list(bquote()), max_dim)
    indices[[max_dim]] <- 1

    # Generate the call to [
    call <- as.call(c(
        list(as.name("["), quote(x)),
        indices
    ))

    # Finally, evaluate it
    eval(call)
}

#' Function to bundles mcmc samples into a list
#'
#' This function is used to make manipulation of the mcmc samples easier. The
#' default output from \code{rjags::jags.samples} is less predictable than
#' a standard array.
#'
#' This function also concatenates the MCMC chains into one array.
#'
#' @param model_fit list. output from \code{fit_model()}
#'
#' @return array
#'
#' @export
bundle_mcmc_samples <- function(model_fit) {

    # convert list of mcmc.arrays to list of regular arrays
    array_list <- lapply(model_fit, function(x) array(x, dim = dim(x)))

    mcmc_samples <- lapply(array_list, select_last)

    names(mcmc_samples) <- paste0(names(mcmc_samples), '_samples')

    mcmc_samples

}

