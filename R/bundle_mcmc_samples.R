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
bundle_mcmc_samples <- function(model_fit) {

    # convert list of mcmc.arrays to list of regular arrays
    array_list <- lapply(model_fit, function(x) array(x, dim = dim(x)))

    # concatenate samples from different chains (chain is the last dimension)
    mcmc_samples <- lapply(
        array_list,
        function(x) apply(x, seq_len(length(dim(x)) -1), c)
    )

    names(mcmc_samples) <- paste0(names(mcmc_samples), '_samples')

    mcmc_samples

}

