
#' Tabulate results of posterior predictive check
#'
#' Create a table of p-values using the results of the posterior predictive
#' checks
#'
#' @param ppc_results output from \code{\link{check_model}}
#'
#' @return matrix of p values
#'
#' @export
ppc_table <- function(ppc_results) {

    ppc_tab <- lapply(ppc_results, function(x) `$`(x, 'pvals'))
    ppc_tab <- do.call(rbind, ppc_tab)

    colnames(ppc_tab) <- get_host_names()

    ppc_tab

}
