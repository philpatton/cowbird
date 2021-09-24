#' Function to estimate the WAIC for all three models model
#'
#' WAIC is used to estimate the parsimony of each model.
#'
#' @param data_list list. output from \code{make_data_list} function.
#' @param fit_list list. output from \code{fit_all_models} function.
#'
#' @return data.frame of estimates for WAIC, elppd, and pdWAIC
#'
#' @export
evaluate_all_models = function(fit_list, data_list) {

    waic <- lapply(fit_list, evaluate_model, data_list)

    waic <- do.call(rbind, waic)

    waic <- cbind(model = c('Model 1', 'Model 2', 'Model 3'), waic)
    rownames(waic) <- NULL

    waic

}
