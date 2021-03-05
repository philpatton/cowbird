
make_diagnostic <- function(model_fit) {

    params <- c('a', 'b', 'mu_c', 'sd_c', 'mu_d', 'sd_d')

    model_fit <- model_fit[names(model_fit) %in% params]

    coda_list <- lapply(model_fit, coda::as.mcmc.list)

    diagnostic <- lapply(coda_list, ggmcmc::ggs)

    diagnostic

}
