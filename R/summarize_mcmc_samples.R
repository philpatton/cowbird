
summarize_mcmc_samples <- function(samples) {

    sum_fun <- function(x) {
        cbind(
            avg = apply(x, 1, mean),
            lower = apply(x, 1, stats::quantile, 0.025),
            upper = apply(x, 1, stats::quantile, 0.975)
        )
    }

    sample_summary <- lapply(samples, sum_fun)

    sample_summary

}
