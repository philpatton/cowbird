#' Generate co-occurrence plot data.frame
#'
#' Generate the data.frame used in the co-occurrence plot
#'
#' @param model_fit output from \code{\link{fit_model}}
#'
#' @return data.frame
make_cooccur_plot_df <- function(model_fit) {

    model_fit <- bundle_pred_samples(model_fit)

    average <- sapply(model_fit, function(x) apply(x, 1, mean))
    average <- reshape2::melt(
        average,
        varnames = c('host', 'parameter'),
        value.name = 'mean'
    )

    lower_quantile <- sapply(
        model_fit,
        function(x) apply(x, 1, stats::quantile, 0.025)
    )
    lower_quantile <- reshape2::melt(
        lower_quantile,
        varnames = c('host', 'parameter'),
        value.name ='lower_quantile'
    )

    upper_quantile <- sapply(
        model_fit,
        function(x) apply(x, 1, stats::quantile, 0.975)
    )
    upper_quantile <- reshape2::melt(
        upper_quantile,
        varnames = c('host', 'parameter'),
        value.name ='upper_quantile'
    )

    plot_df <- merge(average, merge(lower_quantile, upper_quantile))
    plot_df <- plot_df[grep('coc', plot_df$parameter), ]

    host_names <- get_host_names()
    plot_df$host <- host_names[plot_df$host]

    habitat_id <- substr(plot_df$parameter, 5, 7)
    plot_df$habitat <- ifelse(
        habitat_id == 'for',
        'Forest',
        ifelse(habitat_id == 'sha', 'Shade', 'Sun')
    )

    rownames(plot_df) = NULL

    plot_df

}

#' Generate Figure 3
#'
#' Generate plot of co-occurrence probabilities between each hosts and
#' the Shiny Cowbird
#'
#' @param model_fit output from \code{\link{fit_model}}
#'
#' @return ggplot2 object
plot_cooccur_probs <- function(model_fit) {

    cooccur_plot_df <- make_cooccur_plot_df(model_fit)

    p <- ggplot2::ggplot(
            cooccur_plot_df,
            ggplot2::aes(x = habitat, y = mean)
        ) +
        ggplot2::geom_point() +
        ggplot2::facet_grid(host ~ .) +
        ggplot2::geom_errorbar(
            ggplot2::aes(ymin = lower_quantile, ymax = upper_quantile),
            width = 0
        ) +
        ggplot2::ylab("Co-occurrence Probability") +
        ggplot2::xlab("Habitat") +
        ggplot2::ggtitle(
            "Probability of host parasite\n co-occurrence by habitat"
        ) +
        ggplot2::theme_bw() +
        ggplot2::scale_y_continuous(breaks = c(0.0, 0.3, 0.6, 0.9)) +
        ggplot2::theme(
            plot.title = ggplot2::element_text(size = 10),
            axis.text.x = ggplot2::element_text(size = 10),
            strip.text = ggplot2::element_text(size=10),
            axis.title = ggplot2::element_text(size=10),
            panel.grid.major = ggplot2::element_blank(),
            panel.grid.minor = ggplot2::element_blank())

    p

}

#' Save Figure 2
#'
#' Save the plot to a PDF
#'
#' @param plot output from \code{\link{plot_cooccur_probs}}
#' @param plot_file character. specifies file to save to.
#'
save_occurrence_plot <- function(plot, plot_file) {
    grDevices::pdf(file = plot_file, width = 2.3, height = 4, pointsize = 9)
    plot
    grDevices::dev.off()
}

#' Generate co-occurrence plot data.frame
#'
#' Generate the data.frame used in the co-occurrence plot
#'
#' @param model_fit output from \code{\link{fit_model}}
#'
#' @return data.frame
make_sites_with_both_df <- function(model_fit) {

    samples <- bundle_pred_samples(model_fit)

    samples <- samples[grep('n_both_', names(samples))]

    sites_with_both <- reshape2::melt(
        samples,
        varnames = c('host', 'iteration'),
        value.name = 'Number of Sites'
    )
    names(sites_with_both)[names(sites_with_both) == 'L1'] <- 'parameter'

    host_names <- get_host_names()

    sites_with_both$host <- host_names[sites_with_both$host]

    habitat_id <- substr(sites_with_both$parameter, 3, 4)
    sites_with_both$habitat <- ifelse(
        habitat_id == 'for',
        'Forest',
        ifelse(habitat_id == 'sha', 'Shade', 'Sun')
    )

    sites_with_both

}

#' Generate Figure 4
#'
#' Generate plot of predicted number of sites with both the host and the
#' parasite by host and land-use.
#'
#' @param model_fit output from \code{\link{fit_model}}
#'
#' @return ggplot2 object
plot_sites_with_both <- function(model_fit) {

    sites_with_both <- make_sites_with_both_df(model_fit)

    p <- ggplot2::ggplot(
            sites_with_both,
            ggplot2::aes(x=`Number of Sites`, y = ..density..)
        ) +
        ggplot2::geom_histogram(binwidth = 1) +
        ggplot2::facet_grid(habitat ~ host, scales = "free_y") +
        ggplot2::ggtitle("Predicted Number of Sites Where\n Host and Parasite Co-occur") +
        ggplot2::theme_bw() +
        ggplot2::scale_x_continuous(breaks = c(0, 20, 40)) +
        ggplot2::theme(
            plot.title = ggplot2::element_text(size=10),
            axis.text.x = ggplot2::element_text(size=9),
            axis.text.y = ggplot2::element_blank(),
            axis.ticks.y = ggplot2::element_blank(),
            strip.text = ggplot2::element_text(size=8),
            axis.title = ggplot2::element_text(size=9),
            panel.grid.major = ggplot2::element_blank(),
            panel.grid.minor = ggplot2::element_blank()
        )

    p

}

#' Save Figure 4
#'
#' Save the plot to a PDF
#'
#' @param plot output from \code{\link{plot_sites_with_both}}
#' @param plot_file character. specifies file to save to.
#'
save_sites_with_both <- function(plot, plot_file) {

    grDevices::pdf(file = plot_file,
        width = 2.75, height = 2.75, pointsize=9)
    plot
    grDevices::dev.off()
}

#' Generate co-occurrence plot data.frame
#'
#' Generate the data.frame used in the co-occurrence plot
#'
#' @param model_fit output from \code{\link{fit_model}}
#'
#' @return data.frame
make_just_host_df <- function(model_fit) {

    samples <- bundle_pred_samples(model_fit)

    samples <- samples[grep('n_only_', names(samples))]

    just_host <- reshape2::melt(
        samples,
        varnames = c('host', 'iteration'),
        value.name = 'Number of Sites'
    )
    names(just_host)[names(just_host) == 'L1'] <- 'parameter'

    host_names <- get_host_names()

    just_host$host <- host_names[just_host$host]

    habitat_id <- substr(just_host$parameter, 3, 4)
    just_host$habitat <- ifelse(
        habitat_id == 'for',
        'Forest',
        ifelse(habitat_id == 'sha', 'Shade', 'Sun')
    )

    just_host

}

#' Generate Figure 5
#'
#' Generate plot of predicted number of sites where the host occurs without
#' the parasite
#'
#' @param model_fit output from \code{\link{fit_model}}
#'
#' @return ggplot2 object
plot_just_hosts <- function(model_fit) {

    just_host <- make_just_host_df(model_fit)

    p <- ggplot2::ggplot(
        just_host,
        ggplot2::aes(x=`Number of Sites`, y = ..density..)
    ) +
        ggplot2::geom_histogram(binwidth = 1) +
        ggplot2::facet_grid(habitat ~ host, scales = "free_y") +
        ggplot2::ggtitle("Predicted Number of Sites where\n Host Occurs without Parasite") +
        ggplot2::theme_bw() +
        ggplot2::scale_x_continuous(breaks = c(0, 20, 40)) +
        ggplot2::theme(
            plot.title = ggplot2::element_text(size=10),
            axis.text.x = ggplot2::element_text(size=9),
            axis.text.y = ggplot2::element_blank(),
            axis.ticks.y = ggplot2::element_blank(),
            strip.text = ggplot2::element_text(size=8),
            axis.title = ggplot2::element_text(size=9),
            panel.grid.major = ggplot2::element_blank(),
            panel.grid.minor = ggplot2::element_blank()
        )

    p

}

#' Save Figure 5
#'
#' Save the plot to a PDF
#'
#' @param plot output from \code{\link{plot_cooccur_probs}}
#' @param plot_file character. specifies file to save to.
#'
save_just_hosts <- function(plot, plot_file) {

    grDevices::pdf(file = plot_file,
        width = 2.75, height = 2.75, pointsize = 9)
    plot
    grDevices::dev.off()

}

#' Generate data.frame used in \code{\link{plot_ppc}}
#'
#' Generate the data.frame used in the posterior predictive checking plot.
#' This data.frame is used to generate the annotations of the observed number
#' as well as the p-values.
#'
#' @param ppc_results output from \code{\link{posterior_predictive_check}}
#'
#' @return data.frame
make_annotation_df <- function(ppc_results) {

    # annotations
    annos <- data.frame(
        x = 60,
        y = 0.1,
        Host = names(ppc_results$naive_cooccur_observed),
        observed_value = ppc_results$naive_cooccur_observed,
        pvals = round(ppc_results$pvals, 2)
    )

    annos

}

#' Generate posterior predictive checking plots
#'
#' Generate subplots used to generate
#'
#' @param ppc_results output from \code{\link{posterior_predictive_check}}
#' @param model character. specifies which model is being checked.
#'
#' @return ggplot2 object
plot_ppc <- function(ppc_results, model = c('Model 1', 'Model 2', 'Model 3' )) {

    preds <- reshape2::melt(
        ppc_results$naive_cooccur_predicted,
        varnames = c('Host', 'Iteration'),
        value.name = 'Number of Sites'
    )

    host <- get_host_names()

    preds$Host <- host[preds$Host]
    annos <- make_annotation_df(ppc_results)

    ppc_plot <- ggplot2::ggplot(
            preds,
            ggplot2::aes(x = `Number of Sites`, y = ..density..)
        ) +
        ggplot2::geom_histogram(fill="white", colour="black", binwidth = 2) +
        ggplot2::facet_grid(Host ~ .) +
        ggplot2::geom_density() +
        ggplot2::ggtitle(model) +
        ggplot2::geom_vline(
            ggplot2::aes(xintercept = observed_value),
            data = annos,
            col = "red"
        ) +
        ggplot2::geom_text(
            ggplot2::aes(
                x,
                y,
                label = paste0("p=", pvals)),
            data = annos,
            inherit.aes = F
        ) +
        ggplot2::theme(
            panel.grid.major = ggplot2::element_blank(),
            panel.grid.minor = ggplot2::element_blank()
        )

}

#' Generate Figure 2
#'
#' Generate plot of used in posterior predictive checking
#'
#' @param ppc_list output from \code{\link{check_all_models}}
#'
#' @return gridExtra grob
plot_all_ppc <- function(ppc_list) {

    p1 <- plot_ppc(ppc_list$model1_ppc, 'Model 1')
    p2 <- plot_ppc(ppc_list$model2_ppc, 'Model 2')
    p3 <- plot_ppc(ppc_list$model3_ppc, 'Model 3')

    gridExtra::grid.arrange(p1, p2, p3, ncol = 3)

}

#' Save Figure 2
#'
#' Save the plot to a PDF
#'
#' @param plot output from \code{\link{plot_all_ppc}}
#' @param plot_file character. specifies file to save to.
#'
save_ppc <- function(plot, plot_file){

    grDevices::pdf(file = plot_file, height = 4, width = 7)
    plot
    grDevices::dev.off()

}
