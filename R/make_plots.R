#' Generate data.frame used in \code{\link{plot_ppc}}
#'
#' Generate the data.frame used in the posterior predictive checking plot.
#' This data.frame is used to generate the annotations of the observed number
#' as well as the p-values.
#'
#' @param ppc_results output from \code{\link{check_model}}
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
#' @param ppc_results output from \code{\link{check_model}}
#' @param model character. specifies which model is being checked.
#'
#' @return ggplot2 object
#'
#' @export
plot_ppc_individual <- function(ppc_results,
                                model = c('Model 1', 'Model 2', 'Model 3' )) {

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
        ggplot2::geom_histogram(
            fill = 'deepskyblue',
            colour = "white",
            binwidth = 2
        ) +
        ggplot2::facet_grid(Host ~ .) +
        ggplot2::ggtitle(model) +
        ggplot2::geom_vline(
            ggplot2::aes(xintercept = observed_value),
            data = annos,
            col = "darkorange"
        ) +
        ggplot2::geom_text(
            ggplot2::aes(
                x,
                y,
                label = paste0(pvals)),
            data = annos,
            inherit.aes = F
        ) +
        ggplot2::theme(
            axis.text.y = ggplot2::element_blank(),
            axis.ticks.y = ggplot2::element_blank(),
            axis.title.y = ggplot2::element_blank(),
            strip.background = ggplot2::element_rect(
                fill = 'white',
                colour = 'black'
            ),
            panel.grid.major = ggplot2::element_blank(),
            panel.grid.minor = ggplot2::element_blank(),
            panel.background = ggplot2::element_blank()
        )

}

#' Generate Figure 2
#'
#' Generate plot of used in posterior predictive checking
#'
#' @param ppc_list output from \code{\link{check_all_models}}
#'
#' @return gridExtra grob
#'
#' @export
make_figure_two <- function(ppc_list) {

    p1 <- plot_ppc_individual(ppc_list$fit1, 'Model 1')
    p2 <- plot_ppc_individual(ppc_list$fit2, 'Model 2')
    p3 <- plot_ppc_individual(ppc_list$fit3, 'Model 3')

    gridExtra::grid.arrange(p1, p2, p3, ncol = 3)

}

#' Save Figure 2
#'
#' Save the plot to a PDF
#'
#' @param ppc output from \code{\link{plot_ppc}}
#' @param plot_file character. specifies file to save to.
#'
#' @export
save_ppc <- function(ppc, plot_file){

    grDevices::pdf(file = plot_file, height = 4, width = 7)
    plot(ppc)
    grDevices::dev.off()

}

#' Generate co-occurrence plot data.frame
#'
#' Generate the data.frame used in the co-occurrence plot
#'
#' @param model_fit output from \code{\link{fit_model}}
#'
#' @return data.frame
#'
#' @export
cooccur_probs <- function(model_fit) {

    samples <- bundle_mcmc_samples(model_fit)

    sample_summary <- summarize_mcmc_samples(samples)

    plot_df <- reshape2::melt(sample_summary)
    names(plot_df) <- c('host', 'measure', 'value', 'parameter')

    plot_df <- plot_df[grep('coc', plot_df$parameter), ]

    host_names <- get_host_names()
    plot_df$host <- host_names[plot_df$host]

    habitat_id <- substr(plot_df$parameter, 5, 7)
    plot_df$habitat <- ifelse(
        habitat_id == 'for',
        'Forest',
        ifelse(habitat_id == 'sha', 'Shade', 'Sun')
    )

    plot_df <- reshape2::dcast(plot_df, host + parameter + habitat ~ measure)

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
#'
#' @export
make_figure_three <- function(model_fit) {

    has_preds <- all(get_predicition_parameters() %in% names(model_fit))

    if (!has_preds) {
        stop('model_fit must have samples for the prediction parameters')
    }

    plot_df <- cooccur_probs(model_fit)

    p <- ggplot2::ggplot(
            plot_df,
            ggplot2::aes(x = habitat, y = avg)
        ) +
        ggplot2::geom_point() +
        ggplot2::facet_grid(host ~ .) +
        ggplot2::geom_errorbar(
            ggplot2::aes(ymin = lower, ymax = upper),
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

#' Save Figure 3
#'
#' Save the plot to a PDF
#'
#' @param occurrence output from \code{\link{plot_cooccur_probs}}
#' @param plot_file character. specifies file to save to.
#'
#' @export
save_figure_three <- function(occurrence, plot_file) {
    grDevices::pdf(file = plot_file, width = 2.3, height = 4, pointsize = 9)
    plot(occurrence)
    grDevices::dev.off()
}

#' Make data.frame used in figure four
#'
#' Make data.frame used in figure four
#'
#' @param model_fit output from \code{\link{fit_model}}
#'
#' @return data.frame
#'
#' @export
sites_with_both <- function(model_fit) {

    samples <- model_fit[grep('n_both_', names(model_fit))]

    samples <- bundle_mcmc_samples(samples)

    sites_with_both <- reshape2::melt(
        samples,
        varnames = c('host', 'iteration'),
        value.name = 'Number of Sites'
    )
    names(sites_with_both)[names(sites_with_both) == 'L1'] <- 'parameter'

    host_names <- get_host_names()

    sites_with_both$host <- host_names[sites_with_both$host]

    habitat_id <- substr(sites_with_both$parameter, 8, 10)
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
#' @export
make_figure_four <- function(model_fit) {

    plot_df <- sites_with_both(model_fit)

    p <- ggplot2::ggplot(
            plot_df,
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
#' @param sites_with_both output from \code{\link{plot_sites_with_both}}
#' @param plot_file character. specifies file to save to.
#'
#' @export
save_figure_four <- function(sites_with_both, plot_file) {

    grDevices::pdf(file = plot_file,
        width = 2.75, height = 2.75, pointsize=9)
    plot(sites_with_both)
    grDevices::dev.off()

}

#' Make data.frame used in figure five
#'
#' Make data.frame used in figure five
#'
#' @param model_fit output from \code{\link{fit_model}}
#'
#' @return data.frame
just_host <- function(model_fit) {

    samples <- bundle_mcmc_samples(model_fit)

    samples <- samples[grep('n_only_', names(samples))]

    just_host <- reshape2::melt(
        samples,
        varnames = c('host', 'iteration'),
        value.name = 'Number of Sites'
    )
    names(just_host)[names(just_host) == 'L1'] <- 'parameter'

    host_names <- get_host_names()

    just_host$host <- host_names[just_host$host]

    habitat_id <- substr(just_host$parameter, 8, 10)
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
#'
#' @export
make_figure_five <- function(model_fit) {

    plot_df <- just_host(model_fit)

    p <- ggplot2::ggplot(
        plot_df,
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
#' @param just_host output from \code{\link{plot_cooccur_probs}}
#' @param plot_file character. specifies file to save to.
#'
#' @export
save_figure_five <- function(just_host, plot_file) {

    grDevices::pdf(file = plot_file,
        width = 2.75, height = 2.75, pointsize = 9)
    plot(just_host)
    grDevices::dev.off()

}
