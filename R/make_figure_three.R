
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
