

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
