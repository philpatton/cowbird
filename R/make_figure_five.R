

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
