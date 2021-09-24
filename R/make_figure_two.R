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
