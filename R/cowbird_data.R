#' Detection data for the Shiny Cowbird and its Puerto Rican Hosts
#'
#' A dataset containing detection / non-detection data for the invasive Shiny
#' Cowbird and four Puerto Rican hosts. The dataset was collected in the Spring
#' of 2015 in the highlands of Puerto Rico in three land-uses: forests,
#' shaded coffee plantations, and sun coffee plantations. Each site was visited
#' three times, save for five sites that were visited twice.
#'
#' @format A data frame with 1785 rows and 6 variables:
#' \describe{
#'   \item{site_index}{id for each site}
#'   \item{land_use}{land-use at each site}
#'   \item{visit}{id for each visit to a site}
#'   \item{observer_b}{indicator variable for observer}
#'   \item{species}{species detected}
#'   \item{detection}{binary, indicating detection}
#' }
#' @source \url{http://www.lib.ncsu.edu/resolver/1840.20/33237}
"cowbird_data"
