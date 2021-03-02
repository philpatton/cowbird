#' Naive occurrence state for multiple species
#'
#' Calculate the naive occurrence state for multiple species, i.e., determine
#' if a species was detected at a site
#'
#' @param detections three dimensional array of detections, with dimensions of
#' number of sites by number of species by number of visits (J x N x K)
#'
#' @return logical matrix (J x N). was the species detected at the site?
multispecies_naive_state <- function(detections) {

    detection_count <- apply(detections, c(1, 2), sum, na.rm = T)
    naive_state <- detection_count > 0

    naive_state

}

#' Naive occurrence state for hosts
#'
#' Calculate the naive occurrence state for the hosts, i.e., determine
#' if a host was detected at a site
#'
#' @param detections three dimensional array of detections, with dimensions of
#' number of sites by number of hosts by number of visits (J x N x K)
#'
#' @return logical matrix (J x N). was the host detected at the site?
calc_naive_state_host <- function(detections) {

    detection_count <- apply(detections, c(1, 2), sum, na.rm = T)
    naive_state <- detection_count > 0

    naive_state

}

#' Naive occurrence state for the cowbird
#'
#' Calculate the naive occurrence state for the cowbird, i.e., determine
#' if a cowbird was detected at a site
#'
#' @param detections matrix of detections, with dimensions of
#' number of sites by number of visits (J x K)
#'
#' @return logical vector with length J. was the cowbird detected at the site?

calc_naive_state_cowbird <- function(detections) {

    detection_count <- apply(detections, 1, sum, na.rm = T)
    naive_state <- detection_count > 0

    naive_state

}
