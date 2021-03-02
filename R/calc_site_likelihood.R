#' Calculate the likelihood of the detections at a site
#'
#' Calculates the likelihood of a detection history given parameter values
#' for occupancy and detection
#'
#' This calculates the likelihood formula,
#' \deqn{
#' I * (\psi \prod p^y * (1 - p)^{1 - y}) + (1 - I) * ((1 - \psi) + \psi \prod (1 - p))
#' }
#'
#'
#' @param pr_detection vector of detection probabilities, with length equal to
#' the number of visits
#' @param detection_history vector of detection history, with length equal to
#' the number visits
#' @param pr_occupancy numeric. the probability of occurrence at the site
#' @param detected_at_site logical. was the species detected at the site?
#'
#' @return numeric. likelihood of the detection data given the parameters
#'
#' @seealso See Appendix S1 in \href{https://esajournals.onlinelibrary.wiley.com/doi/abs/10.1890/15-1471.1}{Broms, Hooten, and Fitzpatrick 2016}
#' for information about the formula
#' @export
calc_site_likelihood <- function(pr_detection, detection_history, pr_occupancy,
                                 detected_at_site) {

    if (detected_at_site) {

        pr_detected <- pr_detection ^ detection_history
        pr_not_detected <- (1 - pr_detection) ^ (1 - detection_history)

        pr_detection_history_vec <- pr_detected * pr_not_detected

        pr_detection_history <- prod(pr_detection_history_vec, na.rm = T)

        site_likelihood <- pr_occupancy * pr_detection_history

    } else {

        pr_not_detected <- prod(1 - pr_detection, na.rm = TRUE)

        site_likelihood <- (1 - pr_occupancy) + pr_occupancy * pr_not_detected

    }

    site_likelihood

}
