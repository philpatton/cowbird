#' Get the abbreviated names of the host species
#'
#' Get a vector of abbreviated names of the host species
#'
#' @return vector. names of the host species
#'
get_host_names <- function() c("ADWA", "BWVI", "PROR", "PRVI")

#' Get the full names of the host species
#'
#' Get a vector of names of the host species
#'
#' @return vector. names of the host species
#'
get_host_full_name <- function() {
    host_names <- c(
        "Adelaides Warbler",
        "Black-whiskered Vireo",
        "Puerto Rican Oriole",
        "Puerto Rican Vireo"
    )

    host_names

}
#' Get the names of the habitats
#'
#' Get a vector of names of the habitats
#'
#' @return vector. names of the habitats
#'
get_habitat_names <- function() c("Forest", "Shade", 'Sun')
