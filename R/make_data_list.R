#' Create matrix of cowbird detections
#'
#' Create a matrix of cowbird detections for the data list
#'
#' @param cowbird_data data.frame, from \code{data('cowbird_data')}
#'
#'
#' @return matrix of cowbird detections
get_y <- function(cowbird_data){

    cowbird <- cowbird_data[cowbird_data$species == 'SHCO', ]

    Y <- reshape2::acast(
        cowbird,
        site_index ~ visit,
        value.var = 'detection'
    )

    Y

}

#' Create matrix of host detections
#'
#' Create a matrix of host detections for the data list
#'
#' @param cowbird_data data.frame, from \code{data('cowbird_data')}
#'
#'
#' @return matrix of host detections
get_x <- function(cowbird_data){

    hosts <- cowbird_data[cowbird_data$species != 'SHCO', ]

    X <- reshape2::acast(
        hosts,
        site_index ~ species ~ visit,
        value.var = 'detection'
    )

    X

}

#' Create matrix of visit covariates affecting the hosts
#'
#' Create a matrix of matrix of visit covariates for the data list
#'
#' @param cowbird_data data.frame, from \code{data('cowbird_data')}
#'
#'
#' @return matrix of visit covariates affecting the hosts
get_o <- function(cowbird_data){

    cowbird <- cowbird_data[cowbird_data$species == 'SHCO', ]

    observer <- reshape2::acast(
        cowbird,
        site_index ~ visit,
        value.var = 'observer_b'
    )

    missing_visit <- is.na(observer)
    n_missing_visit <- sum(missing_visit)
    observer[missing_visit] <- stats::rbinom(n_missing_visit, 1, 0.5)

    O <- array(dim = c(2, dim(observer)))

    O[1, , ] <- 1
    O[2, , ] <- observer

    dimnames(O) <- list(
        variable = c('intercept', 'observer'),
        site = dimnames(observer)[[1]],
        visit = dimnames(observer)[[2]]
    )

    O

}

#' Create matrix of site covariates affecting the hosts
#'
#' Create a matrix of matrix of site covariates for the data list
#'
#' @param cowbird_data data.frame, from \code{data('cowbird_data')}
#'
#' @return matrix of site covariates affecting the hosts
get_w <- function(cowbird_data){

    sites <- unique(cowbird_data[c('site_index', 'land_use')])

    coffee_shade <- ifelse(sites$land_use == 'coffee_shade', 1, 0)
    coffee_sun <- ifelse(sites$land_use == 'coffee_sun', 1, 0)

    W <- array(dim = c(nrow(sites), 3))

    W[, 1] <- 1
    W[, 2] <- coffee_shade
    W[, 3] <- coffee_sun

    dimnames(W) <- list(
        site = sites$site_index,
        variable = c('intercept', 'coffee_shade', 'coffee_sun')
    )

    W

}

#' Create matrix of site covariates affecting the cowbird
#'
#' Create a matrix of matrix of site covariates for the data list
#'
#' @param cowbird_data data.frame, from \code{data('cowbird_data')}
#'
#' @return matrix of site covariates affecting the hosts
get_v <- function(cowbird_data){

    sites <- unique(cowbird_data[c('site_index', 'land_use')])

    is_coffee <- grepl('coffee', sites$land_use)
    coffee <- ifelse(is_coffee, 1, 0)

    V <- array(dim = c(nrow(sites), 2))

    V[, 1] <- 1
    V[, 2] <- coffee

    dimnames(V) <- list(
        site = sites$site_index,
        variable = c('intercept', 'coffee')
    )

    V

}

#' Make a list of data for easy model fitting
#'
#' This makes a list that can be easily used in the \code{fit_model} function
#'
#' @param cowbird_data data.frame, loaded from
#' \code{data(cowbird_data)}
#' @return list of data
#' @export
make_data_list <- function(cowbird_data) {

    x <- get_x (cowbird_data)
    y <- get_y(cowbird_data)

    o <- get_o(cowbird_data)
    u <- o

    w <- get_w(cowbird_data)
    v <- get_v(cowbird_data)

    # number of site covariates for cowbird and hosts, respectively
    n_w <- dim(w)[2]
    n_v <- dim(v)[2]

    # number of visit covariates for cowbird and hosts, respectively
    n_o <- dim(o)[1]
    n_u <- dim(u)[1]

    # number of sites, hosts, and visits
    n_site = dim(x)[1]
    n_host = dim(x)[2]
    n_visit = dim(x)[3]

    data_list <- list(
        X = x,
        Y = y,
        O = o,
        U = u,
        W = w,
        V = v,
        n_w = n_w,
        n_v = n_v,
        n_o = n_o,
        n_u = n_u,
        n_site = n_site,
        n_host = n_host,
        n_visit = n_visit
    )

    data_list

}
