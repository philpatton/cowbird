% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calc_naive_state.R
\name{calc_naive_state_cowbird}
\alias{calc_naive_state_cowbird}
\title{Naive occurrence state for the cowbird}
\usage{
calc_naive_state_cowbird(detections)
}
\arguments{
\item{detections}{matrix of detections, with dimensions of
number of sites by number of visits (J x K)}
}
\value{
logical vector with length J. was the cowbird detected at the site?
}
\description{
Calculate the naive occurrence state for the cowbird, i.e., determine
if a cowbird was detected at a site
}
