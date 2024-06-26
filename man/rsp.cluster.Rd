% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rsp.cluster.R
\name{rsp.cluster}
\alias{rsp.cluster}
\alias{rsp_distance_profile}
\title{Profile cluster analysis methods}
\usage{
rsp_distance_profile(rsp, output = c("plot", "report"))
}
\arguments{
\item{rsp}{A \code{respeciate} object, a \code{data.frame} of respeciate
profiles.}

\item{output}{Character vector, required function output: \code{'report'} the
calculated distance matrix; \code{'plot'} a heat map of that distance
matrix.}
}
\value{
Depending on the \code{output} option, \code{sp_distance_profile} returns
one or more of the following: the correlation matrix, a heat map of the
correlation matrix.
}
\description{
Functions for studying similarities (or
dissimilarities) within respeciate data sets

\code{\link{rsp_distance_profile}} calculates the statistical distance
between respeciate profiles, and clusters profiles according to nearness.
}
\note{
Please note: function in development; structure and arguments may be
subject to change.
}
