% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rsp.cor.R
\name{rsp.cor}
\alias{rsp.cor}
\alias{rsp_cor_species}
\title{Species correlations}
\usage{
rsp_cor_species(
  rsp,
  min.n = 3,
  cols = c("#80FFFF", "#FFFFFF", "#FF80FF"),
  na.col = "#CFCFCF",
  heatmap.args = TRUE,
  key.args = TRUE,
  report = "silent"
)
}
\arguments{
\item{rsp}{\code{respeciate} object, a \code{data.frame} of respeciate
profiles.}

\item{min.n}{\code{numeric} (default 3), the minimum number of species measurements
needed in a profile for the function to use it in correlation calculations.
Here, it should be noted that this does not guarantee the three matched
pairs of measurements needed to calculate a correlation coefficient because
not all profiles contain all species, so there may still be insufficient
overlap on a case-by-case basis.}

\item{cols}{a series of \code{numeric}, \code{character} or other class values
that can be translated into a color gradient, used to color valid cases when
generating plots and color keys, default \code{c("#80FFFF", "#FFFFFF", "#FF80FF")}
equivalent to \code{\link{cm.colors}} output.}

\item{na.col}{\code{numeric}, \code{character} or other class that can be
translated into a single color, used to color \code{NA}s when generating
plots and color keys, default grey \code{"#CFCFCF"}.}

\item{heatmap.args}{\code{logical} or \code{list}, heat map settings. Options
include \code{TRUE} (default) to generate the heat map without modification;
\code{FALSE} to not plot it; or a list of heat map options to alter the plot
default appearance. The plot, a standard heat map with the dendrograms
removed, is generated using \code{\link[stats]{heatmap}}, so see associated
documentation for valid options.}

\item{key.args}{\code{logical} or \code{list}, color key settings if plotting
the correlation matrix heat map. Options include \code{TRUE} (default) to
generate the key without modification; \code{FALSE} to not include the key;
or a list of options to alter the key appearance.}

\item{report}{\code{logical} or \code{character}, the required function
output. Options include: \code{'silent'} (default), to return the
correlation matrix invisibly; \code{TRUE} to return the matrix
(visibly); and, \code{FALSE} to not return it.}
}
\value{
By default \code{rsp_cor_species} invisibly returns the calculated
correlation matrix a plots it as a heat map, but arguments including
\code{heatmap} and \code{report} can be used to modify function outputs.
}
\description{
Functions for studying relationships between
species in respeciate data sets.

\code{\link{rsp_cor_species}} generates a by-species correlation
matrix of the supplied respeciate data sets.
}
