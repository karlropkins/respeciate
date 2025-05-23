% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rsp.find.R
\name{rsp.find}
\alias{rsp.find}
\alias{rsp_find_profile}
\alias{rsp_find}
\alias{rsp_find_species}
\alias{rsp_profile_info}
\alias{rsp_species_info}
\title{Information about data sets currently in respeciate}
\usage{
rsp_find_profile(
  ...,
  by = "keywords",
  partial = TRUE,
  source = "all",
  ref = NULL
)

rsp_profile_info(...)

rsp_find_species(
  ...,
  by = ".species",
  partial = TRUE,
  source = "all",
  ref = NULL
)

rsp_species_info(...)
}
\arguments{
\item{...}{character(s), any search term(s) to use when searching
the local respeciate archive for relevant records using
\code{rsp_find_profile} or \code{rsp_find_species}.}

\item{by}{character, the section of the archive to
search, by default \code{'keywords'} for \code{rsp_find_profile} and
\code{'.species'} for \code{sp_find_species}.}

\item{partial}{logical, if \code{TRUE} (default)
\code{rsp_find_profile} and \code{rsp_find_species} use partial matching.}

\item{source}{character, the data set to search: \code{'us'}
US EPA SPECIATE; \code{'eu'} JRC SPECIEUROPE; or, \code{'all'} (default)
both archives.}

\item{ref}{any \code{respeciate} object, \code{data.frame} or similar
that profile or species information can be extracted from.}
}
\value{
\code{rsp_profile_info} returns a \code{data.frame} of
profile information, as a \code{respeciate} object.
\code{rsp_species_info} returns a \code{data.frame} of
species information as a \code{respeciate} object.
}
\description{
Functions that provide respeciate
source information.
\code{rsp_find_profile} searches the currently installed respeciate
data sets for profile records.
\code{rsp_find species} searches the currently installed respeciate
data sets for species records.
}
\examples{
\dontrun{
profile <- "Ethanol"
pr <- rsp_find_profile(profile)
pr

species <- "Ethanol"
sp <- rsp_find_species(species)
sp}

}
\references{
For SPECIATE:

Simon, H., Beck, L., Bhave, P.V., Divita, F., Hsu, Y., Luecken, D.,
Mobley, J.D., Pouliot, G.A., Reff, A., Sarwar, G. and Strum, M., 2010.
The development and uses of EPA SPECIATE database.
Atmospheric Pollution Research, 1(4), pp.196-206.

For SPECIEUROPE:

Pernigotti, D., Belis, C.A., Spano, L., 2016. SPECIEUROPE: The
European data base for PM source profiles. Atmospheric Pollution Research,
7(2), pp.307-314. DOI: https://doi.org/10.1016/j.apr.2015.10.007
}
\seealso{
\code{\link{SPECIATE}} and \code{\link{SPECIEUROPE}}
}
