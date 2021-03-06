% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_EcoCounter_location.R
\name{get_EcoCounter_location}
\alias{get_EcoCounter_location}
\title{Get location from an EcoCounter installation}
\usage{
get_EcoCounter_location(EcoCounterId)
}
\arguments{
\item{EcoCounterId}{\link{character} or \link{integer} or \link{vector} of \link{character} or \href{**required**}{integer}:
Id of the EcoCounter installation}
}
\value{
\link{data.frame} with parsed data from EcoCounter API.
Coloumns: longitude, lattitude, name, id and idParent.
This can be used as input in \link[leaflet:leaflet]{leaflet::leaflet}, see example.
}
\description{
Get location from an EcoCounter installation
}
\section{Function version}{
 0.0.1
}

\examples{

EcoCounterId <- 100037011
location <- get_EcoCounter_location(EcoCounterId)

\dontrun{
library(leaflet)
leaflet(location) \%>\%
  addProviderTiles(providers$OpenStreetMap) \%>\%
  addTiles() \%>\%
  addMarkers(~long, ~lat, popup = ~htmltools::htmlEscape(name))
}

}
\author{
Johannes Friedrich
}
