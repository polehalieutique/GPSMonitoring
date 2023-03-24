\name{emprise}
\alias{emprise}
\docType{data}
\title{
Sample Area of interest
}
\description{
This sf object was set up using the create.extent function and is a polygon where all gps position within this polygon will be kept for analysis.
}
\usage{data("emprise")}
\format{
  A sf object with 1 polygon.
  \describe{
    \item{\samp{_leaflet_id}}{a numeric vector}
    \item{\code{feature_type}}{a character vector}
    \item{\code{geometry}}{a sfc_POLYGON}
  }
}
\details{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
}
\references{
%%  ~~ possibly secondary sources and usages ~~
}
\examples{
library(ggplot2)
library(sf)
data(emprise)
ggplot(emprise)+geom_sf()
}
\keyword{datasets}
