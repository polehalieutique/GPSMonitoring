\name{grid}
\alias{grid}
\docType{data}
\title{
 A 0.5x0.5° grid related to the GPSdataset
}
\description{
 This sf object was set up using the st_makegrid function
}
\usage{data("grid")}
\format{
  A data frame with 20000 observations on the following 3 variables.
  \describe{
    \item{\code{id}}{a numeric vector, each statistical rectangle can be named using this id}
    \item{\code{coast_dist}}{a numeric vector, for each statistical rectangle, this value is the st_distance between the coastline (fond) and the centroid of each rectangle of this grid}
    \item{\code{geom}}{a sfc_POLYGON}
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
data(grid)
ggplot(grid)+ geom_sf()
}
\keyword{datasets}
