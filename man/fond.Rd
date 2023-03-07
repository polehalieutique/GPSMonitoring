\name{fond}
\alias{fond}
\docType{data}
\title{
sf object with coastline related to the GPS dataset
}
\description{
 Fond is a subset of a world shapefile
}
\usage{data("fond")}
\format{
  A data frame with 1 observations.
  \describe{
    \item{\code{geom}}{a sfc_MULTIPOLYGON}
  }
}
\details{
%%
}
\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
}
\references{
%%
}
\examples{
data(fond)
ggplot(fond)+ geom_sf()

}
\keyword{datasets}
