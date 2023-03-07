\name{GPSdataset}
\alias{GPSdataset}
\docType{data}
\title{
A Large datset of GPS position collected on Fishing Canoe in Guinée during the DEMERSTEM EU project
}
\description{
GPS were embarked since September 2019 on such Artisanal fishing boat through the CNSHB* data collection (Centre National des Sciences Halieutiques de Bousssourah)
}
\usage{data("GPSdataset")}
\format{
  A data frame with 602344 observations on the following 6 variables.
  \describe{
    \item{\code{code_village}}{a string with the code of the village}
    \item{\code{code_engin}}{a string with the gear code}
    \item{\code{code_pecheur}}{a string with the fisherman code}
    \item{\code{date_heure}}{a POSIXct date of the Ping}
    \item{\code{longitude}}{The longitude (WGS84) of the ping}
    \item{\code{latitude}}{The Latitude (WGS84) of the ping}
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
data(GPSdataset)
head(GPSdataset)
}
\keyword{datasets}
