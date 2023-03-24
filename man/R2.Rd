\name{R2}
\alias{R2}
\docType{data}
\title{
A Large datset of GPS position (GPSdataset) with supplementary column (activity_plus) filled from expert knowledge using silicoTraj function to flag fishing activity along a trajectory.
}
\description{
GPS were embarked since September 2019 on such Artisanal fishing boat through the CNSHB* data collection (Centre National des Sciences Halieutiques de Bousssourah). Mohamed Soumah has then flagged position as active (=fishing) or not fishing.
}
\usage{data("R2")}
\format{
  A data frame with 58416 observations on the following 6 variables.
  \describe{
    \item{\code{date}}{a string with the code of the village}
    \item{\code{dx}}{a string with the gear code}
    \item{\code{dy}}{a string with the fisherman code}
    \item{\code{dist}}{distance}
    \item{\code{dt}}{distance see adehabitatLT package}
    \item{\code{R2n}}{the squared distance between the first relocation of the trajectory
and the current relocation is often used to test some movements models
(e.g. the correlated random walk, see the seminal paper of Kareiva and
Shigesada, 1983}
    \item{\code{abs.angle}}{he absolute angle αi between the x direction and the step
built by relocations i and i + 1 is sometimes used together with the pa-
rameter dist to fit movement models (e.g. Marsh and Jones 1988)}
    \item{\code{rel.angle}}{the relative angle βi measures the change of direction between
the step built by relocations i − 1 and i and the step built by relocations
i and i + 1 (often called “turning angle”). It is often used together with
the parameter dist to fit movement models (e.g. Root and Kareiva 1984,
Marsh and Jones 1988);}
    \item{\code{no_trajet}}{The traject id}
    \item{\code{track_fid}}{Idem as traject id}
    \item{\code{filename}}{Filename regarding the data collection protocol}
    \item{\code{code_village}}{Code of the village}
    \item{\code{code_engin}}{Gear code}
    \item{\code{code_pecheur}}{Fisherman code}
    \item{\code{duree}}{duration}
    \item{\code{longitude}}{The Latitude (WGS84) of the ping}
    \item{\code{latitude}}{The Latitude (WGS84) of the ping}
    \item{\code{id}}{The id unique value of the trajectory}
    \item{\code{obs}}{The Latitude (WGS84) of the ping}
    \item{\code{activity}}{Fishing event with on board observer}
    \item{\code{activity_plus}}{Fishing event with SilicoTraj observer}
    \item{\code{geometry}}{geometry - WGS84 - of the position}
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
