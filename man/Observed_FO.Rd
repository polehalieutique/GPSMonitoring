\name{Observed_FO}
\alias{Observed_FO}
\docType{data}
\title{
%%  Observed fishing event using on board obervers.
}
\description{
%% During preliminary observation, we wanted to obtain real fishing event on vessel with GPS on board. Scientific observers was thus embarked. Linked to the GPS on board they were requested to note each fishing event during the fishing trip
}
\usage{data("Observed_FO")}
\format{
  A data frame with 328 observations on the following 7 variables.
  Each observed unit (for on board observation and GPS) can be identified using code_village,code_engin,code_pecheur
  \describe{
    \item{\code{X}}{a numeric vector}
    \item{\code{code_village}}{a string with the code of the village}
    \item{\code{code_engin}}{a string with the gear code}
    \item{\code{code_pecheur}}{a string with the fisherman code}
    \item{\code{start_op}}{a timestamp with the beginning of the fishing event}
    \item{\code{end_op}}{a timestamp with the end of the fishing event}
    \item{\code{fishing}}{a string that could qualify the fishing event - here only active values}
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
data(Observed_FO)
head(Observed_FO)
}
\keyword{datasets}
