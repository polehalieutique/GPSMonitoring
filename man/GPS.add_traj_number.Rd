% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/GPS.add_traj_number.R
\name{GPS.add_traj_number}
\alias{GPS.add_traj_number}
\title{GPS.add_traj_number, the function will split set of ddata collected from a file in several independant trajectory.
A set of ping is consider as a new trajectory if duration between two ping is higher that limit parameter}
\usage{
GPS.add_traj_number(GPS.data, limit)
}
\arguments{
\item{GPS.data}{sf object set up for GPX.load function}

\item{limit}{definition of the duration between two position to consider a new traject}
}
\description{
GPS.add_traj_number, the function will split set of ddata collected from a file in several independant trajectory.
A set of ping is consider as a new trajectory if duration between two ping is higher that limit parameter
}
\examples{
data(GPSdataset)
library(sqldf)
limit<-600*120
GPSdataset \%>\% mutate(filename=paste(code_village,code_engin,code_pecheur,'.gpx',sep='_')) \%>\%
arrange (filename) \%>\% dplyr::distinct(code_village,code_engin,code_pecheur,filename) \%>\%
 dplyr::mutate(track_fid=row_number(),track_seg_id=track_fid) \%>\%
 inner_join(GPSdataset) \%>\%
 group_by(filename) \%>\% arrange (filename,date_heure) \%>\% dplyr::mutate(track_seg_point_id = row_number()) \%>\%
 dplyr::rename(time=date_heure) \%>\%
 st_as_sf(coords = c("longitude", "latitude"), crs = 4326,remove=FALSE)->gps.all
 gps.all.cur_traj<-GPS.add_traj_number(gps.all,limit)
}
