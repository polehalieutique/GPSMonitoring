#' Redis.traj function will redistribute ping along the trajectory with a constant duration between ping with respect of speed and time of initial data.
#' @param GPS.data sf object loading GPX.load function or similar one
#' @param step constant between ping
#' @examples
#' data(GPSdataset)
#' library(sqldf)
#' library(dplyr)
#' library(adehabitatLT)
#' library(sf)
#' limit<-600*120
#' step_dt=300
#' GPSdataset %>% mutate(filename=paste(code_village,code_engin,code_pecheur,'.gpx',sep='_')) %>%
#' arrange (filename) %>% dplyr::distinct(code_village,code_engin,code_pecheur,filename) %>%
#'  dplyr::mutate(track_fid=row_number(),track_seg_id=track_fid) %>%
#'  inner_join(GPSdataset) %>%
#'  group_by(filename) %>% arrange (filename,date_heure) %>% dplyr::mutate(track_seg_point_id = row_number()) %>%
#'  dplyr::rename(time=date_heure) %>%
#'  st_as_sf(coords = c("longitude", "latitude"), crs = 4326,remove=FALSE)->gps.all
#'  gps.all.cur_traj<-GPS.add_traj_number(gps.all,limit)%>% mutate(x = longitude,y = latitude)
#'  R.gps.all.cur_traj<-Redis.traj(GPS.data=st_drop_geometry(gps.all.cur_traj),step=step_dt,silent=TRUE)
#' @export
#'
#'
Redis.traj<- function (GPS.data,step_dt,silent=NULL){


    if (is.null(step)) {print('step parameter is request in order to use the function')}
  if (!is.null(step)) {
#La faire trajet par trajet
iter<-0
#GPS.data<-gpstmp
for (id_trajet in unique(GPS.data$no_trajet))
{
if (is.null(silent)) {print(id_trajet)}
     GPStmp<-GPS.data %>% dplyr::filter(no_trajet==id_trajet)
filename<-unique(GPStmp$filename)
#print(paste(filename,id_trajet))

#print(paste(GPStmp %>% group_by(no_trajet) %>% summarise(toto=n())))

tr1 <- as.ltraj(GPStmp[,c("x","y")],
                     date = GPStmp$time,
                     id=id_trajet,burst=id_trajet)

    tr1<-redisltraj(tr1,step_dt,type="time")

    tr3<-ltraj2sf(tr1)

#    print(paste(dim(tr1[[1]])))
#print(paste("TR2",dim(tr2[[1]])))
#print(paste("TR3",dim(tr3)))
#print(paste(tr3 %>% group_by(no_trajet) %>% summarise(toto=n())))
reste<-tr3 %>% dplyr::mutate(filename=filename,duree=dt)
if (iter==0) {reste_total<-reste
iter<-1}
else
{
reste_total<-bind_rows(reste_total,reste)
}
}
return(reste_total)

  }

}

