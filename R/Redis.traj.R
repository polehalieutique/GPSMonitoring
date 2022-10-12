#' Redis.traj function will redistribute ping along the trajectory with a constant duration between ping with respect of speed and time of initial data.
#' @param GPS.data sf object set up for GPX.load function
#' @param step constant between ping
#' @examples
#'
#' @export
#'
#'
#GPS.data<-st_drop_geometry(gps.all.cur_traj)
#step_dt<-300
#id_trajet<-165
Redis.traj<- function (GPS.data,step_dt){


    if (is.null(step)) {print('step parameter is request in order to use the function')}
  if (!is.null(step)) {
#La faire trajet par trajet
iter<-0
#GPS.data<-gpstmp
for (id_trajet in unique(GPS.data$no_trajet))
{
print(id_trajet)
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

