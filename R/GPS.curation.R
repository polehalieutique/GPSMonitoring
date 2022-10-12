#'  Bayesian global model, on encapsule un modèle global générique dans la fonction
#' @param GPS.data sf object set up for GPX.load function
#' @param extent sf object for area of interest definition
#' @param exclude sf object for exclusion of data (ie port by examples)
#' @examples
#'emprise<-matrix(c(-15,11,-14,11,-14,10,-15,10,-15,11),ncol=2, byrow=TRUE)
#'pol.extent <-st_sfc(st_polygon(list(emprise)))
#'st_crs(pol.extent) = 4326
#'
#'#exclude is some area to exclude from analysis as position too close from the port
#'# I want to exlude all data that intersect exclude area
#'ports<-data.frame(code=c('KPN','KCK'),long=c(-14.61,-15.04),lat=c(10.62,10.892))
#'ports.sf<- st_as_sf(ports,coords=c("long","lat"),crs=4326)
#'exclude<-st_buffer(ports.sf,0.1)
#'
#'
#'gps.all.cur<-GPS.curation(gps.all,extent=pol.extent,exclude=exclude)
#'
#'g1<-ggplot(gps.all)+geom_sf()+geom_sf(data=exclude,fill=rgb(0.8,0.11,0.1,0.5))+
#'  geom_sf(data=pol.extent,fill=rgb(0.1,0.8,0.1,0.5))
#'
#'g2<-ggplot(gps.all.cur)+geom_sf()+geom_sf(data=exclude,fill=rgb(0.8,0.11,0.1,0.5))+
#'  geom_sf(data=pol.extent,fill=rgb(0.1,0.8,0.1,0.5))
#'
#'ggarrange(g1,g2)
#'
#'
#' @export
GPS.curation<- function (GPS.data,extent=NULL,exclude=NULL){

  if (!is.null(extent)) {GPS.data<-st_join(GPS.data,extent,join=st_within,left=FALSE) }
  if (!is.null(exclude)) {
    GPS.data<-st_join(GPS.data,exclude,join=st_disjoint,left=FALSE)

      }
return(GPS.data)
}
