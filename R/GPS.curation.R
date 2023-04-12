#' Generic function to cure gps data
#' @param GPS.data sf object set up for GPX.load function
#' @param extent sf object for area of interest definition
#' @param exclude sf object for exclusion of data (ie port by examples)
#' @examples
#' library(ggpubr)
#' data(GPSdataset)
#' data(emprise)
#' ports<-data.frame(code=c('KPN'),long=c(-14.61),lat=c(10.6789))
#' ports.sf<- st_as_sf(ports,coords=c("long","lat"),crs=4326)
#' exclude<-st_as_sf(st_union(st_buffer(ports.sf,1000)))
#' gps.all.cur<-GPS.curation(gps.all,extent=emprise,exclude=exclude)
#' g1<-ggplot(gps.all)+geom_sf()+geom_sf(data=exclude,fill=rgb(0.8,0.11,0.1,0.5))+
#' geom_sf(data=emprise,fill=rgb(0.1,0.8,0.1,0.5))
#' g2<-ggplot(gps.all.cur)+geom_sf()+geom_sf(data=exclude,fill=rgb(0.8,0.11,0.1,0.5))+
#'  geom_sf(data=emprise,fill=rgb(0.1,0.8,0.1,0.5))
#' ggarrange(g1,g2)
#' @export
GPS.curation<- function (GPS.data,extent=NULL,exclude=NULL){

inputdim<-dim(GPS.data[,1])[1]

  if (!is.null(extent)) {GPS.data<-st_join(GPS.data,extent,join=st_intersects,left=FALSE,prepare=TRUE) }
  if (!is.null(exclude)) {
    GPS.data<-st_join(GPS.data,exclude,join=st_disjoint,left=FALSE)

  }

outputdim<-GPS.data %>% st_drop_geometry() %>% summarize(nb_lignes=n())
print (paste("input number of position :",inputdim,sep=''))
print (paste("output number of position :",outputdim$nb_lignes,sep=''))

       return(GPS.data)
}
