#' Generic function to cure gps data
#' @param GPS.data sf object set up for GPX.load function
#' @param extent sf object for area of interest definition
#' @param exclude sf object for exclusion of data (ie port by examples)
#' @examples
#'
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
