#' Generic function to cure gps data
#' @param GPS.data sf object set up for GPX.load function
#' @param extent sf object for area of interest definition
#' @param exclude sf object for exclusion of data (ie port by examples)
#' @examples
#'
#' @export
GPS.curation<- function (GPS.data,extent=NULL,exclude=NULL){

  if (!is.null(extent)) {GPS.data<-st_join(GPS.data,extent,join=st_within,left=FALSE) }
  if (!is.null(exclude)) {
    GPS.data<-st_join(GPS.data,exclude,join=st_disjoint,left=FALSE)

      }
return(GPS.data)
}
