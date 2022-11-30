#' Load one GPX file or All gpx files of a give directory
#' @param files Name of a gpx file to be loaded
#' @param gpx_rep Name of the directory that contains all gps files that should be loaded
#' @examples
#' #GPX.load(gpx_rep='data/gpx')
#' @export

GPX.load<- function (file=NULL,gpx_rep=NULL){

  if (!is.null(file)) {A<-st_read(file,layer="track_points") %>% mutate(filename=basename(file)) }
  else
  {
    if (!is.null(gpx_rep)) {
        filelist<-paste(gpx_rep,'/',dir(gpx_rep,pattern='*.gpx'),sep='')


        for (i in 1:length(filelist))
        {
        data<-st_read(filelist[i],layer="track_points") %>%
          mutate(filename=basename(filelist[i]),track_fid=i)
        if (i==1) {A<-data}
        else {A<-rbind(A,data)}

        }

        }
  }
return(A)

}
