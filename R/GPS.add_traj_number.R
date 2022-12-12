#' GPS.add_traj_number, the function will split set of ddata collected from a file in several independant trajectory.
#' A set of ping is consider as a new trajectory if duration between two ping is higher that limit parameter
#' @param GPS.data sf object set up for GPX.load function
#' @param limit definition of the duration between two position to consider a new traject
#' @examples
#'
#' @export
GPS.add_traj_number<- function (GPS.data,limit){

  if (is.null(limit)) {print('limit parameter is request in order to use the function')}
  if (!is.null(limit)) {
#La faire trajet par trajet
    liste_trajet<-GPS.data %>% st_drop_geometry() %>% dplyr::distinct(track_fid)
    final_entraxes<-data.frame()
    inc_trajet=0
    track<-1
      for(track in liste_trajet$track_fid)
  {

  GPS.data.tmp<-GPS.data %>% filter(track_fid==track)
    trajet_tmp<-GPS.data.tmp %>% dplyr::select(track_fid,track_seg_point_id,time) %>%
      dplyr::arrange(track_fid,time) %>%
      dplyr::mutate(points_suivant=lead(track_seg_point_id),traj_suivant=lead(track_fid),duree=as.numeric(lead(time)-time)) %>%
      st_drop_geometry()

    first_ligne<-trajet_tmp[1,]

    trajet_tmp %>%filter(traj_suivant!=track_fid |duree>limit) ->entraxes
    entraxes<-rbind(first_ligne,entraxes)

     entraxes %>%
       dplyr::mutate(point_courant=track_seg_point_id,no_trajet=inc_trajet+row_number()) %>%
      dplyr::select(track_fid,point_courant,no_trajet) %>%
       dplyr::mutate(point_suivant=lead(point_courant))->entraxes_step2

    entraxes_step2$point_suivant[length(entraxes_step2$point_suivant)]<-max(GPS.data.tmp$track_seg_point_id)

dispatche_entraxe<-sqldf("select A.track_fid,track_seg_point_id,no_trajet,duree from trajet_tmp A inner join entraxes_step2 B on
                         (A.track_fid=B.track_fid and A.track_seg_point_id>point_courant and A.track_seg_point_id<point_suivant)",drv='SQLite')
final_entraxes<-rbind(final_entraxes,dispatche_entraxe)
inc_trajet<-max(final_entraxes$no_trajet)+1

  }#Fin boucle

#Ca merde ici :


    GPS.data %>% dplyr::inner_join(final_entraxes,by=c('track_fid','track_seg_point_id')) %>% dplyr::mutate(track_fid=no_trajet) ->leretour

#    reset_lastduration<-leretour   %>%
#      dplyr::group_by(no_trajet) %>% dplyr::summarise(zero_update=max(track_seg_point_id))


return(leretour)
      }

}
