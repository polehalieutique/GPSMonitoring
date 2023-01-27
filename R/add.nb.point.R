#' add.nb.point will add a new column with the number of ping from the same sub trajectory contains in a circle of r radius
#' and in a temporal windows of temp_windows position before and temp_windows position after
#' @param traj subset of trajectory model reduce to one traject
#' @param r distance to use to calculate the number of close Gps position to each track position
#' @examples
#' library(parallel)

# cl<-makeCluster(6)

# R2test_retour<-R2 %>% inner_join(do.call(rbind,parLapply(cl,split(R2, R2$no_trajet),add.nb.point,r=2000)))

# stopCluster(cl)

#' @export
#'

add.nb.point <-function (traj,r=2000,temp_windows=10)
{
  library(units)
  library(sf)
  library(dplyr)

    units(r)<-'m'

  dist_matrix<-(as.matrix((st_distance(traj,traj,by_element = FALSE)))<r)

  dist.matrix.num <- matrix(as.numeric(dist_matrix),    # Convert to numeric matrix
                    ncol = ncol(dist_matrix))

  liste_idpos<-traj %>% st_drop_geometry() %>%  dplyr::select(id)

  liste_idpos %>%
    inner_join(liste_idpos,by = character()) %>% dplyr::mutate(diff=as.numeric(abs(id.y-id.x)<temp_windows)) %>%
    tidyr::pivot_wider(names_from =id.y,values_from=diff) %>% dplyr::select(-id.x)->temporal_window

  nb_points<-rowSums(dist.matrix.num*temporal_window)
ret<-data.frame(no_trajet=unique(traj$no_trajet),id=traj$id,circle=nb_points)

names(ret)[3]<-paste("circle",r,sep='')
  return(ret)
}

