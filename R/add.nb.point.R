#' add.nb.point will add a new column with the number of ping from the same sub trajectory contains in a circle of r radius
#' @param traj subset of trajectory model reduce to one traject
#' @param r distance to use to calculate the number of close Gps position to each track position
#' @examples
#' library(parallel)

# cl<-makeCluster(6)

# R2test_retour<-R2 %>% inner_join(do.call(rbind,parLapply(cl,split(R2, R2$no_trajet),add.nb.point,r=2000)))

# stopCluster(cl)

#' @export
#'

add.nb.point <-function (traj,r=2000)
{
  library(units)
  library(sf)
  units(r)<-'m'
  test<-(as.matrix((st_distance(traj,traj,by_element = FALSE)))<r)
  mat_num <- matrix(as.numeric(test),    # Convert to numeric matrix
                    ncol = ncol(test))

  nb_points<-rowSums(mat_num)


  return(data.frame(no_trajet=unique(traj$no_trajet),id=traj$id,nb_points=nb_points))
}

