#' all.add.nb.point will proceed to add.nb.point to all subtrajectory of a global traj dataset
#' @param traj dataset of trajectories
#' @param r distance to use to calculate the number of close Gps position to each track position
#' @examples
#' data(R2)
#' R2test_retour<-all.add.nb.point <-function (R2,r=2000)
#' @export
#'
all.add.nb.point <-function (traj,r=2000,temp_windows=10)
{
  library(parallel)

  cl<-makeCluster(detectCores()/2)

  R2test_retour<-traj %>% inner_join(do.call(rbind,parLapply(cl,split(traj, traj$no_trajet),add.nb.point,r=r,temp_windows=temp_windows)))

  stopCluster(cl)

  return(R2test_retour)

}

