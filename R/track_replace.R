#' Replace a given track id by a new one. Espacially used when adding In silico observation to a given set of trajectory.
#' @param trajInit A big sf objection with all the track id used
#' @param new_track A subset of trajInit modified and to be replace instead of the old Subset
#' @examples
#'
#' @export
#'

track_replace<-function(trajInit,new_track)
{
  no_trajet_replace<-new_track$no_trajet[1]
  return(trajInit %>% filter(no_trajet!=no_trajet_replace) %>% rbind(new_track))
}
