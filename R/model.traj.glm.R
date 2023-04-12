#' Tool to draw more complex extent using
#' @param traj A subset of global trajectory dataset identified by one no_trajet
#' @param obervation  The name of the column used to store observed activity
#' @param formula Default formula is observation~dist but users can specified his on model
#' @examples
#' data(R2)
#' engin_encours<-"FMCy"
#' gear.glm<-model.traj.glm(filter(R2,code_engin==engin_encours),observation='activity')
#' plot(gear.glm)
#' @export


model.traj.glm<-function(traj,observation=NULL,form=NULL) {

  # on ne doit selecionner que les no_trajet avec observation
  traj %>% dplyr::filter(!!as.name(observation)=='active') %>% dplyr::distinct(no_trajet)->liste_traj.obser
  traj %>% dplyr::filter(no_trajet %in% liste_traj.obser$no_trajet) %>% dplyr::mutate(observation2=as.numeric(!!as.name(observation)=='active')) ->traj2


  #if (is.null(formula)) {formula<- "traj2$observation2~traj2$dist+traj2$R2n+traj2$rel.angle"}

  if (is.null(form)) {formula<- 'observation2~dist'}
  else
  {
    formula<- paste('observation2','~',form,sep='')
  }

  print(formula)

  model<-glm(formula,data=traj2)


  return (model)
}
