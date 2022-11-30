#' GLM model on the data
#' @param traj A subset of global trajectory dataset identified by one no_trajet
#' @param obervation  The name of the column used to store observed activity
#' @param formula Default formula is observation~dist but users can specified his on model
#' @examples
#'  "gear.glm<-model.traj.glm(filter(R2,code_engin==engin_encours),observation='activity')
#' @export

model.traj.glm<-function(traj,observation=NULL,form=NULL) {

  # on ne doit selecionner que les no_trajet avec observation
  traj %>% filter(!!as.name(observation)=='active') %>% dplyr::distinct(no_trajet)->liste_traj.obser
  traj %>% filter(no_trajet %in% liste_traj.obser$no_trajet) %>% mutate(observation2=as.numeric(!!as.name(observation)=='active')) ->traj2


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
