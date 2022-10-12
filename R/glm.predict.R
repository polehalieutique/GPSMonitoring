#' Prediction on fishing activity based on a given model
#' @param traj A subset of global trajectory dataset identified by one no_trajet
#' @param gear.glm The glm model used for the prediction
#' @param seuil Threshold used to predict fishing activity
#' @examples
#' glm.predict(filter(R2,code_engin==engin_encours),gear.glm2,seuil=0.5)
#' @export
#'
glm.predict <- function(traj,gear.glm,seuil)
{

  traj %>% mutate(activity=as.numeric(activity=='active')) ->traj2



  traj2$predict.glm<-(predict(gear.glm,traj2,type="response")>seuil)
  return(traj2)

}
