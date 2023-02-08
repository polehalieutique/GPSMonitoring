#' Prediction on fishing activity based on a given model
#' @param traj A subset of global trajectory dataset identified by one no_trajet
#' @param gear.glm The glm model used for the prediction
#' @param seuil Threshold used to predict fishing activity
#' @examples
#' #glm.predict(filter(R2,code_engin==engin_encours),gear.glm,seuil=0.5)
#' @export
#'

glm.predict <- function(traj,gear.glm,seuil)
{

traj2<-traj
traj2$predict.glm.int<-(predict(gear.glm,traj,type="response")>seuil)

traj2 %>%  mutate(predict.glm=case_when(predict.glm.int==0 ~ 'UK',predict.glm.int==1 ~ 'active'))->traj2

traj$predict.glm<-traj2$predict.glm

return(traj)

}
