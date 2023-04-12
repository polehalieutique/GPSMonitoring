#' Prediction on fishing activity based on a given model
#' @param traj A subset of global trajectory dataset identified by one no_trajet
#' @param mod.RF The Random forest model calculate by ranger function
#' @examples
#' data(R2)
#' library(ranger)
#' engin_encours<-"FMCy"
#' gear.RF<-model.traj.RF(filter(R2,code_engin==engin_encours),observation='activity','dist')
#' R2.pred<-RF.predict(filter(R2,code_engin==engin_encours),gear.RF)
#' ggplot(R2.pred)+geom_sf(size=0.1,col='red')+ggtitle("GPS ping without prediction")
#' ggplot(R2.pred)+geom_sf(size=0.1,aes(col=activity_plus))+ggtitle("GPS ping with RF prediction")
#' @export


RF.predict <- function(traj,mod.RF)
{


  #Liste des variables explicatives a filter sur les valeurs non nulles
  colX<-mod.RF$forest$independent.variable.names

  traj %>% st_drop_geometry() %>% as_tibble() %>% filter_at(colX,all_vars(!is.na(.)))->dta

  #dta %>% mutate(across(!!as.name(observation), as.factor))->labelled_dta


  predict_data   <- predict(mod.RF, data = dta)

  dta %>% dplyr::mutate(predict.RF = predict_data$predictions)  -> res_RF

  result<-traj %>% inner_join(res_RF)


  return(result)

}
