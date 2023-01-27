#' Prediction on fishing activity based on a given model
#' @param traj A subset of global trajectory dataset identified by one no_trajet
#' @param mod.RF The Random forest model calculate by ranger function
#' @examples
#' #glm.predict(filter(R2,code_engin==engin_encours),gear.glm2,seuil=0.5)
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
