#' Calculate and print sensitivity, accuracy and selectivity quality indicators
#' @param proj.traj Set of trajectory with predict.glm values
#' @param col.activity Column name for observed fishing event
#' @param col.predict Column name for predict values
#' @examples
#' data(R2)
#' library(ranger)
#' library(tidyr)
#' engin_encours<-"FMCy"
#' gear.RF<-model.traj.RF(filter(R2,code_engin==engin_encours),observation='activity','dist')
#' R2.pred<-RF.predict(filter(R2,code_engin==engin_encours),gear.RF)
#' quality.ind(R2.pred,col.activity='activity',col.predict='predict.RF')
#' @export


quality.ind<- function (proj.traj,col.activity=NULL,col.predict=NULL){


  if (is.null(col.predict)) {col.predict<-"predict.glm"}
  if (is.null(col.activity)) {col.activity<-"activity"}

if (!is.null(proj.traj)) {

no_trajet_with_obs<-proj.traj %>% dplyr::filter(!!as.symbol(col.activity)=='active') %>% dplyr::distinct(no_trajet)


proj.traj %>% st_drop_geometry() %>% dplyr::filter(no_trajet %in% no_trajet_with_obs$no_trajet) %>%
  dplyr::mutate(fp=case_when((!!as.symbol(col.activity)=='UK' & !!as.symbol(col.predict)=='active')~1,TRUE~0),
                tp=case_when((!!as.symbol(col.activity)=='active' & !!as.symbol(col.predict)=='active')~1,TRUE~0),
                tn=case_when((!!as.symbol(col.activity)=='active' & !!as.symbol(col.predict)=='UK')~1,TRUE~0),
                fn=case_when((!!as.symbol(col.activity)=='UK' & !!as.symbol(col.predict)=='UK')~1,TRUE~0)
                ) %>%
  dplyr::select(fp,tp,tn,fn) %>% dplyr::summarize(fp=sum(fp),tp=sum(tp),tn=sum(tn),fn=sum(fn)) %>%
  dplyr::mutate(indicator='quality',accuracy=(tn+tp)/(fn+tp+tn+fp),sensitivity=tp/(tp+fn),specificity=tn/(tn+fp))->qual.ind


qual.ind %>% dplyr::select(indicator,accuracy,sensitivity,specificity) %>% pivot_longer(!indicator,names_to='Ind_name') %>%
  ggplot(aes(x=Ind_name,y=value))+geom_bar(position="dodge", stat = "identity")->g1

  g1

        }else {print('No data to feed the function')}

return(list(qual.ind,g1))

}
