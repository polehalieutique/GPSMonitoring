#' Calculate and print sensitivity, accuracy and
#' @param proj.tarj Set of trajectory with predict.glm values
#' @param col.activity Column name for
#' @examples
#' #GPX.load(gpx_rep='data/gpx')
#' @export

quality.ind<- function (proj.traj,col.activity=NULL){

  if (!is.null(col.activity)) {col.activity<-"activity"}
  else
  {
    if (!is.null(proj.traj)) {
no_trajet_with_obs<-R2.pred.plus %>% dplyr::filter(!!as.symbol(col.activity)==1) %>% dplyr::distinct(no_trajet)

R2.pred.plus %>% st_drop_geometry() %>% dplyr::filter(no_trajet %in% no_trajet_with_obs$no_trajet) %>%
  dplyr::mutate(fp=case_when((!!as.symbol(col.activity)==0 & !!as.symbol(col.activity)==as.numeric(predict.glm))~1,TRUE~0),
                tp=case_when((!!as.symbol(col.activity)==1 & !!as.symbol(col.activity)==as.numeric(predict.glm))~1,TRUE~0),
                tn=case_when((!!as.symbol(col.activity)==1 & !!as.symbol(col.activity)!=as.numeric(predict.glm))~1,TRUE~0),
                fn=case_when((!!as.symbol(col.activity)==0 & !!as.symbol(col.activity)!=as.numeric(predict.glm))~1,TRUE~0)
                ) %>%
  dplyr::select(fp,tp,tn,fn) %>% summarize(fp=sum(fp),tp=sum(tp),tn=sum(tn),fn=sum(fn)) %>%
  mutate(indicator='quality',accuracy=(fn+tp)/(fn+tp+fn+fp),sensitivity=tp/(tp+fn),specificity=tn/(tn+fn))->qual.ind
qual.ind %>% dplyr::select(indicator,accuracy,sensitivity,specificity) %>% pivot_longer(!indicator,names_to='Ind_name') %>%
  ggplot(aes(x=Ind_name,y=value))+  geom_bar(position="dodge", stat = "identity")->g1

  g1

        }else {print('No data to feed the function')}
  }

#return(qual.ind)

}
