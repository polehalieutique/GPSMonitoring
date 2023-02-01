#' Tool to draw more complex extent using
#' @param traj A subset of global trajectory dataset identified by one no_trajet
#' @param obervation  The name of the column used to store observed activity
#' @param formula Default formula is observation~dist but users can specified his on model
#' @examples
#'  "gear.glm<-model.traj.RF(filter(R2,code_engin==engin_encours),observation='activity','dist')
#' @export

model.traj.RF<-function(traj,observation=NULL,form=NULL) {


  if (is.null(form)) {formula_train<- 'activity~dist'}
  else
  {
    formula_train<- paste(observation,'~',form,sep='')
  }

  print(formula_train)

  # on ne doit selecionner que les no_trajet avec observation
  traj %>% dplyr::filter(!!as.name(observation)=='active') %>% dplyr::distinct(no_trajet)->labelled_traj
  traj %>% dplyr::filter(no_trajet %in% labelled_traj$no_trajet) %>% st_drop_geometry() %>% as_tibble() ->labelled_dta

  labelled_dta %>% mutate(across(!!as.name(observation), as.factor))->labelled_dta
  colX    <- which(stringr::str_detect(colnames(labelled_dta) , gsub('\\+','|',form)))

  count_class <-  labelled_dta %>% st_drop_geometry()%>% dplyr::group_by(!!as.name(observation)) %>% dplyr::summarise(count = n()) %>% dplyr::select(count) %>% unlist()
  weight_class <-  round(sum(count_class) / count_class,1)


  #Je supprime les valeurs nulles sur le colonnes utilisées pour le modèle
  labelled_dta<-labelled_dta %>% filter_at(colX,all_vars(!is.na(.)))
  mtry.calc<-1+lengths(regmatches(form, gregexpr("\\+", form)))
  mod.RF<-ranger(formula_train,data =labelled_dta , num.trees = 800, mtry = mtry.calc, class.weights = weight_class)


  #if (is.null(formula)) {formula<- "traj2$observation2~traj2$dist+traj2$R2n+traj2$rel.angle"}


  return (mod.RF)
}
