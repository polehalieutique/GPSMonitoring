## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE,out.width="100%")
getwd()
rm (list=ls()) 
library(sf)
library(adehabitatLT)
library(sqldf)
library(dplyr)
library(ggplot2)
library(ggpubr)

library(RPostgreSQL)
library(tidyr)
library(units)
library(GPSMonitoring)
library(knitr)
library(kableExtra)
library(shiny)
library(leaflet)
library(ranger)
library(randomForest)
library(caret)



## ----part1--------------------------------------------------------------------




data(GPSdataset)

GPSdataset<-GPSdataset %>% filter(date_heure<"2019-09-30 20:20:53 CEST")

## -----------------------------------------------------------------------------

GPSdataset %>% mutate(filename=paste(code_village,code_engin,code_pecheur,'.gpx',sep='_')) %>% 
arrange (filename) %>% dplyr::distinct(code_village,code_engin,code_pecheur,filename) %>% 
dplyr::mutate(track_fid=row_number(),track_seg_id=track_fid) %>% 
inner_join(GPSdataset) %>%  
group_by(filename) %>% arrange (filename,date_heure) %>% dplyr::mutate(track_seg_point_id = row_number()) %>% 
  dplyr::rename(time=date_heure) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326,remove=FALSE)->gps.all

ggplot(gps.all)+geom_sf(aes(color=filename),size=0.2)




## -----------------------------------------------------------------------------
data(fond)

ggplot(gps.all)+geom_sf(data=fond)+geom_sf(aes(color=filename),size=0.2)


## ----part2--------------------------------------------------------------------


emprise<-matrix(c(-17,11,-14,11,-14,9,-15,9,-17,11),ncol=2, byrow=TRUE)
pol.extent <-st_as_sf(st_sfc(st_polygon(list(emprise))))
st_crs(pol.extent) = 4326

ports<-data.frame(code=c('KPN'),long=c(-14.61),lat=c(10.6789))
ports.sf<- st_as_sf(ports,coords=c("long","lat"),crs=4326)
exclude<-st_as_sf(st_union(st_buffer(ports.sf,1000)))

ggplot(exclude)+geom_sf(data=pol.extent)+geom_sf(fill='red')

gps.all.cur<-GPS.curation(gps.all,extent=pol.extent,exclude=exclude)



g1<-ggplot(gps.all)+geom_sf()+geom_sf(data=exclude,fill=rgb(0.8,0.11,0.1,0.5))+
  geom_sf(data=pol.extent,fill=rgb(0.1,0.8,0.1,0.5))

g2<-ggplot(gps.all.cur)+geom_sf()+geom_sf(data=exclude,fill=rgb(0.8,0.11,0.1,0.5))+
  geom_sf(data=pol.extent,fill=rgb(0.1,0.8,0.1,0.5))

ggarrange(g1,g2)



## ----eval=FALSE---------------------------------------------------------------
#  
#  pol.extent<-create.extent(st_convex_hull(st_union(gps.all)))
#  
#  

## ----part3--------------------------------------------------------------------
data(emprise)

ggplot(emprise)+geom_sf()+
  ggtitle("More precise pol.extent created using create.extent function")

pol.extent<-emprise

gps.all.cur<-GPS.curation(gps.all,extent=pol.extent,exclude=exclude)

g1<-ggplot(gps.all)+geom_sf()+geom_sf(data=exclude,fill=rgb(0.8,0.11,0.1,0.5))+
  geom_sf(data=pol.extent,fill=rgb(0.1,0.8,0.1,0.5))

g2<-ggplot(gps.all.cur)+geom_sf()+geom_sf(data=exclude,fill=rgb(0.8,0.11,0.1,0.5))+
  geom_sf(data=pol.extent,fill=rgb(0.1,0.8,0.1,0.5))

ggarrange(g1,g2)


## ----part4--------------------------------------------------------------------
limit<-600*120 #2 hours between two point and we consider a new traject
 #limit<-240
head(gps.all.cur)

GPS.data<-gps.all.cur
gps.all.cur_traj<-GPS.add_traj_number(gps.all.cur,limit)  

gps.all.cur_traj %>% mutate(x = sf::st_coordinates(.)[,1],
                y = sf::st_coordinates(.)[,2])->gps.all.cur_traj


unique(gps.all.cur_traj$track_fid)
unique(gps.all.cur_traj[gps.all.cur_traj$track_fid==2,]$no_trajet)



head(gps.all.cur_traj)



## ----part5,echo=FALSE,message=FALSE,warning=FALSE-----------------------------
ggplot(filter(gps.all.cur_traj,duree<350))+geom_histogram(aes(x=duree))


gpstmp<-filter(gps.all.cur_traj,track_fid==1)



step_dt=300
R.gps.all.cur_traj<-Redis.traj(GPS.data=st_drop_geometry(gps.all.cur_traj),step=step_dt,silent=TRUE)

## ----part5.1------------------------------------------------------------------
ggplot(filter(R.gps.all.cur_traj,duree<350))+geom_histogram(aes(x=duree))


projcrs <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
R.gps.all.cur_traj <- st_as_sf(x = R.gps.all.cur_traj,
               coords = c("x", "y"),
               crs = projcrs)



ggplot(R.gps.all.cur_traj)+geom_sf(aes(color=no_trajet))+ggtitle(paste("Tracks redistribute in a ",step_dt," period"))


## -----------------------------------------------------------------------------


data(Observed_FO)
head(Observed_FO) %>% kable()

## -----------------------------------------------------------------------------
R.gps.all.cur_traj %>% tidyr::separate(filename,c('code_village','code_engin','code_pecheur'),remove=FALSE,sep='_') %>% 
  mutate(code_pecheur=as.numeric(code_pecheur)) %>%  mutate(longitude = sf::st_coordinates(.)[,1],
                                             latitude = sf::st_coordinates(.)[,2])->R2
#add id number for each position of a traject
R2$id<-(R2 %>% group_by(no_trajet) %>% mutate(id=row_number()) %>% select (id))$id

R2$date<-as.POSIXct(R2$date)
R2 %>% mutate(obs=as.integer(1),activity='UK')->R2
head(R2)


for (compteur in seq(1,length(Observed_FO$X)))
{  
#compteur<-1  
R2<-R2 %>% mutate(activity=case_when(
code_village==Observed_FO[compteur,]$code_village & code_engin==Observed_FO[compteur,]$code_engin  & code_pecheur==Observed_FO[compteur,]$code_pecheur & date>Observed_FO[compteur,]$start_op & date<Observed_FO[compteur,]$end_op ~ 'active',
TRUE ~ activity),obs=case_when(
code_village==Observed_FO[compteur,]$code_village & code_engin==Observed_FO[compteur,]$code_engin  & code_pecheur==Observed_FO[compteur,]$code_pecheur & date>Observed_FO[compteur,]$start_op & date<Observed_FO[compteur,]$end_op ~ Observed_FO[compteur,]$X,
TRUE ~ obs))
}
compteur<-40
liste_trajet_avec_obs<-R2 %>% st_drop_geometry()%>% filter(activity=='active')%>% dplyr::distinct(no_trajet)                           




ggplot(filter(R2,no_trajet %in% liste_trajet_avec_obs$no_trajet))+geom_sf(aes(color=as.factor(activity)),lwd=0.1)+ggtitle("Tracks redistribute in a 60s period with observation")



## ----part6--------------------------------------------------------------------

liste_trajet_avec_obs<-R2 %>% st_drop_geometry()%>% filter(activity=='active')%>% dplyr::distinct(no_trajet)     

R2_avec_obs<-R2 %>%  filter(no_trajet %in% liste_trajet_avec_obs$no_trajet) 


R2_avec_obs %>% st_drop_geometry() %>% 
  group_by(code_engin,activity,dist) %>% dplyr::summarise(frequence=n()) %>%  ggplot()+
  geom_smooth(aes(x=dist,y=frequence,col=activity))+
  facet_wrap(~code_engin,scales = "free")+ggtitle('Fishing event ~ distance(=speed)')

bb<-st_bbox(R2_avec_obs)

ggplot(R2_avec_obs)+geom_sf(data=fond)+geom_sf(aes(color=activity),alpha=0.45,lwd=1)+
    scale_fill_viridis_c(option = "plasma", trans = "sqrt")+xlim(as.numeric(bb$xmin),as.numeric(bb$xmax))+ ylim(as.numeric(bb$ymin), as.numeric(bb$ymax))+facet_wrap(~code_engin)

unique(R2_avec_obs$code_engin)

engin_encours<-'FMCy'

head(R2_avec_obs)
R2_avec_obs %>% filter(code_engin==engin_encours) %>% ggplot()+geom_line(aes(x=date,y=dist))+
  geom_point(aes(x=date,dist,col = activity))  +
  facet_wrap(~no_trajet,scale='free')+ggtitle(paste("For gear ",engin_encours," Fishing event ~ distance (=speed)",sep=""))


R2_avec_obs %>% filter(code_engin==engin_encours) %>% ggplot()+geom_line(aes(x=date,y=R2n))+
  geom_point(aes(x=date,R2n,col = activity))  +
  facet_wrap(~no_trajet,scale='free')+ggtitle(paste("For gear ",engin_encours," Relation observation et R2n",sep=''))

R2_avec_obs %>% filter(code_engin==engin_encours) %>% ggplot()+geom_line(aes(x=date,y=rel.angle))+
  geom_point(aes(x=date,rel.angle,col = activity))  +
  facet_wrap(~no_trajet,scale='free')+ggtitle(paste("For gear ",engin_encours,"  Relation observation et rel.angle",sep=''))



## -----------------------------------------------------------------------------

observation<-'activity'

#On teste avec les 3 paramètres 
gear.glm<-model.traj.glm(filter(R2,code_engin==engin_encours),observation='activity',form='dist+abs.angle')

summary(gear.glm)

summary(gear.glm)
plot(gear.glm)


## ----part7--------------------------------------------------------------------


R2.pred<-glm.predict(filter(R2,code_engin==engin_encours),gear.glm,seuil=0.5)

p1<-ggplot(R2.pred)+geom_sf(aes(color=as.factor(predict.glm)),alpha=0.45,lwd=0.1)+
    scale_fill_viridis_c(option = "plasma", trans = "sqrt")
p2<-ggplot(filter(R2_avec_obs,code_engin==engin_encours))+geom_sf(aes(color=activity),alpha=0.45,lwd=0.1)+
    scale_fill_viridis_c(option = "plasma", trans = "sqrt")
ggarrange(p1,p2,ncol=1)


## ----eval=FALSE---------------------------------------------------------------
#  data(R2)
#  R2 %>% st_drop_geometry() %>% dplyr::filter(code_engin==engin_encours) %>% dplyr::group_by(no_trajet) %>%
#    dplyr::summarize(nb_positions=n()) %>% dplyr::arrange(desc(nb_positions)) %>% dplyr::top_n(5) -> traj_to_observe
#  
#  R2 %>% filter(no_trajet %in% traj_to_observe$no_trajet) %>%
#    ggplot()+geom_sf(aes(color=as.factor(no_trajet)))+facet_wrap(~no_trajet)
#  
#  #I create the new column and set values to Unknown
#  R2$activity_plus<-'UK'
#  i<-25
#  
#  for (i in traj_to_observe$no_trajet)
#  {
#    R2<-track_replace(R2,silicoTraj(filter(R2,no_trajet==i),mode='speed'))
#  }
#  
#  for (i in traj_to_observe$no_trajet)
#  {
#    R2<-track_replace(R2,silicoTraj(filter(R2,no_trajet==i),mode='map'))
#  }
#  

## -----------------------------------------------------------------------------

data(R2)
head(R2)
engin_encours<-'FMCy'
traj_to_observe<-R2 %>% dplyr::filter(activity_plus=='active') %>% distinct(no_trajet)


## -----------------------------------------------------------------------------

gear.glm.plus<-model.traj.glm(filter(R2,code_engin==engin_encours),observation="activity_plus",form= "dist+abs.angle")

summary(gear.glm.plus)
plot(gear.glm.plus)

R2.pred.plus<-glm.predict(filter(R2,code_engin==engin_encours),gear.glm.plus,seuil=0.5)


p1<-ggplot(R2.pred.plus)+geom_sf(aes(color=as.factor(predict.glm)),alpha=0.45,lwd=1)+
    scale_fill_viridis_c(option = "plasma", trans = "sqrt")
p2<-ggplot(filter(R2.pred.plus,code_engin==engin_encours))+geom_sf(aes(color=activity_plus),alpha=0.45,lwd=1)+
    scale_fill_viridis_c(option = "plasma", trans = "sqrt")
ggarrange(p1,p2,ncol=1)


## ----part8--------------------------------------------------------------------

R2test_retour<-all.add.nb.point(R2.pred.plus,r=2000,temp_windows=20)


ggplot(filter(R2test_retour,no_trajet==traj_to_observe$no_trajet[1]))+geom_sf(aes(color=(circle2000)))

ggplot(filter(R2test_retour,no_trajet==traj_to_observe$no_trajet[1]))+geom_point(aes(x=id,y=circle2000,color=activity_plus))



## -----------------------------------------------------------------------------

gear.glm.plus.nb<-model.traj.glm(filter(R2test_retour,code_engin==engin_encours),observation="activity_plus",form= "dist+abs.angle+circle2000")

summary(gear.glm.plus)

summary(gear.glm.plus.nb)
plot(gear.glm.plus.nb)

R2.pred.plus.nb<-glm.predict(filter(R2test_retour,code_engin==engin_encours),gear.glm.plus.nb,seuil=0.5) %>% filter(!is.na(predict.glm))


p1<-ggplot(R2.pred.plus.nb)+geom_sf(aes(color=as.factor(predict.glm)),alpha=0.45,lwd=1)+
    scale_fill_viridis_c(option = "plasma", trans = "sqrt")
p2<-ggplot(filter(R2.pred.plus.nb,code_engin==engin_encours))+geom_sf(aes(color=activity_plus),alpha=0.45,lwd=1)+
    scale_fill_viridis_c(option = "plasma", trans = "sqrt")
ggarrange(p1,p2,ncol=1)



## ----partRF-------------------------------------------------------------------

#To select some 
form <-"dist+abs.angle+circle2000"
mod.RF<-model.traj.RF(traj=R2.pred.plus.nb ,observation='activity_plus',form=form)

R2.pred.plus.nb.RF<-RF.predict(traj=R2.pred.plus.nb,mod.RF)
  
ggplot(R2.pred.plus.nb.RF)+geom_sf(aes(color=predict.RF),lwd=0.1)+geom_sf(data=st_crop(fond,st_bbox(R2.pred.plus.nb.RF)))+
  scale_color_manual(values = c("active" = "lightgreen","UK"="orange")) +
  ggtitle("Map of predict activities using RF method")

ggplot(R2.pred.plus.nb.RF)+geom_sf(aes(color=predict.glm),lwd=0.1)+geom_sf(data=st_crop(fond,st_bbox(R2.pred.plus.nb.RF)))+
  scale_color_manual(values = c("active" = "lightgreen","UK"="orange")) +
  ggtitle("Map of predict activities using GLM method")




## -----------------------------------------------------------------------------

qual.RF<-quality.ind(R2.pred.plus.nb.RF,col.activity='activity_plus',col.predict='predict.RF')

qual.glm<-quality.ind(R2.pred.plus.nb.RF,col.activity='activity_plus',col.predict='predict.glm')

qual.glm[[1]] %>% mutate(model='glm') %>% select(model,accuracy,sensitivity,specificity) %>% pivot_longer(cols=!model)->glm.ind
qual.RF[[1]] %>% mutate(model='RF') %>% select(model,accuracy,sensitivity,specificity) %>% pivot_longer(cols=!model)->RF.ind

rbind(glm.ind,RF.ind) %>% 
  ggplot()+geom_bar(aes(x=name,y=value,fill=model),stat='identity',position='dodge')+
  ggtitle("Quality comparaison between two last models GLM/RF")


## ----partgrid-----------------------------------------------------------------


engin_encours<-'FMCy'

#Remettre avec un shapefile dans data
#fond<-st_read(con,query="select st_intersection(st_buffer(geom,0),ST_GeomFromText('POLYGON((-18 13,-13 13,-13 7.94,-18 7.94,-18 13))',4326)) as geom 
#					   from communes_uni")
data(grid)

ggplot(grid)+geom_sf(color=NA)+geom_sf(data=R2.pred.plus.nb.RF)




 st_join(grid,filter(R2.pred.plus.nb.RF,predict.RF=='active'),left=FALSE) %>% group_by(code_engin,id.x) %>% dplyr::summarize(total=n()) %>% 
   ggplot()+ geom_sf(aes(fill=total),lwd=0)+geom_sf(data=fond)+facet_wrap(~code_engin)+ scale_fill_continuous(trans = 'reverse')+
  xlim(as.numeric(bb$xmin),as.numeric(bb$xmax))+ ylim(as.numeric(bb$ymin),as.numeric(bb$ymax))+
   ggtitle("Map of fishing event density")


st_join(grid,R2.pred.plus.nb.RF,left=FALSE) %>% group_by(code_engin,id.x) %>% dplyr::summarize(total=n()) %>% 
   ggplot()+ geom_sf(aes(fill=total),lwd=0)+geom_sf(data=fond)+facet_wrap(~code_engin)+ scale_fill_continuous(trans = 'reverse')+
  xlim(as.numeric(bb$xmin),as.numeric(bb$xmax))+ ylim(as.numeric(bb$ymin),as.numeric(bb$ymax))+
  ggtitle("Map of GPS data density")









