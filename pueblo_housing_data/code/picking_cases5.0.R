puebr4<-readRDS("/Users/rpscott/Documents/GitHub/RESPECT-QTS/pueblo_housing_data/processed_data/full_s_2018.version3.rds")
puebr5<-readRDS("/Users/rpscott/Documents/GitHub/RESPECT-QTS/pueblo_housing_data/processed_data/permit_s_2018.version3.rds")
pueblo_contractors<-read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vRj8AkwjfguXDbr14v80dLPcOFzQxjKZ9ZMKbln08X7gQPNDgT94mS7alolTmwH6mCais4Nd0g2T5nK/pub?output=csv")

#contractor_code
puebr5$SaleYear<-puebr5$SaleDate %>% lubridate::ymd() %>% lubridate::year()
m1<-inla(mech.permit~scale(HCB)+
           scale(Pr_____)+
           f(YearBuilt,model="rw2")+
           lowfaircondition+
           evapcooler+
           centralair+
           ebaseboard+
           scale(prob_hispanic)+
           scale(log(as.numeric(SquareFootage+1)))+
           scale(log(as.numeric(ActualValue+1)))+
           saleinperiod+
           lastsalepre2012+
           f(placeidinla,model="bym",graph="Documents/qt-local/mechanicals/ebnb.adj"),
         data=puebr4,
         family="binomial")
library(INLA)
batch1<-c(30, 134, 68, 103,76,10,12,106,96,97,11,100)
set.1129<-c(46,27,105,27,37,16,17,14,9,13)
set.1218<-c(25,41,79,111,39,19,21,126,121,38,34,84,88,110,130,58,38,34,120,116,121,126,131)
oldsamps<-list.files("Documents/qt-local/qualsamples/",full.names=T) %>% lapply(read.csv)
oldamps<-lapply(oldsamps,function(X) X %>% select(Permit_No,ASSESSOR_PARID)) %>% bind_rows() %>% unique()
puebsamped<-filter(puebr5, Year==2022,SaleYear!=2023, placeidinla%in%c(batch1,set.1129,set.1218))
puebsamped.new<-puebsamped[which(puebsamped$Permit_No%in%oldamps$Permit_No==F),]
missed_parcels<-puebsamped.new

sampled_homes<-rbind(puebsamped, puebsamped.new)



head(sampled_homes)

library(ggplot2)
m1$summary.random$placeidinla
ggplot(sampled_homes)+geom_bar(aes(x=Contractor))+coord_flip()

puebr5$Year
devtools::install_github("wilkox/treemapify")


f1<-puebr5 %>% group_by(Contractor) %>% filter(Year==2022) %>% summarize(ActualValue=sum(ActualValue,na.rm=T)) %>% arrange(-ActualValue) 
f1$Contractorrecode<-f1$Contractor
f1$Contractorrecode[30:nrow(f1)]<-"OTHER"
f1$Contractorrecode<-as.factor(f1$Contractorrecode)
f1$Contractorrecode<-forcats::fct_inorder(f1$Contractorrecode)
puebr5$evapcooler
library(sf)
puebr5shape<-st_sf(puebr5)
puebr5shape$insample<-puebr5$ASSESSOR_PARID%in%sampled_homes$ASSESSOR_PARID

ggplot(filter(puebr5shape,insample==T))+geom_sf()+theme_minimal()


f1 %>% ggplot(.)+geom_bar(aes(x=Contractorrecode,y=ActualValue),stat="identity")+coord_flip()+theme_minimal()+theme_minimal()

f1$hhip<-f1$ActualValue/sum(f1$ActualValue)*100
sum(f1$hhip^2)
puebr4 %>% xtabs(~evapcooler+centralair,data=.) %>% reshape2::melt() %>% ggplot()+geom_tile(aes(x=evapcooler,y=centralair,fill=value))+theme_minimal()
puebr4$GEOID

geogroups<-puebr4 %>% group_by(GEOID) %>% summarize(sqft=sum(SquareFootage,na.rm=T),cooler=sum(evapcooler,na.rm=T),ac=sum(centralair,na.rm=T),permited=sum(mech.permit,na.rm=T),n=n())

blockgroup<-tigris::block_groups(state="CO",county="101")
blockgroup<-left_join(blockgroup,as.data.frame(geogroups) %>% select(-geometry))
library(cartogram)
#blockgroup<-blockgroup %>% st_transform(st_crs(2232))
blockgroup<-blockgroup[is.na(blockgroup$n)==F,]
blockgroup$logn<-log(blockgroup$n)
pueb_cart <- 
  cartogram_cont(blockgroup,
                 weight = "logn")
rosm::osm.types()
library(ggspatial)
?annotation_map_tile
ggplot(blockgroup  %>% sf::st_zm() %>% st_crop(., xmin = -104.5, xmax = -104.8, ymin = 38.1, ymax = 38.4))+annotation_map_tile(type="cartolight",zoom=12)+geom_sf(aes(fill=ac/n),alpha=.5)+scale_fill_viridis_c()+theme_minimal() 

ggplot(blockgroup %>% st_crop(., xmin = -104.5, xmax = -104.8, ymin = 38.1, ymax = 38.4))+geom_sf(aes(fill=permited/n))+scale_fill_viridis_c()+theme_minimal() 

rosm::osm.types()
coolers<-ggplot(blockgroup %>% st_crop(., xmin = -104.5, xmax = -104.8, ymin = 38.1, ymax = 38.4))+annotation_map_tile(type="cartolight",zoom=12)+geom_sf(aes(fill=cooler/n),alpha=.5)+scale_fill_viridis_c("evaporative cooler %")+ggthemes::theme_map() +theme(text=element_text(size=22))
centralair<-ggplot(blockgroup %>% st_crop(., xmin = -104.5, xmax = -104.8, ymin = 38.1, ymax = 38.4))+annotation_map_tile(type="cartolight",zoom=12)+geom_sf(aes(fill=ac/n),alpha=.5)+scale_fill_viridis_c("central air %")+ggthemes::theme_map()+scale_alpha(guide=F)+theme(text=element_text(size=22))
gridExtra::grid.arrange(coolers,centralair,nrow=1)


left_join(tigris::block_groups(state="CO",county="101"),puebr4  %>% as.data.frame() %>% select(GEOID,placeidinla) %>% unique() %>% left_join(.,m1$summary.random$placeidinla %>% mutate(placeidinla=ID))) %>% st_crop(., xmin = -104, xmax = -105, ymin = 38.1, ymax = 38.4) %>% ggplot()+geom_sf(aes(fill=`0.5quant`))+scale_fill_viridis_c("mechanical permit\nln odds\nblock group")+theme_minimal()
spteffectmean<-sampled_homes %>% left_join(.,m1$summary.random$placeidinla %>% mutate(placeidinla=ID)) %>% st_sf(.,sf_column_name="geoms") %>% ggplot()+annotation_map_tile(type="cartolight",zoom=12)+geom_sf(shape=3,aes(colour=`0.5quant`))+scale_colour_viridis_c("spatial re median")+theme_minimal()+theme(legend.position=c(.8,.6))
pointplot<-cbind(puebr4 %>% select(id),m1$summary.fitted.values %>% select(`0.5quant`))
pointplot$insample<-pointplot$id%in%sampled_homes$id

obsplot<-ggplot(pointplot %>% filter(insample==T))+annotation_map_tile(type="cartolight",zoom=12)+geom_sf(aes(colour=X0.5quant),shape=3)+scale_colour_viridis_c("observation\n fitted probability of permit")+theme_minimal()+theme(legend.position=c(.8,.6))

gridExtra::grid.arrange(spteffectmean,obsplot,nrow=1)

sampled_homes %>% left_join(.,m1$summary.random$placeidinla %>% mutate(placeidinla=ID)) %>% st_sf(.,sf_column_name="geoms") %>% ggplot()+annotation_map_tile(type="cartolight",zoom=12)+geom_sf(shape=3,aes(colour=`0.5quant`))+scale_colour_viridis_c("spatial re median")+theme_minimal()+theme(legend.position=c(.7,.6))

sampled_homes 

sampled_homes %>% filter(stringr::str_detect(Workclass,"A\\/C|AC")) %>% ggplot()+geom_bar(aes(x=Contractor))+coord_flip()

sampled_homes %>% filter(stringr::str_detect(Workclass,"Furnace")) %>% ggplot()+geom_bar(aes(x=Contractor))+coord_flip()

sampled_homes %>% filter(stringr::str_detect(Workclass,"HVAC")) %>% ggplot()+geom_bar(aes(x=Contractor))+coord_flip()


puebr5$Description

puebr5$Workclass %>% stringr::str_detect("ashp")
log(puebr4$SquareFootage)
m1$summary.fixed


library(tidycensus)

govaid.sample<-filter(puebr5, Year==2022, stringr::str_detect(Contractor,"FLOW RIGHT|PUEBLO COUNTY")) %>% filter(.,ASSESSOR_PARID%in%oldamps$ASSESSOR_PARID==F)
write.csv(missed_parcels,"general_sample_missing_in_hoods.2.6.csv")
write.csv(govaid.sample,"2022govaid.2.6.csv")



