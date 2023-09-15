library(plyr)
library(dplyr)
library(rvest)
do1<-list.files("Documents/qt-local/mechanicals/property_pages/",full.names = T)

ht1e<-lapply(do1,function(X) {try({read_html(X) %>% html_table })})
names(ht1e)<-basename(do1) %>% gsub(".html","",.)

improvements<-sapply(ht1e,function(demo){try({
colnames(demo[[1]])[1]<-"var"
"IMP"%in%demo[[1]]$var})
})
ht1e<-ht1e[names(which(improvements=="TRUE"))]
ht1e<-ht1e[sapply(ht1e, length)>=7]

ht1b<-lapply(ht1e, function(X) {X[[2]] %>% pivot_wider(names_from=`Detail Type`,values_from=c(Detail,Count),values_fn = ~paste(.x,sep=";",collapse=";"))}) 
ht1b<-ht1b %>% bind_rows(.id="PAR_NUM")
saveRDS(ht1b,"Documents/qt-local/mechanicals/housedetails_pages.rds")

demo1<-lapply(do1,function(X) {try({
  demo<-read_html(X) 
  yb<-demo %>% html_nodes("#improvements .col-12") %>% html_text() %>% stringr::str_extract(.,"Year Built[0-9]+") %>% stringr::str_extract(.,"[0-9]+") %>% unique() %>% na.omit()  %>% as.numeric()
  yr<-demo %>% html_nodes("#improvements .col-12") %>% html_text() %>% stringr::str_extract(.,"Year Remodeled[0-9]+") %>% stringr::str_extract(.,"[0-9]+") %>% unique() %>% na.omit()  %>% as.numeric()
  st1<-demo %>% html_nodes("#improvements .col-12") %>% html_text() %>% stringr::str_extract(.,"Style[\\w ]+") %>% gsub("Style","",.) %>% unique() %>% na.omit()  %>% as.character()
  br1<-demo %>% html_nodes("#improvements .col-12") %>% html_text() %>% stringr::str_extract(.,"Bedrooms[0-9]+") %>% stringr::str_extract(.,"[0-9]+") %>% unique() %>% na.omit()  %>% as.numeric()
  br2<-demo %>% html_nodes("#improvements .col-12") %>% html_text() %>% stringr::str_extract(.,"Full Baths[0-9]+") %>% stringr::str_extract(.,"[0-9]+") %>% unique() %>% na.omit()  %>% as.numeric()
  br3<-demo %>% html_nodes("#improvements .col-12") %>% html_text() %>% stringr::str_extract(.,"Half Baths[0-9]+") %>% stringr::str_extract(.,"[0-9]+") %>% unique() %>% na.omit()  %>% as.numeric()
cbind("year_built"=yb,"year_remodelled"=yr,"style"=st1,"bedrooms"=br1,"baths"=br2,"hbaths"=br3)
})})
names(demo1)<-basename(do1) %>% gsub(".html","",.)
demo1<-lapply(demo1,as.data.frame)

demo2<-bind_rows(demo1,.id="PAR_NUM")
#saveRDS(demo2,"Documents/qt-local/mechanicals/mech_property_pages.rds")
demo2<-readRDS("Documents/qt-local/mechanicals/mech_property_pages.rds")
pueb_parcels<-readRDS('Documents/dbc/qts_data/pueblo_parcels.rds')
library(dplyr)
parcals<-left_join(pueb_parcels,demo2 %>% mutate(PAR_NUM=as.numeric(PAR_NUM))) %>% left_join(.,ht1b %>% mutate(PAR_NUM=as.numeric(PAR_NUM)))
parcals<-filter(parcals,ImprovementsAssessedValue>0)
parcals<-filter(parcals,is.na(year_built)==F)
head(parcals)
parcals$heating_forced_air<-stringr::str_detect(parcals$`Detail_Heating/Cooling`,"Forced Air")
parcals$heating_gravity<-stringr::str_detect(parcals$`Detail_Heating/Cooling`,"Gravity")
parcals$heating_wall<-stringr::str_detect(parcals$`Detail_Heating/Cooling`,"Wall Furnace")
parcals$heating_electric_baseboard<-stringr::str_detect(parcals$`Detail_Heating/Cooling`,"Electric Baseboard")
parcals$heating_radiant<-stringr::str_detect(parcals$`Detail_Heating/Cooling`,"Radiant")
parcals$heating_hotwater_baseboard<-stringr::str_detect(parcals$`Detail_Heating/Cooling`,"Radiant")
parcals$cooling_wall_ac<-stringr::str_detect(parcals$`Detail_Heating/Cooling`,"Wall Air")
parcals$cooling_evap<-stringr::str_detect(parcals$`Detail_Heating/Cooling`,"Evaporative")
parcals$central_ac<-stringr::str_detect(parcals$`Detail_Heating/Cooling`,"Refrigerated")
parcals %>% as.data.frame() %>% select(PAR_NUM,heating_forced_air,cooling_wall_ac,central_ac,heating_electric_baseboard)

#end here class

parcals_c<-sf::st_centroid(parcals)
lubridate::ymd("1999-01-01")
parcals_c$year_built<-paste0(parcals_c$year_built,"-01-01") %>% lubridate::ymd() %>% lubridate::year()
parcals_c$year_built<-ifelse(parcals_c$year_built>2024,NA,parcals_c$year_built)
ggplot(parcals_c)+geom_sf(aes(colour=year_built))+scale_fill_viridis_c()



ggplot(parcals_c)+geom_sf(aes(colour=central_ac))


filter(permit_join, HEPUMP==T,Type=="Mechanical") %>% ggplot()+geom_bar(aes(x=`Detail_Heating/Cooling`))+coord_flip()


table(fancy_only$PAR_NUM%in%parcals$PAR_NUM)


permit_in_parcal<-filter(fancy_only, PAR_NUM%in%parcals$PAR_NUM)
permit_join<-inner_join(permit_in_parcal,as.data.frame(parcals))

ggplot()+geom_bar(aes(x=permit_join$`Detail_Heating/Cooling`),state="count")+coord_flip()
permit_join$heating_forced_air<-stringr::str_detect(permit_join$`Detail_Heating/Cooling`,"Forced Air")
permit_join$heating_gravity<-stringr::str_detect(permit_join$`Detail_Heating/Cooling`,"Gravity")
permit_join$heating_wall<-stringr::str_detect(permit_join$`Detail_Heating/Cooling`,"Wall Furnace")
permit_join$heating_electric_baseboard<-stringr::str_detect(permit_join$`Detail_Heating/Cooling`,"Electric Baseboard")
permit_join$heating_radiant<-stringr::str_detect(permit_join$`Detail_Heating/Cooling`,"Radiant")
permit_join$heating_hotwater_baseboard<-stringr::str_detect(permit_join$`Detail_Heating/Cooling`,"Radiant")
permit_join$cooling_wall_ac<-stringr::str_detect(permit_join$`Detail_Heating/Cooling`,"Wall Air")
permit_join$cooling_evap<-stringr::str_detect(permit_join$`Detail_Heating/Cooling`,"Evaporative")
permit_join$central_ac<-stringr::str_detect(permit_join$`Detail_Heating/Cooling`,"Refrigerated")
permit_join$ev_cool<-stringr::str_detect(permit_join$`Detail_Heating/Cooling`,"Evaporative")

filter(permit_join, HEPUMP==T,Type=="Mechanical") %>% ggplot()+geom_bar(aes(x=`Detail_Heating/Cooling`))+coord_flip()



ggplot(filter(permit_join, HEPUMP==T,Type=="Mechanical"))+geom_bar(data=permit_join,aes(x=`Detail_Heating/Cooling`))+geom_bar(aes(x=`Detail_Heating/Cooling`),fill="dark red")+coord_flip()+theme_minimal()



#improvements .col-12




ht1b$`Detail_Heating/Cooling` %>% stringr::str_split(";") %>% unlist() %>% table()

ht1e %>% html_node(".img-fluid") %>% html_attr("src")
