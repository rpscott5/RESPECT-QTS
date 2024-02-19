library(plyr)
library(dplyr)
library(tidyverse)
library(jsonlite)
library(purrr)

puebrecs<-list.files("Documents/qt-local/property_api_calls/",full.names = T)
puebr1<-lapply(puebrecs, function(X) X %>% readRDS %>% unlist() %>% t() %>% as.data.frame(make.names=T))


puebr1<-puebr1 %>% rbind.fill()

puebr2<-puebr1 %>% select(id,
                                               nbhd,
                                               ctx,
                                               cty,
                                               parcel.sections.YearBuilt,
                                               parcel.sections.EffectiveYear,
                                               parcel.sections.Grade,parcel.sections.FullBaths,
                                               parcel.sections.HalfBaths,
                                               parcel.sections.Bedrooms,parcel.sections.SquareFootage,
                                               parcel.sections.Style,
                                               parcel.sections.LandClassDescription,
                          parcel.sections.ActualValue,
                                               parcel.sections.SaleDate,parcel.sections.SalePrice,
                                               parcel.sections.Acreage,
                                               parcel.sections.Grantees,
                                               parcel.sections.Grantors,
                                               parcel.sections.Description,
                                               parcel.sections.Units)
puebr2$parcel.sections.YearBuilt[which(as.numeric(puebr2$parcel.sections.YearBuilt)>2023)]<-NA
puebr2$parcel.sections.YearBuilt[which(as.numeric(puebr2$parcel.sections.YearBuilt)<1800)]<-NA
puebr2$parcel.sections.SquareFootage <- puebr2$parcel.sections.SquareFootage %>% gsub(" sqft","",.) %>% gsub(",","",.) %>% as.numeric()
puebr2<-filter(puebr2,parcel.sections.LandClassDescription=="Residential")
puebr2$parcel.sections.SaleDate<-lubridate::mdy(puebr2$parcel.sections.SaleDate)
colnames(puebr2)<-gsub("parcel.sections.","",colnames(puebr2))
puebp<-list.files("Documents/dbc/pueblo_permits",full.names = T)
pueb_parcels<-readRDS('Documents/dbc/qts_data/pueblo_parcels.rds')
puebp<-lapply(puebp,function(X) {
  rd1<-readRDS(X)
  if(ncol(rd1)>9){
    colnames(rd1)<-rd1[1,1:10] %>% as.character() %>% gsub(" ","_",.) %>% gsub("\\.","",.)
    rd1<-rd1[-1,]}
  rd1})
puebp<-puebp[sapply(puebp,nrow)>0]

puebp<-bind_rows(puebp)

puebp$Contractor<-puebp$Contractor %>% stringr::str_squish() %>% gsub("\\.","",.)
puebp$Contractor[stringr::str_detect(puebp$Contractor,"0000")]<-"SELF"
puebp<-filter(puebp,Type%in%c("Complaint","Inspection","Sign","Manufactured Home","Information")==F) 


contractors<-read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vTv4axcm1jiPeZb2NxOlldnnjDLqalCuU5T02Qy01rJyUKLzXceIXiHDrWkcjTrckQ3l44AlLmjZ7KC/pub?gid=1448195501&single=true&output=csv")

puebp$Date<-lubridate::ymd(puebp$Date)
puebp<-filter(puebp,lubridate::year(Date)>2009)
library(plyr)
library(dplyr)
library(rvest)
puebp$Year<-puebp$Date %>% lubridate::year()
puebppage<-read_html("https://www.prbd.com/reports/index.php")
permit_types<-puebppage %>% html_element("select") %>% html_children() %>% html_text()
permit_types<-data.frame("permit_types"=permit_types,"permit_type_id"=stringr::str_extract(permit_types,"^\\w+"))
permit_types<-na.omit(permit_types)
pueb_address<-esri2sf::esri2sf("https://maps.co.pueblo.co.us/outside/rest/services/Landbase/PuebloCounty_AddressPoints/MapServer/0")
puebp$Street_No<-puebp$Street_No %>% as.numeric()
head(puebp$Street)
puebp$direction<-puebp$Street %>% stringr::str_extract(.," [NESW][. ]") %>% stringr::str_extract("[NESW]")
pueb_address$direction<-pueb_address$STPREDIR %>% stringr::str_extract("^[NESW]")
puebp$simplestreet<-puebp$Street %>% stringr::str_extract("\\w+")
pueb_address_part<-pueb_address %>% as.data.frame() %>% select(ADDRID,ADDRNUM,STNAME,direction) %>% dplyr::rename(Street_No=ADDRNUM,simplestreet=STNAME)

puebp$cleanadd<-ifelse(is.na(puebp$direction),paste(puebp$Street_No,puebp$simplestreet),paste(puebp$Street_No,puebp$direction,puebp$simplestreet))
pueb_address_part$cleanadd<-ifelse(is.na(pueb_address_part$direction),paste(pueb_address_part$Street_No,pueb_address_part$simplestreet),paste(pueb_address_part$Street_No,pueb_address_part$direction,pueb_address_part$simplestreet))

puebp2<-filter(puebp,is.na(puebp$Street_No)==F)%>% left_join(.,pueb_address_part %>% select(ADDRID,cleanadd) %>% distinct(.,cleanadd,.keep_all=TRUE))

puebp2<-merge(puebp2,pueb_address %>% select(ADDRID,ASSESSOR_PARID) %>% distinct(ADDRID,.keep_all=T))
puebp2<-filter(puebp2,Year==2022)
puebp2<-filter(puebp2,Type=="Mechanical")
puebp2<-filter(puebp2,stringr::str_detect(Workclass," AC |A\\/C|Furnace|Furnace\\/AC|split|mini|Split|SPLIT|Mini|MINI|Coooler|EVAP|COOLER"))
pueb_parcels$ASSESSOR_PARID<-pueb_parcels$PAR_TXT
puebr2$ASSESSOR_PARID<-puebr2$id

puebr2$mech.permit<-as.numeric(puebr2$ASSESSOR_PARID)%in%as.numeric(puebp2$ASSESSOR_PARID)
puebr2$mech.permit<-as.numeric(puebr2$mech.permit)

puebr2$ActualValue<-puebr2$ActualValue %>% gsub("[\\$\\,]","",.) %>% as.numeric()
puebr2$sale22<-lubridate::year(puebr2$SaleDate)==2022


ebg<-sf::read_sf("Downloads/Colorado_EnviroScreen_v1_BlockGroup.geojson")
ebg<-filter(ebg,Cnt_N=="Pueblo County")

puebr2$ctx<-as.numeric(puebr2$ctx)
puebr2$cty<-as.numeric(puebr2$cty)
puebr2<-filter(puebr2,is.na(puebr2$cty)==F)
puebr2<-filter(puebr2,is.na(puebr2$ctx)==F)
puebr3 = sf::st_as_sf(puebr2, coords = c("ctx", "cty"),crs = 4326)
puebr3<-sf::st_intersection(puebr3,ebg)
puebr3$mech.permit<-NA
puebr3$mech.permit<-as.numeric(puebr3$ASSESSOR_PARID)%in%as.numeric(puebp2$ASSESSOR_PARID)
puebr3$mech.permit<-as.numeric(puebr3$mech.permit)
#Ld_x_ percentage of housing units built before 1960, as an indicator of potential exposure to lead
#noise Noise dba
#Hr___Heart Disease in Adults (percent)
#Lf_xp Life Expectancy (years)
#Cncr_ Cancer Prevalence (percent)
#Pr_____ Less Than High School Education (percent)
#POC people of color
# LI Low Income (percent)
# HCB Housing Cost Burdened (percent)



#buildingapis


puebrecs<-list.files("Documents/qt-local/building_api_calls/",full.names = T)

puebr1<-lapply(puebrecs, function(X) X %>% readRDS)

puebr1<-sapply(lapply(puebr1,function(X){unlist(X)[stringr::str_which(unlist(X),"Heating/Cooling")+1]}),function(K) paste(unique(K),sep=",",collapse=","))
buildingd<-data.frame("ASSESSOR_PARID"=basename(puebrecs) %>% gsub(".rds","",.))
buildingd$ebaseboard<-puebr1 %>% stringr::str_detect("Electric Baseboard") %>% as.numeric()
buildingd$centralair<-puebr1 %>% stringr::str_detect("Refrigerated Air") %>% as.numeric()
buildingd$evapcooler<-puebr1 %>% stringr::str_detect("Evaporative Cooler") %>% as.numeric()
buildingd$hotwater<-puebr1 %>% stringr::str_detect("Hot Water") %>% as.numeric()
puebr4<-left_join(puebr3,buildingd)

library(INLA)
puebr4$lastsalepre2012<-lubridate::year(puebr4$SaleDate)<2012

ebnb<-ebg %>% spdep::poly2nb()
ebg$placeidinla<-1:nrow(ebg)

ebnb.inla<-spdep::nb2INLA("Documents/qt-local/mechanicals/ebnb.adj",ebnb)
puebr4<-left_join(puebr4,ebg %>% as.data.frame() %>% select(OBJECTID,placeidinla))
puebr4$decadebuilt<-puebr4$YearBuilt %>% as.numeric() %>% round(-1)
puebr4$YearBuilt<-ifelse(as.numeric(puebr4$YearBuilt)<1900,1900,as.numeric(puebr4$YearBuilt))
puebr4$Grantees[is.na(puebr4$Grantees)]<-" "
puebr4$Grantees[which(puebr4$Grantees=="SAME")]<-puebr4$Grantors[which(puebr4$Grantees=="SAME")]
puebr4$Grantees[1:10]
puebr4$GranteesLast<-stringr::str_squish(puebr4$Grantees) %>% stringr::str_extract("^\\w+")
puebr4$GranteesLast[is.na(puebr4$GranteesLast)]<-" "
leth<-rethnicity::predict_ethnicity(lastnames = puebr4$GranteesLast,method="lastname")
puebr4$prob_hispanic<-leth$prob_hispanic
puebr4$newbuild<-puebr4$YearBuilt==2022
puebr4$ybcopy<-puebr4$YearBuilt
library(plyr)
library(dplyr)
library(ggplot2)
library(sf)
puebr4 %>% head()
ggplot(puebr4)+geom_sf(aes(colour=as.numeric(EffectiveYear)),size=.2)+scale_colour_viridis_c()+theme_minimal()
puebr4$mech.permit
ggplot(puebr4 %>% filter(mech.permit==T))+geom_sf(size=.5)+scale_colour_viridis_c()+theme_minimal()
library(raster)
puebrast<-st_make_grid(puebr4, n=c(300,100))
install.packages("stars")
puebrast<-data.frame("rastid"=1:length(puebrast),"geometry"=puebrast)
puebrast<-puebrast %>% st_sf()
puebrastpoints<-puebr4 %>% mutate(Year=as.numeric(YearBuilt)) %>% dplyr::select(Year,mech.permit)
puebrastpoints<-na.omit(puebrastpoints)
puebrastpointsi<-st_intersects(puebrastpoints,puebrast)
puebrastpoints$rastid<-puebrast$rastid[puebrastpointsi %>% sapply(.,function(X) X[1])]
puebrast<-puebrastpoints %>% as.data.frame %>% group_by(rastid) %>% summarize(Year=mean(Year),ns=n(),mech=sum(mech.permit)) %>% right_join(puebrast) %>% st_sf()
install.packages("sf")
library(remotes)
install_github("r-spatial/sf")
yrast<-stars::st_rasterize(puebrast %>% dplyr::select(Year, geometry))

# export as tiff
write_stars(r.enn2mean, "enn2mean.tif")
library(plyr)
library(dplyr)
library(sf)
library(ggplot2)
library("ggmap")
install.packages("basemaps")
library(basemaps)
library(ggspatial)
lay1<-
lay1<-basemap_gglayer(puebrast, map_service = "osm_stamen", map_type = "toner")
base1<-basemap_magick(puebrast, map_service = "osm_stamen", map_type = "toner")
map1<-ggplot(puebrast %>% na.omit())+annotation_map_tile(zoom=12,type="stamenbw")+geom_sf(aes(fill=Year,colour=Year))+scale_fill_viridis_c("Year Built, mean")+scale_colour_viridis_c("Year Built, mean")+theme_minimal()+ggthemes::theme_map()+theme(legend.position = c(.8,.1))+theme(plot.background = element_rect(colour="black"))
map2<-ggplot(puebrast %>% na.omit())+annotation_map_tile(zoom=12,type="stamenbw")+geom_sf(alpha=0,colour="grey")+geom_sf(data=puebrast %>% mutate(x=log(mech/c(ns+1))) %>% na.omit() %>% filter(x!=-Inf),aes(fill=x,colour=x))+scale_fill_viridis_c("Mechanical Permits\nin 2022,\nln per house")+scale_colour_viridis_c("Mechanical Permits\nin 2022,\nln per house")+ggthemes::theme_map()+theme(legend.position = c(.8,.1))+theme(plot.background = element_rect(colour="black"))
map2
rosm::osm.types()
plot3<-ggplot(puebrast)+geom_point(aes(x=Year,y=log((mech+1)/c(ns+1))),size=.5)+geom_smooth(aes(x=Year,y=log((1+mech)/c(ns+1))),size=.5)+theme_minimal()+ylab("ln mechanical permits/house")+xlab("Year built")
cowplot::plot_grid(map1,map2,plot3, align = "v", nrow = 3)





library(snapKrig)
g1 = sk_snap(from=sf::st_geometry(puebrastpoints), list(gres=c(.001, .001)))
g_pred = sk_cmean(g1$gval, pars, X=0)

puebrastpoints %>% head()
puebrastid<-st_intersection(puebr4,puebrast)
puebrast
puebr4$lowfaircondition<-puebr4$Grade%in%c("Fair","Low")
puebr4

m1<-inla(mech.permit~HCB+
           Pr_____+
           f(YearBuilt,model="rw2")+
           lowfaircondition+
           evapcooler+
           centralair+
           ebaseboard+
           scale(prob_hispanic)+
           scale(log(as.numeric(SquareFootage)))+
           scale(log(as.numeric(ActualValue)))+
           newbuild+
           sale22+
           lastsalepre2012+
           f(placeidinla,model="bym",graph="Documents/qt-local/mechanicals/ebnb.adj"),
         data=puebr4,
         family="binomial")


fits<-m1$summary.fitted.values$mean


fits[which(puebr4$mech.permit==1)][sort(fits[which(puebr4$mech.permit==1)],index.return=T)$ix[1:50]]


fits[which(puebr4$mech.permit==1)][sort(fits[which(puebr4$mech.permit==1)],index.return=T)$ix[1:50]]
fframe<-data.frame("p"=fits)
fframe$mechp<-puebr4$mech.permit


selectingcases<-ggplot(fframe)+geom_histogram(aes(x=log(fits),fill=as.factor(mechp),group=mechp))+facet_wrap(~mechp,scale="free_y",ncol=1)+theme_minimal()+geom_rect(aes(xmin=-3,xmax=-1,ymin=0,ymax=100,alpha=mechp),fill=NA,colour="black",lty=2)+geom_rect(aes(xmin=-8,xmax=-6,ymin=0,ymax=100,alpha=mechp),fill=NA,colour="black",lty=2)+scale_fill_viridis_d("Mechanical Permit")+theme(legend.position=c(.8,.8))+xlab("observation predicted probability of mechanical permit (based on model)")+geom_text(aes(x=-7,y=200,label=ifelse(mechp==1,"grid=selection for interviews","")))+scale_alpha_identity()+ylab("observations")

head(lsl)
probmodel<-m1$summary.fixed  %>% mutate(id=rownames(m1$summary.fixed)) %>% mutate(id=forcats::fct_recode(id,"house, hispanic owner, prob"="scale(prob_hispanic)","house, sqft,ln"="scale(log(as.numeric(SquareFootage)))","house, assessor value"="scale(log(as.numeric(ActualValue)))","house, sold 2022"="sale22TRUE","house, poor condition"="lowfairconditionTRUE","house, sold last pre-2012"="lastsalepre2012TRUE","block group, housing cost burdened"="HCB","house, evaporative cooler"="evapcooler","house, electric baseboard"="ebaseboard","house, central air"="centralair","block group, less than hs education"="Pr_____")) %>% filter(id!="newbuildTRUE") %>% ggplot()+geom_hline(aes(yintercept=0),lty=2)+geom_pointrange(aes(x=id,y=mean,ymin=`0.025quant`,ymax=`0.975quant`))+coord_flip()+theme_minimal()+ylab("95% fixed effect credible interval (DV=Mechanical Permit in 2022)")+xlab("variable")
cowplot::plot_grid(probmodel,selectingcases,ncol=1)
puebr4 %>% nrow()
e?fct_recode
lowprobabilityhomes<-puebr4[which(puebr4$mech.permit==1),] %>% .[sort(fits[which(puebr4$mech.permit==1)],index.return=T)$ix[1:50],] %>% select(ASSESSOR_PARID, Grantees,YearBuilt,id)
highprobabilityhomes<-puebr4[which(puebr4$mech.permit==1),] %>% .[sort(fits[which(puebr4$mech.permit==1)],index.return=T,decreasing=T)$ix[1:50],]  %>% select(ASSESSOR_PARID, Grantees,YearBuilt,id)
INLA::inla.doc("binomial")
INLA::inla.doc("iid")
INLA::inla.doc("z")
?scale
lowprobpermits<-filter(puebp2, ASSESSOR_PARID%in%lowprobabilityhomes$ASSESSOR_PARID)
highprobpermits<-filter(puebp2, ASSESSOR_PARID%in%highprobabilityhomes$ASSESSOR_PARID)
rbind(lowprobpermits) %>% as.data.frame() %>% select(Permit_No,Street, Workclass,Owner,Contractor,Phone,cleanadd, ASSESSOR_PARID) %>% write.csv("Documents/qt-local/low_probability_addresses_test.csv")
rbind(highprobpermits) %>% as.data.frame() %>% select(Permit_No,Street, Workclass,Owner,Contractor,Phone,cleanadd, ASSESSOR_PARID) %>% write.csv("Documents/qt-local/high_probability_addresses_test.csv")







m1$summary.random$YearBuilt %>% ggplot()+geom_pointrange(aes(x=ID,y=mean,ymin=`0.025quant`,ymax=`0.975quant`))+theme_minimal()

m1$summary.random$ybcopy %>% ggplot()+geom_pointrange(aes(x=ID,y=mean,ymin=`0.025quant`,ymax=`0.975quant`))+theme_minimal()

m1$summary.random$placeidinla %>% ggplot()+geom_pointrange(aes(x=ID,y=mean,ymin=`0.025quant`,ymax=`0.975quant`))+theme_minimal()

m1$summary.fixed %>% mutate(ID=rownames(m1$summary.fixed)) %>% ggplot()+geom_pointrange(aes(x=ID,y=mean,ymin=`0.025quant`,ymax=`0.975quant`))+theme_minimal()+coord_flip()

rm1$summary.random$decadebuilt %>% ggplot()+geom_pointrange(aes(x=ID,y=mean,ymin=`0.025quant`,ymax=`0.975quant`))

table(puebr4$centralair)

lm(mech.permit~HCB+Pr_____+as.numeric(YearBuilt)+c(Grade=="Fair")+evapcooler+centralair+ebaseboard+log(as.numeric(SquareFootage))+log(as.numeric(ActualValue))+sale22+lastsalepre2008,data=puebr4) %>% summary()

lm(mech.permit~HCB+Pr_____+YearBuilt+lowfaircondition+evapcooler+centralair+ebaseboard+scale(prob_hispanic)+scale(log(as.numeric(SquareFootage)))+scale(log(as.numeric(ActualValue)))+newbuild+sale22+lastsalepre2012, data=puebr4) %>% summary()






3https://image-cdn.spatialest.com/file/co-pueblo-images/Documents/NOVs/331006034-2023.pdf

parcals$PAR_NUM %>% max()

https://co-pueblo-citizen.comper.info/ajax/getInfoJSON.ashx?propID=1401000015_20211013
https://co-pueblo-citizen.comper.info/ajax/getInfoJSON.ashx?subjID=331006034&propID=330001015&propID=330003026

https://co-pueblo-citizen.comper.info/ajax/getPropertyImage.ashx?parid=1401000015



https://co-pueblo-citizen.comper.info/ajax/getSubjectJSON.ashx?subjID=9625001001

j1<-jsonlite::read_json("https://property.spatialest.com/co/pueblo/api/v1/recordcard/9625001001")

https://property.spatialest.com/co/pueblo/api/v1/recordcard/9625001001
https://api.spatialest.com/v1/co/pueblo/buildings/9625001001

https://api.spatialest.com/v1/co/pueblo/sale-summary/9625001001
install_github("r-spatial/sf")


j1


https://co-pueblo-citizen.comper.info/ajax/getCompsJSON.ashx?PropertyList=330003026,9625001001
