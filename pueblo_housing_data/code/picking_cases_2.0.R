library(plyr)
library(dplyr)
library(tidyverse)
library(jsonlite)
library(purrr)
library(rvest)
library(INLA)

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


cobus<-read.csv("Documents/qt-local/Business_Entities_in_Colorado.csv")
trade_names<-read.csv("Documents/qt-local/Trade_Names_for_Businesses_in_Colorado.csv")
table(trade_names$masterTradenameId)
tradenames<- trade_names %>% select(masterTradenameId,firstName,lastName,registrantOrganization,zipCode)
colnames(tradenames)<-c("entityid","agentfirstname","agentlastname","entityname","principalzipcode")
which(tradenames$masterTradenameId==14481122427)

cobus<-cobus %>% select(entityid,agentfirstname,agentlastname,entityname,principalzipcode) 
cobus<-rbind(tradenames,cobus) %>% unique()
head(cobus)
#matchin<-read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vQPCy10zqgGetP69c-Zmkb7Bc3hwvA6LqkfnQt5G8jGG6z4bCsDB2tfIHqAUKyOYklcfm5j7tjE2eLd/pub?gid=1198396725&single=true&output=csv") %>% dplyr::rename(Contractor=Pueblo.Name)

#matchin<-matchin %>% mutate(entityid=colorado) %>% select(-colorado) %>% left_join(.,cobus)
#matchin[154,]
#matchin<-filter(matchin,is.na(agentlastname)==F)
#matchin.missing<-filter(matchin,is.na(agentlastname))
#matchin.missing2<-matchin.missing

#for(X in 26:nrow(matchin.missing2)){ 
 # try({
  #  Sys.sleep(3)
#temp<-rvest::read_html(paste0("https://www.sos.state.co.us/biz/BusinessEntityDetail.do?entityId2=&masterFileId=",matchin.missing2$entityid[X]))
#temp.last<-temp %>% html_node("td td tr:nth-child(3)") %>% html_text() %>% gsub("Registrant name","",.) %>% stringr::str_trim() %>% stringr::str_extract("\\w+$")
#temp.address<-temp %>% html_nodes("td td tr:nth-child(7) td") %>% html_text() 
#temp.address<-temp.address[2]
#temp.address<-stringr::str_extract(temp.address,"8[0-9][0-9][0-9][0-9]")
#matchin.missing2$agentlastname[X]<-temp.last
#matchin.missing2$principalzipcode[X]<-temp.address
#})
#  gc()
#}
#matchin.missing2$agentlastname<-ifelse(matchin.missing2$agentlastname=="number",NA,matchin.missing2$agentlastname)
#matchin.missing2$agentlastname<-ifelse(stringr::str_detect(matchin.missing2$agentlastname,"[0-9]"),NA,matchin.missing2$agentlastname)
#matchin<-rbind(matchin,matchin.missing2)
#write.csv(matchin,"contractorsprocessed.csv")
matchin<-read.csv("contractorsprocessed.csv")

matchin<-matchin %>% select(-X)
puebp<-left_join(puebp, matchin)
puebp$Date<-lubridate::ymd(puebp$Date)
puebp<-filter(puebp,lubridate::year(Date)>2009)


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
puebp2<-filter(puebp2,Year%in%c(2018,2019,2020,2021,2022))
puebp2<-filter(puebp2,Type=="Mechanical")
puebp2<-filter(puebp2,stringr::str_detect(Workclass," AC |A\\/C|Furnace|Furnace\\/AC|split|mini|Split|SPLIT|Mini|MINI|Coooler|EVAP|COOLER"))
pueb_parcels$ASSESSOR_PARID<-pueb_parcels$PAR_TXT
puebr2$ASSESSOR_PARID<-puebr2$id

puebp2<-left_join(puebp2,pueb_address %>% select(ASSESSOR_PARID, ADDRID), na_matches="never")

puebr2$mech.permit<-as.numeric(puebr2$ASSESSOR_PARID)%in%as.numeric(puebp2$ASSESSOR_PARID)
puebr2$mech.permit<-as.numeric(puebr2$mech.permit)


puebr2$ActualValue<-puebr2$ActualValue %>% gsub("[\\$\\,]","",.) %>% as.numeric()

puebr2$sale22<-lubridate::year(puebr2$SaleDate)==2022

puebr2$sale1821<-lubridate::year(puebr2$SaleDate)%in%c(2018:2021)


ebg<-sf::read_sf("Downloads/Colorado_EnviroScreen_v1_BlockGroup.geojson")
ebg<-filter(ebg,Cnt_N=="Pueblo County")

puebr2$ctx<-as.numeric(puebr2$ctx)
puebr2$cty<-as.numeric(puebr2$cty)
puebr2<-filter(puebr2,is.na(puebr2$cty)==F)
puebr2<-filter(puebr2,is.na(puebr2$ctx)==F)
puebr3 = sf::st_as_sf(puebr2, coords = c("ctx", "cty"),crs = 4326)
puebr3<-sf::st_intersection(puebr3,ebg)
puebr3$mech.permit %>% table()

puebr3$mech.permit<-NA
puebr3$mech.permit<-as.numeric(puebr3$ASSESSOR_PARID)%in%as.numeric(puebp2$ASSESSOR_PARID)
puebr3$mech.permit<-as.numeric(puebr3$mech.permit)

table(puebr3$mech.permit)

puebrecs<-list.files("Documents/qt-local/building_api_calls/",full.names = T)

puebr1<-lapply(puebrecs, function(X) X %>% readRDS)

puebr1<-sapply(lapply(puebr1,function(X){unlist(X)[stringr::str_which(unlist(X),"Heating/Cooling")+1]}),function(K) paste(unique(K),sep=",",collapse=","))
buildingd<-data.frame("ASSESSOR_PARID"=basename(puebrecs) %>% gsub(".rds","",.))
buildingd$ebaseboard<-puebr1 %>% stringr::str_detect("Electric Baseboard") %>% as.numeric()
buildingd$centralair<-puebr1 %>% stringr::str_detect("Refrigerated Air") %>% as.numeric()
buildingd$evapcooler<-puebr1 %>% stringr::str_detect("Evaporative Cooler") %>% as.numeric()
buildingd$hotwater<-puebr1 %>% stringr::str_detect("Hot Water") %>% as.numeric()
puebr4<-left_join(puebr3,buildingd,na_matches="never")





puebr4$lastsalepre2012<-lubridate::year(puebr4$SaleDate)<2012

ebnb<-ebg %>% spdep::poly2nb()
ebg$placeidinla<-1:nrow(ebg)

ebnb.inla<-spdep::nb2INLA("Documents/qt-local/mechanicals/ebnb.adj",ebnb)
puebr4<-left_join(puebr4,ebg %>% as.data.frame() %>% select(OBJECTID,placeidinla))
puebr4$decadebuilt<-puebr4$YearBuilt %>% as.numeric() %>% round(-1)
puebr4$YearBuilt<-ifelse(as.numeric(puebr4$YearBuilt)<1900,1900,as.numeric(puebr4$YearBuilt))
puebr4$Grantees[is.na(puebr4$Grantees)]<-" "
puebr4$Grantees[which(puebr4$Grantees=="SAME")]<-puebr4$Grantors[which(puebr4$Grantees=="SAME")]
puebr4$GranteesLast<-stringr::str_squish(puebr4$Grantees) %>% stringr::str_extract("^\\w+")
puebr4$GranteesLast[is.na(puebr4$GranteesLast)]<-" "
leth<-rethnicity::predict_ethnicity(lastnames = puebr4$GranteesLast,method="lastname")
puebr4$prob_hispanic<-leth$prob_hispanic
puebr4$newbuild<-puebr4$YearBuilt==2022
puebr4$ybcopy<-puebr4$YearBuilt
puebr4$saleinperiod<-as.numeric(puebr4$sale1821)+as.numeric(puebr4$sale22)>0
puebr4$lowfaircondition<-puebr4$Grade%in%c("Fair","Low")
puebr4$newbuild<-puebr4$YearBuilt>=2018

#contractupdate
contupt<-read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vQPCy10zqgGetP69c-Zmkb7Bc3hwvA6LqkfnQt5G8jGG6z4bCsDB2tfIHqAUKyOYklcfm5j7tjE2eLd/pub?gid=1198396725&single=true&output=csv")

 head(contupt)
#fitmodel


m1<-inla(mech.permit~scale(HCB)+
           scale(Pr_____)+
           f(YearBuilt,model="rw2")+
           lowfaircondition+
           evapcooler+
           centralair+
           ebaseboard+
           scale(prob_hispanic)+
           scale(log(as.numeric(SquareFootage)))+
           scale(log(as.numeric(ActualValue)))+
           saleinperiod+
           lastsalepre2012+
           f(placeidinla,model="bym",graph="Documents/qt-local/mechanicals/ebnb.adj"),
         data=puebr4,
         family="binomial")


puebr5<-filter(puebr4,mech.permit==1)

puebr5<-left_join(puebp2,puebr4 %>% mutate(ASSESSOR_PARID=as.numeric(ASSESSOR_PARID)),na_matches="never")
puebr5<-left_join(puebr5,contupt %>% mutate(Contractor=Pueblo.Name) %>% select(Contractor,EStarACCABPINCI),na_matches="never")
puebr5$EStarACCABPINCI<-puebr5$EStarACCABPINCI>0
puebr5$EStarACCABPINCI<-as.numeric(puebr5$EStarACCABPINCI)
puebr5<-unique(puebr5)

m1a<-inla(EStarACCABPINCI~scale(HCB)+
           scale(Pr_____)+
           f(YearBuilt,model="rw2")+
           lowfaircondition+
           evapcooler+
           centralair+
           ebaseboard+
           scale(prob_hispanic)+
           scale(log(as.numeric(SquareFootage)))+
           scale(log(as.numeric(ActualValue)))+
          saleinperiod+
           lastsalepre2012+
           f(placeidinla,model="bym",graph="Documents/qt-local/mechanicals/ebnb.adj"),
         data=puebr5,
         family="binomial")
summary(m1a)
library(basemaps)
library(ggspatial)
library(sf)


ebg %>% select(OBJECTID,placeidinla) %>% left_join(.,data.frame("placeidinla"=m1a$summary.random$placeidinla$ID,"mean"=m1a$summary.random$placeidinla$mean[1:134]+m1a$summary.random$placeidinla$mean[135:268])) %>% st_crop(census_data, xmin = -104.6, xmax = -104.7, ymin = 38.2, ymax = 38.3) %>% ggplot()+annotation_map_tile(zoom=13,type="stamenbw")+geom_sf(aes(fill=mean),alpha=.5)+scale_fill_viridis_c()+theme_minimal()+geom_sf_text(aes(label=placeidinla,colour=ifelse(placeidinla%in%c(30, 134, 68, 103,76,10,12,106,96,97,11,100),"selected","not selected")))+scale_colour_manual(name="sample", values=c("black","red"))

ebg %>% select(OBJECTID,placeidinla) %>% left_join(.,data.frame("placeidinla"=m1$summary.random$placeidinla$ID,"mean"=m1$summary.random$placeidinla$mean[1:134]+m1$summary.random$placeidinla$mean[135:268])) %>% st_crop(census_data, xmin = -104.6, xmax = -104.7, ymin = 38.2, ymax = 38.3) %>% ggplot()+annotation_map_tile(zoom=13,type="stamenbw")+geom_sf(aes(fill=mean),alpha=.5)+scale_fill_viridis_c()+theme_minimal()+geom_sf_text(aes(label=placeidinla,colour=ifelse(placeidinla%in%c(30, 134, 68, 103,76,10,12,106,96,97,11,100),"selected","not selected")))+scale_colour_manual(name="sample", values=c("black","red"))



filter(puebr5, placeidinla%in%c()) %>% as.data.frame() %>% select(-geometry,-geoms) %>% filter(Year==2022) %>% mutate(permitlink=paste0("https://www.prbd.com/searches/inspectionsearch1.php?pno=",Permit_No)) %>% write.csv("neighborhood_sample_contacts.csv")
filter(puebr5, placeidinla%in%c()) %>% as.data.frame() %>% select(-geometry,-geoms) %>% filter(Year==2022) %>% mutate(permitlink=paste0("https://www.prbd.com/searches/inspectionsearch1.php?pno=",Permit_No)) %>% write.csv("neighborhood_sample_contacts_batch2.csv")
filter(puebr5, placeidinla%in%c(30, 134, 68, 103,76,10)) %>% as.data.frame() %>% select(-geometry,-geoms) %>% filter(Year==2022) %>% mutate(permitlink=paste0("https://www.prbd.com/searches/inspectionsearch1.php?pno=",Permit_No)) %>% write.csv("neighborhood_sample_contacts_batch3.csv")

filter(puebr5, placeidinla%in%c(30, 134, 68, 103,76,10,12,106,96,97,11,100)) %>% as.data.frame() %>% select(-geometry,-geoms) %>% filter(Year==2022) %>% mutate(permitlink=paste0("https://www.prbd.com/searches/inspectionsearch1.php?pno=",Permit_No)) %>% write.csv("neighborhood_sample_contacts_batch_merged.csv")

filter(puebr5, placeidinla%in%c(30, 134, 68, 103,76,10,12,106,96,97,11,100)) %>% as.data.frame() %>% select(-geometry,-geoms) %>% filter(Year==2022) %>% mutate(permitlink=paste0("https://www.prbd.com/searches/inspectionsearch1.php?pno=",Permit_No)) %>% nrow()



filter(puebr5, placeidinla%in%c(30, 134, 68, 103,76,10,12,106,96,97,11,100)) %>% as.data.frame() %>% select(-geometry,-geoms) %>% filter(Year==2022) %>% mutate(permitlink=paste0("https://www.prbd.com/searches/inspectionsearch1.php?pno=",Permit_No)) %>% write.csv("neighborhood_sample_contacts_batch_merged.csv")

filter(puebr5, placeidinla%in%c(30, 134, 68, 103,76,10,12,106,96,97,11,100)) %>% as.data.frame() %>% select(-geometry,-geoms) %>% filter(Year==2022) %>% mutate(permitlink=paste0("https://www.prbd.com/searches/inspectionsearch1.php?pno=",Permit_No)) %>% nrow()



set.1129<-c(46,27,105,27,37,16,17,14,9,13)
filter(puebr5, placeidinla%in%set.1130) %>% as.data.frame() %>% select(-geometry,-geoms) %>% filter(Year==2022) %>% mutate(permitlink=paste0("https://www.prbd.com/searches/inspectionsearch1.php?pno=",Permit_No)) %>% write.csv("neighborhood_sample_contacts_batch_1129.csv")





nrow(puebr4)
ebg %>% select(OBJECTID,placeidinla) %>% left_join(.,data.frame("placeidinla"=m1a$summary.random$placeidinla$ID,"mean"=m1a$summary.random$placeidinla$mean[1:134]+m1a$summary.random$placeidinla$mean[135:268])) %>% mutate(sampled=placeidinla%in%c(11,100,12,96,97,106,30,134,68,103,76,10)) %>% st_crop(census_data, xmin = -104.6, xmax = -104.7, ymin = 38.2, ymax = 38.3) %>% ggplot()+annotation_map_tile(zoom=13,type="stamenbw")+geom_sf(aes(fill=mean,colour=sampled,size=sampled),alpha=.9)+scale_fill_viridis_c()+theme_minimal()+scale_colour_manual(values=c(NA,"white"))+scale_size_manual(values=c(.1,3))




probmodel<-m1$summary.fixed  %>% mutate(id=rownames(m1$summary.fixed)) %>% mutate(id=forcats::fct_recode(id,"house, hispanic owner, prob"="scale(prob_hispanic)","house, sqft,ln"="scale(log(as.numeric(SquareFootage)))","house, assessor value"="scale(log(as.numeric(ActualValue)))","house, sold in period"="saleinperiodTRUE","house, poor condition"="lowfairconditionTRUE","house, sold last pre-2012"="lastsalepre2012TRUE","block group, housing cost burdened"="scale(HCB)","house, evaporative cooler"="evapcooler","house, electric baseboard"="ebaseboard","house, central air"="centralair","block group, less than hs education"="scale(Pr_____)","house, built in period"="newbuildTRUE"),var="hvac mechanical permit")  %>% ggplot()+geom_hline(aes(yintercept=0),lty=2)+geom_pointrange(aes(x=id,y=mean,ymin=`0.025quant`,ymax=`0.975quant`))+coord_flip()+theme_minimal()+ylab("95% fixed effect credible interval (DV=Mechanical Permit in 2022)")+xlab("variable")

probmodel2<-m1a$summary.fixed  %>% mutate(id=rownames(m1$summary.fixed)) %>% mutate(id=forcats::fct_recode(id,"house, hispanic owner, prob"="scale(prob_hispanic)","house, sqft,ln"="scale(log(as.numeric(SquareFootage)))","house, assessor value"="scale(log(as.numeric(ActualValue)))","house, sold in period"="saleinperiodTRUE","house, poor condition"="lowfairconditionTRUE","house, sold last pre-2012"="lastsalepre2012TRUE","block group, housing cost burdened"="HCB","house, evaporative cooler"="evapcooler","house, electric baseboard"="ebaseboard","house, central air"="centralair","block group, less than hs education"="Pr_____","house, built in period"="newbuildTRUE"),var="permit, quality installation")  %>% ggplot()+geom_hline(aes(yintercept=0),lty=2)+geom_pointrange(aes(x=id,y=mean,ymin=`0.025quant`,ymax=`0.975quant`))+coord_flip()+theme_minimal()+ylab("95% fixed effect credible interval (DV=Mechanical Permit in 2022)")+xlab("variable")
options(warn=1)
jointplot<-rbind(m1a$summary.fixed  %>% mutate(id=rownames(m1a$summary.fixed)) %>% mutate(id=forcats::fct_recode(id,"house, hispanic owner, prob"="scale(prob_hispanic)","house, sqft,ln"="scale(log(as.numeric(SquareFootage)))","house, assessor value"="scale(log(as.numeric(ActualValue)))","house, sold in period"="saleinperiodTRUE","house, poor condition"="lowfairconditionTRUE","house, sold last pre-2012"="lastsalepre2012TRUE","block group, housing cost burdened"="scale(HCB)","house, evaporative cooler"="evapcooler","house, electric baseboard"="ebaseboard","house, central air"="centralair","block group, less than hs education"="scale(Pr_____)"),var="permit, quality installation"),m1$summary.fixed  %>% mutate(id=rownames(m1$summary.fixed)) %>% mutate(id=forcats::fct_recode(id,"house, hispanic owner, prob"="scale(prob_hispanic)","house, sqft,ln"="scale(log(as.numeric(SquareFootage)))","house, assessor value"="scale(log(as.numeric(ActualValue)))","house, sold in period"="saleinperiodTRUE","house, poor condition"="lowfairconditionTRUE","house, sold last pre-2012"="lastsalepre2012TRUE","block group, housing cost burdened"="scale(HCB)","house, evaporative cooler"="evapcooler","house, electric baseboard"="ebaseboard","house, central air"="centralair","block group, less than hs education"="scale(Pr_____)"),var="hvac mechanical permit")) %>% filter(id!="(Intercept)") %>% ggplot()+geom_hline(aes(yintercept=0),lty=2)+geom_pointrange(aes(x=id,y=mean,ymin=`0.025quant`,ymax=`0.975quant`,colour=var),position = position_dodge(width=.5))+coord_flip()+theme_minimal()+ylab("95% fixed effect credible interval (DV=Mechanical Permit 2018:2022)")+xlab("variable")+theme(legend.position="top")


jointplot

#home value linked to applying for permits, not to a high quality installer.

#a sale is the strongest predictor aof a permit but comes with a decreased odds of quality installation.

#low education-->lower quality installs.

#Hispanic homeowners less likely to get a "quality installer"

#homes in poor condition are least likely to get a permit, but more likely to have a quality installer perform the task.

puebr5$Contractor
puebr5 %>% filter(lowfaircondition==T) %>% select(Contractor) %>% table() %>% sort()
