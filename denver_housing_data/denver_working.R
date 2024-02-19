library(rvest)
library(tidyverse)
parc<-read.csv("Documents/qt-local/DenMet/Denver/parcels.csv")


adds<-read.csv("Documents/qt-local/DenMet/Denver/addresses.csv")
#buildingadds<-esri2sf::esri2sf("https://arcgisserver.dev.accela.com/arcgis/rest/services/Denver/MapServer/0")
#saveRDS(buildingadds,"Documents/qt-local/DenMet/Denver/building_dept_addresses.rds")
buildingadds<-readRDS("Documents/qt-local/DenMet/Denver/building_dept_addresses.rds")
parcels23<-sf::st_read("Documents/qt-local/DenMet/Denver/parcelseries/2023parcels/parcels.shp")
parcels23<-parcels23 %>% select(SCHEDNUM,OWNER_NAME,SALE_MONTH,SALE_YEAR,SALE_PRICE,APPRAISE_1,RES_ABOVE_,ZONE_10)
parcels22<-sf::st_read("Documents/qt-local/DenMet/Denver/parcelseries/2022parcels/parcels.shp")%>% select(SCHEDNUM,OWNER_NAME,SALE_MONTH,SALE_YEAR,SALE_PRICE,APPRAISE_1,RES_ABOVE_,ZONE_10)
parcels21<-sf::st_read("Documents/qt-local/DenMet/Denver/parcelseries/2021parcels/parcels.shp")%>% select(SCHEDNUM,OWNER_NAME,SALE_MONTH,SALE_YEAR,SALE_PRICE,APPRAISE_1,RES_ABOVE_,ZONE_10)
parcels20<-sf::st_read("Documents/qt-local/DenMet/Denver/parcelseries/2020parcels/parcels.shp")
parcels20<-parcels20 %>% mutate(APPRAISE_1=TOTAL_VALU,RES_ABOVE_=IMP_AREA,ZONE_10=ACT_ZONE) %>% select(SCHEDNUM,OWNER_NAME,SALE_MONTH,SALE_YEAR,SALE_PRICE,TOTAL_VALU,RES_ABOVE_,ZONE_10) 
parcels20<-parcels20 %>% rename(APPRAISE_1=TOTAL_VALU)

parcels23$PAR_YEAR<-2023
parcels22$PAR_YEAR<-2022
parcels21$PAR_YEAR<-2021
parcels20$PAR_YEAR<-2020

allparcels<-rbind(parcels23, parcels22) %>% rbind(parcels21) %>% rbind(parcels20)
allgeos<-parcels23 %>% select(SCHEDNUM)
allgeos<-allgeos %>% sf::st_zm(drop=T)
allgeos<-sf::st_make_valid(allgeos)
val.list<-sapply(allgeos$geometry,sf::st_is_valid)
allgeos<-allgeos[-which(val.list==F),]
allgeocent<-sf::st_centroid(allgeos)
head(allgeocent)
rm(allgeos)
rm(val.list)
allparcels<-allparcels %>% as.data.frame() %>% select(-geometry)
allparcels %>% filter(SCHEDNUM=="0529324024000")
allparcels<-left_join(allparcels,allgeocent)
head(allparcels)
rm(parcels23)
rm(parcels22)
rm(parcels21)
rm(parcels20)
head(allparcels)
allparcels %>% filter(SCHEDNUM=="0515119013000")
allnames<-allparcels %>% select(SCHEDNUM,OWNER_NAME,geometry) %>% unique()
allnames$OWNER_LAST<-allnames$OWNER_NAME %>% stringr::str_split_fixed(.,",",2) %>% .[,1]
allnames$OWNER_FIRST<-allnames$OWNER_NAME %>% stringr::str_split_fixed(.,",",2) %>% .[,2]

##add block level dem data

racedata<-sf::read_sf("Documents/qt-local/denmet/voter_blocks/co_pl2020_b/co_pl2020_p3_b.shp")
ethdata<-sf::read_sf("Documents/qt-local/denmet/voter_blocks/co_pl2020_b/co_pl2020_p4_b.shp")
ethdata<-ethdata %>% select(GEOID20,P0040001,P0040002,COUNTY) %>% mutate("hisprate"=P0040002/P0040001) %>% filter(COUNTY=="031")
racedata<-racedata %>% select(GEOID20,P0030001,P0030003,COUNTY) %>% mutate("whiterate"=P0030003/P0030001) %>% filter(COUNTY=="031")
ethdata<-ethdata %>% left_join(.,racedata %>% as.data.frame() %>% select(GEOID20,whiterate,P0030001,P0030003))
allnames<-allnames %>% sf::st_sf()
ethdata<-ethdata %>% sf::st_transform(sf::st_crs(allnames))
?sf::st_intersects
allnames_i<-sf::st_intersects(allnames,ethdata)
allnames_i1<-sapply(allnames_i,function(X) X[1])
allnames<-cbind(allnames,ethdata[allnames_i1,] %>% as.data.frame() %>% select(-geometry))
allnames<-filter(allnames,is.na(OWNER_LAST)==F)
reth1<-rethnicity::predict_ethnicity(lastnames=allnames$OWNER_LAST,firstnames=allnames$OWNER_FIRST)
allnames<-cbind(allnames, reth1 %>% select(prob_black,prob_hispanic,prob_white))



parc<-filter(parc,stringr::str_detect(D_CLASS_CN,"SFR|RESIDENTIAL\\-ROWHOUSE|RESIDENTIAL\\-DUPLEX|RESIDENTIAL-TRIPLEX"))

parc$firstaddress<-parc$SITUS_ADDRESS_LINE1 %>% gsub("UNIT|APT|BLDG|#|\\,","",.) %>% stringr::str_squish()
adds$firstaddress<-adds$FULL_ADDRESS %>% toupper() %>% gsub("UNIT|APT|BLDG|#|\\,|STE","",.)  %>% gsub("-"," ",.) %>% stringr::str_squish() 
buildingadds$firstaddress<-buildingadds$FULL_ADDRESS %>% toupper() %>% gsub("UNIT|APT|BLDG|#|\\,|STE","",.) %>% stringr::str_squish()


permit1<-read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vQvD__E0zyFKQJoIRlwsRlDtZzRpTA-eupD4jPyAWLrLIxz002jT3tCguh8CjejFLkzPtOPlWbwS7PY/pub?gid=1505581905&single=true&output=csv")

permit1$firstaddress<-toupper(permit1$Address) %>% gsub("UNIT|APT|BLDG|#|\\,|STE","",.)  %>% gsub("-"," ",.) %>% stringr::str_squish()
permit1$firstaddress[which(stringr::str_extract(permit1$firstaddress,"^[0-9]+")==stringr::str_extract(permit1$firstaddress,"[0-9]+$"))]<-permit1$firstaddress[which(stringr::str_extract(permit1$firstaddress,"^[0-9]+")==stringr::str_extract(permit1$firstaddress,"[0-9]+$"))] %>% gsub(" [0-9]+$","",.)

permit1$firstaddress[which(stringr::str_extract(permit1$firstaddress," 0[1-9]$")==stringr::str_extract(permit1$firstaddress," 0[1-9]$"))]

permit1$firstaddress[which(stringr::str_extract(permit1$firstaddress," 0[1-9]$")==stringr::str_extract(permit1$firstaddress," 0[1-9]$"))]<-stringr::str_replace(permit1$firstaddress[which(stringr::str_extract(permit1$firstaddress," 0[1-9]$")==stringr::str_extract(permit1$firstaddress," 0[1-9]$"))],"0[1-9]$",as.character(permit1$firstaddress[which(stringr::str_extract(permit1$firstaddress," 0[1-9]$")==stringr::str_extract(permit1$firstaddress," 0[1-9]$"))] %>% sapply(.,function(X) as.numeric(stringr::str_extract(X," 0[1-9]$")))))
permit1$Valuation<-permit1$Valuation %>% gsub("\\$|\\,","",.) %>% as.numeric()


buildingadds$firstaddress[which(stringr::str_extract(buildingadds$firstaddress," 0[1-9]$")==stringr::str_extract(buildingadds$firstaddress," 0[1-9]$"))]<-stringr::str_replace(buildingadds$firstaddress[which(stringr::str_extract(buildingadds$firstaddress," 0[1-9]$")==stringr::str_extract(buildingadds$firstaddress," 0[1-9]$"))],"0[1-9]$",as.character(buildingadds$firstaddress[which(stringr::str_extract(buildingadds$firstaddress," 0[1-9]$")==stringr::str_extract(buildingadds$firstaddress," 0[1-9]$"))] %>% sapply(.,function(X) as.numeric(stringr::str_extract(X," 0[1-9]$")))))

adds$firstaddress[which(stringr::str_extract(adds$firstaddress," 0[1-9]$")==stringr::str_extract(adds$firstaddress," 0[1-9]$"))]<-stringr::str_replace(adds$firstaddress[which(stringr::str_extract(adds$firstaddress," 0[1-9]$")==stringr::str_extract(adds$firstaddress," 0[1-9]$"))],"0[1-9]$",as.character(adds$firstaddress[which(stringr::str_extract(adds$firstaddress," 0[1-9]$")==stringr::str_extract(adds$firstaddress," 0[1-9]$"))] %>% sapply(.,function(X) as.numeric(stringr::str_extract(X," 0[1-9]$")))))


table(permit1$firstaddress %in% adds$firstaddress)

permit1$firstaddress[which(c(permit1$firstaddress %in% adds$firstaddress)==F)] %>% unique() %>% sort()

permit.limited<-permit1 %>% select(Permit..,firstaddress)
permit.limited<-left_join(permit.limited,adds)
permit.limited$ADDRESS_NUMBER%in%buildingadds$ADDRESS_NUMBER %>% table()
permit.limited$STREET_NAME%in%buildingadds$STREET_NAME %>% table()
permit.limited$COMPOSIT_UNIT_IDENTIFIER[which(permit.limited$COMPOSIT_UNIT_IDENTIFIER=="")]<-NA
buildingadds$COMPOSIT_UNIT_IDENTIFIER[which(buildingadds$COMPOSIT_UNIT_IDENTIFIER=="")]<-NA
permit.limited$COMPOSIT_UNIT_IDENTIFIER%in%buildingadds$COMPOSIT_UNIT_IDENTIFIER %>% table()
permit.limited<-permit.limited %>% select(Permit..,ADDRESS_NUMBER,STREET_NAME,PREDIRECTIONAL,COMPOSIT_UNIT_IDENTIFIER) %>% merge(.,buildingadds)
permit.limited$COMPOSIT_UNIT_IDENTIFIER %>% table()
permit.limited<-filter(permit.limited, is.na(FULL_ADDRESS)==F)
permit.limited$STREET_NAME[which(c(permit.limited$STREET_NAME %>% toupper(.)) %in% c(buildingadds$STREET_NAME %>% toupper(.))==F)]
library(tidyverse)
permit.limited %>% head()

permit.limited<-permit.limited %>% select(Permit..,SCHEDNUM)
parc<-parc %>% select(SCHEDNUM,D_CLASS_CN,PROP_CLASS,D_CLASS,ZONE_ID,RES_ORIG_YEAR_BUILT,RES_ABOVE_GRADE_AREA)
library(sf)
allparcels<-left_join(allparcels %>% mutate(SCHEDNUMCHAR=SCHEDNUM,SCHEDNUM=as.numeric(SCHEDNUM)),parc)

permit.limited$PAR_YEAR<-permit.limited$Permit.. %>% stringr::str_extract("^[0-9][0-9][0-9][0-9]") %>% as.numeric()
allparcels<-left_join(allparcels,permit.limited %>% mutate(SCHEDNUM=as.numeric(SCHEDNUM)),na_matches = c("never"))


#parcels with multiple permits show up multiple times


allparcels<-left_join(allparcels,permit1)

allparcels<-left_join(allparcels,allnames %>% as.data.frame() %>% select(-geometry) %>% mutate(SCHEDNUM=as.numeric(SCHEDNUM)),na_matches = c("never"))

allparcels$mech.permit01<-ifelse(is.na(allparcels$Permit..)==T,0,1)

allparcels %>% as.data.frame(select(SCHEDNUM))

allparcels$Stat.Code %>% table()
#314 ductwork
#315 water heaters and gas appliances
#310 cooling
#313 warm air
#316 gas piping
table(allparcels$Stat.Code)

cooling<-filter(allparcels,Stat.Code==310)
heating<-filter(allparcels,Stat.Code==313)


table(allparcels$Stat.Code)
head(cooling)
cooling$Date.Issued<- cooling$Date.Issued %>% lubridate::mdy()
heating$Date.Issued<-lubridate::mdy(heating$Date.Issued)
heatcool<-rbind(heating,cooling)

ggplot(heatcool %>% filter(Valuation<50000))+geom_point(aes(x=Date.Issued,y=Valuation,colour=as.factor(Stat.Code)),size=.1)+geom_smooth(aes(x=Date.Issued,y=Valuation,colour=as.factor(Stat.Code)))+theme_minimal()+ggthemes::scale_colour_economist()


ggplot(heatcool %>% filter(Valuation<50000))+geom_point(aes(x=Date.Issued,y=Valuation,colour=as.factor(Stat.Code)),size=.1)

filter(heatcool,Date.Issued>mdy("11/01/22"), Date.Issued<mdy("2/28/23")) %>% filter(Valuation<30000) %>% ggplot(. )+geom_point(aes(x=Date.Issued,y=Valuation,colour=as.factor(Stat.Code)),size=.1)+geom_smooth(aes(x=Date.Issued,y=Valuation,colour=as.factor(Stat.Code),lty=(Date.Issued>mdy("12/31/22"))),method="lm")+theme_minimal()


head(heatcool)
table(heatcool$Contractor.s.Name) %>% sort()



cs1<-lapply(list.files("Documents/qt-local/voter_lists/",full.names = T)[5:8],read.csv) %>% bind_rows()
cs2<-read.csv("Documents/qt-local/denmet/voter_blocks/CO_2022_prim_l2_vf_2020blocks.csv")
table(cs1$COUNTY)
cs1<-filter(cs1, COUNTY=="Denver")
head(cs1)
cs1$STREET_NAME
temp1<-cs1 %>% select(VOTER_ID,HOUSE_NUM,STREET_NAME,FIRST_NAME,LAST_NAME) 

head(heatcool)










allparcels %>% filter(Stat.Code==317) %>% select(Permit..) %>% .[1,] %>% as.character()
blocksum<-allparcels %>% group_by(GEOID20) %>% summarise(permitsum=sum(mech.permit01,na.omit=T))
ifelse(Stat.Code)
blocksum<-left_join(ethdata,blocksum)
#blocksum<-blocksum %>% select(GEOID20,P0040001) %>% unique() %>% left_join(.,blocksum)
blocksum$permitsum<-ifelse(is.na(blocksum$permitsum),0,blocksum$permitsum)
ggplot(blocksum %>% filter(P0040001>0))+geom_sf(aes(fill=log(permitsum/c(P0040001+1)),colour=log(permitsum/c(P0040001+1))))+scale_fill_viridis_c()+scale_colour_viridis_c()+theme_minimal()


parcm<-merge(parc,permit.limited %>% as.data.frame() %>% select(Permit..,PIN,SCHEDNUM,LAND_VALUE,IMPROVEMENT_VALUE,IMPROVEMENTS,TOTAL_VALUE,LAND,ACT_ZONE,IMP_AREA,CCYRBLT) %>% mutate(SCHEDNUM=as.numeric(SCHEDNUM)))
parcm<-parcm[,-c(2:30,32,33,35:47,67:70)]
parcm<-left_join(allparcels %>% mutate(SCHEDNUM=as.numeric(SCHEDNUM)),parcm)

parcm<-left_join(parcm,permit1[1:18])
head(parcm)
parcm$Valuation %>% log() 
  
parcm$Date.Issued<-lubridate::mdy(parcm$Date.Issued)
parcm$APPRAISED_IMP_VALUE<-parcm$APPRAISED_IMP_VALUE %>% as.numeric()
parcm$IMP_AREA>1500


ggplot(racedata)+geom_sf(aes(fill=whiterate,colour=whiterate))+scale_fill_viridis_c()+scale_colour_viridis_c()
ggplot(ethdata)+geom_sf(aes(fill=hisprate,colour=hisprate))+scale_fill_viridis_c()+scale_colour_viridis_c()
ggplot(ethdata)+geom_sf(aes(fill=hisprate,colour=hisprate))+scale_fill_viridis_c()+scale_colour_viridis_c()
parcm$OWNER_NAME[1:100]
rethnicity::predict_ethnicity()
parcm$OWNER_LAST<-parcm$OWNER_NAME %>% stringr::str_split_fixed(.,",",2) %>% .[,1]
parcm$OWNER_FIRST<-parcm$OWNER_NAME %>% stringr::str_split_fixed(.,",",2) %>% .[,2]
parcm$SITUS_X_COORD

install.packages("eiCompare")
install.packages("wru")
future::plan(future::multisession)
library(wru)
data(voters)
voters %>% head()
sf::st_crs(rescon)


parcm



predict_race(voter.file = voters, surname.only = T)
library(eiCompare)


testm1<-lm(log(Valuation+1)~log(TOTAL_VALUE)+as.numeric(Date.Issued)+RES_ABOVE_GRADE_AREA+as.numeric(RES_ORIG_YEAR_BUILT),data=parcm)

table(as.numeric(buildingadds$SCHEDNUM)%in%parcm$SCHEDNUM)

rescon<-esri2sf::esri2sf("https://arcgisserver.dev.accela.com/arcgis/rest/services/Denver/FeatureServer/4")


permit.limited %>% select(-c(colnames(permit.limited)[which(colnames(permit.limited)%in%colnames(parc)==T)][-4]))
permit.limited %>% select(Permit.)
head(permit.limited)


table(permit1$firstaddress %in% buildingadds$firstaddress)

permit1$firstaddress[which(c(permit1$firstaddress %in% buildingadds$firstaddress)==F)] %>% unique %>% sort()

buildingadds$firstaddress[stringr::str_detect(buildingadds$firstaddress, "1331 17TH")]








permit1<-left_join(permit1,adds,na_matches="never",multiple="first")


permit1[which(permit1$Valuation>100000),]
head(adds)
permit1$firstaddress
parc[which(parc$SITUS_ADDRESS_ID %>% as.numeric()==320453),]

https://www.denvergov.org/denvermapsservices/FindAddressCandidates/?querystring=1740 N BROADWAY

  library(jsonlite)
ad1<-jsonlite::fromJSON(URLencode(paste0("https://www.denvergov.org/denvermapsservices/FindAddressCandidates/?querystring=",permit1$firstaddress[2])))
head(ad1)



xcelquality<-esri2sf::esri2sf("https://services1.arcgis.com/eM84fwjsSggLQk61/ArcGIS/rest/services/COServiceQuailty2022/FeatureServer/0")
xcelquality %>% saveRDS("xcelquality2022.rds")




cont1<-httr::POST("https://denvergov.org/contractorlicenses//api/Contractors")
cont1 %>% httr::content() %>% View()

body <- list(companyNameSrch = "%")
url <- "https://denvergov.org/contractorlicenses//api/Contractors" # Insert complete base URL

response <- httr::POST(url, body = body)
response 

