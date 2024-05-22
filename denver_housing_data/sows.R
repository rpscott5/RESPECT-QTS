library(plyr)
library(dplyr)

sows<-list.files("Documents/qt-local/denmet/Denver/permit_sow/",full.names=T)

sows.names<-list.files("Documents/qt-local/denmet/Denver/permit_sow/")

sows<-sows %>% sapply(.,readLines)


sows<-data.frame("Permit.."=sows.names,"sow"=sows)

sows$sow2<-sows$sow

sows$sow2[which(stringr::str_detect(tolower(sows$sow),"replac") & stringr::str_detect(tolower(sows$sow),"with|w\\/"))]<-sows$sow[which(stringr::str_detect(tolower(sows$sow),"replac") & stringr::str_detect(tolower(sows$sow),"with|w\\/"))] %>% tolower() %>% stringr::str_split_fixed(.,pattern="with",n=2) %>% .[,2] %>% stringr::str_squish()

sows$sow1<-NA
sows$sow1[which(stringr::str_detect(tolower(sows$sow),"replac") & stringr::str_detect(tolower(sows$sow),"with|w\\/"))]<-sows$sow[which(stringr::str_detect(tolower(sows$sow),"replac") & stringr::str_detect(tolower(sows$sow),"with|w\\/"))] %>% tolower() %>% stringr::str_split_fixed(.,pattern="with",n=2) %>% .[,1] %>% stringr::str_squish()

sows$sow2[which(stringr::str_detect(tolower(sows$sow),"to replace"))]<-sows$sow[which(stringr::str_detect(tolower(sows$sow),"to replace"))] %>% tolower() %>% stringr::str_split_fixed(.,pattern="to replace",n=2) %>% .[,1] %>% stringr::str_squish()
sows$sow1[which(stringr::str_detect(tolower(sows$sow),"to replace"))]<-sows$sow[which(stringr::str_detect(tolower(sows$sow),"to replace"))] %>% tolower() %>% stringr::str_split_fixed(.,pattern="to replace",n=2) %>% .[,2] %>% stringr::str_squish()



sows$sow2[which(stringr::str_detect(tolower(sows$sow2),"replacing"))]<-sows$sow2[which(stringr::str_detect(tolower(sows$sow2),"replacing"))] %>% tolower() %>% stringr::str_split_fixed(.,pattern="replacing",n=2) %>% .[,1] %>% stringr::str_squish()
sows$sow1[which(stringr::str_detect(tolower(sows$sow),"replacing"))]<-sows$sow[which(stringr::str_detect(tolower(sows$sow),"replacing"))] %>% tolower() %>% stringr::str_split_fixed(.,pattern="replacing",n=2) %>% .[,2] %>% stringr::str_squish()


rownames(sows)<-1:nrow(sows)

sows$ac_efficiency<-stringr::str_extract(tolower(sows$sow2), "([0-9\\.]+) seer|([0-9\\.]+)seer",1)
sows$ac_size<-stringr::str_extract(tolower(sows$sow2),"([0-9\\.\\-]+) ton")
sows$furnace_efficiency<-stringr::str_extract(tolower(sows$sow2),"[0-9.]+\\%")
sows$furnace_size<-stringr::str_extract(tolower(sows$sow2),"[0-9\\,\\-k]+btu|[0-9\\,\\-k]+ btu")
sows$heatpump.mini<-stringr::str_detect(tolower(sows$sow2), "mini|split|head") 
sows$heatpump.central<-c(stringr::str_detect(tolower(sows$sow2), "heat pump|\\Whp\\W|ashp")==T & stringr::str_detect(tolower(sows$sow2), "mini|split|head")==F)
sows$furnace<-stringr::str_detect(tolower(sows$sow2), "furnace")
sows$ac<-gsub("ac circuit","",tolower(sows$sow2)) %>% stringr::str_detect(., "\\Wac|a\\/c|air conditioner")
sows$ac_size<-as.numeric(stringr::str_squish(gsub("ton","",sows$ac_size)))

sows$ac<-ifelse(sows$heatpump.mini==F & sows$heatpump.central==F & sows$ac==F & stringr::str_detect(tolower(sows$sow2),"seer"),T,sows$ac)

sows$Permit..<-sows$Permit.. %>% gsub(".txt","",.) 

sows$furnace_efficiency %>% table()
sows$heatpump.mini %>% table()
sows$heatpump.central %>% table()


permit.details<-readRDS("Documents/qt-local/denmet/Denver/permitdetails.rds")
permit.details$Permit..<-permit.details$filename %>% basename() %>% gsub(".html","",.)
sows<-left_join(sows,permit.details)



allparcels<-readRDS("Documents/qt-local/DenMet/Denver/scratch/denverpermit_working_batch.rds")
allparcels$zonesimple<-allparcels$ZONE_ID %>% stringr::str_extract("^[A-Z]")
allparcels$yb.decade<-floor(as.numeric(allparcels$RES_ORIG_YEAR_BUIL)/10)*10 
allparcels$yb.decade[which(allparcels$yb.decade<1940)]<-"<1940"

allparcels$res_bin<-allparcels$RES_ABOVE_ %>% cut(breaks=c(0,1500,2500,4000,100000),include.lowest=T,ordered_result=T)
levels(allparcels$res_bin)<-c("0-1500","1500-2499","2500-3999","4000+")

allparcels$appraise_bin<-allparcels$APPRAISE_1 %>% cut(breaks=c(0,300000,600000,1000000,3000000,10000000),right=T,include.lowest=T,ordered_result=T) 

levels(allparcels$appraise_bin)<-c("<300k","300k-600k","600-1m","1m-3m","3m+")

allparcels<-left_join(mutate(allparcels,RES_ORIG_YEAR_BUILT=as.numeric(RES_ORIG_YEAR_BUILT)),read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vSCl_Jp_Uq8WJ0-eqBB-ALHktdO75WSR1ldCsFYsJea0c5oIYfiP25Gcf_sbLhbYLJYRCnivKzhdjaH/pub?gid=1311720634&single=true&output=csv"))

sows<-left_join(sows,allparcels)
sows<-filter(sows,is.na(PAR_YEAR)==F)



parcels23<-sf::st_read("Documents/qt-local/DenMet/Denver/parcelseries/2023parcels/parcels.shp")
parcels23<-parcels23 %>% select(SCHEDNUM,OWNER_NAME,SALE_MONTH,SALE_YEAR,SALE_PRICE,APPRAISE_1,RES_ABOVE_,ZONE_10)
basecrs<-sf::st_crs(parcels23)
rm(parcels23)
library(sf)
allparcels<-st_sf(allparcels,sf_column_name="geometry",crs=basecrs)
#redlined<-esri2sf::esri2sf("https://services1.arcgis.com/zdB7qR0BtYrg0Xpl/ArcGIS/rest/services/ODC_HLTH_REDLINING_A/FeatureServer/307")
#redlines<-st_intersects(sows %>% st_sf(),redlined %>% select(GRADE))

#sows$redgrade<-redlined$GRADE[sapply(redlines, function(X) X[1])]

apshape<-allparcels %>% select(SCHEDNUMCHAR) %>% unique()
apshape<-apshape[-which(sf::st_is_empty(apshape)),]


apshape %>% sf::st_write(., dsn = "Documents/qt-local/denmet/Denver/scratch/parlocs.geojson", layer = "parlocs.geojson",append=F)




allparcels$blockgroup<-allparcels$GEOID20 %>% substr(.,1,12)
allparcels$tract<-allparcels$GEOID20 %>% substr(.,1,10)
allparcels$block<-allparcels$GEOID20 %>% substr(.,12,15)

bg.acs.data<-esri2sf::esri2sf("https://services1.arcgis.com/zdB7qR0BtYrg0Xpl/ArcGIS/rest/services/ODC_POP_AMERCMTYSVY20172021BLKGP_A/FeatureServer/365")
bg.acs.data$blockgroup<-bg.acs.data$STFID %>% substr(.,10,21)
bgnb<-spdep::poly2nb(bg.acs.data)
spdep::nb2INLA("Documents/GitHub/RESPECT-QTS/denver_housing_data/scratch/denverbg.adj",bgnb)
nbdepframe<-data.frame("inlaid"=1:length(bg.acs.data$blockgroup),"blockgroup"=bg.acs.data$blockgroup)
write.csv(nbdepframe,"Documents/GitHub/RESPECT-QTS/denver_housing_data/scratch/denverbgids.csv")
allparcels<-left_join(allparcels,bg.acs.data %>% as.data.frame() %>% select(blockgroup,MED_HH_INCOME,BACHELORS_OR_HIGHER_EDU,AGE65PLUS,AGE_LESS_5,TTL_POPULATION_ALL,HISPANIC_OR_LATINO))



allparcels$perc.over.65<-c(allparcels$AGE65PLUS/allparcels$TTL_POPULATION_ALL)
allparcels$perc.hisp<-c(allparcels$HISPANIC_OR_LATINO/allparcels$TTL_POPULATION_ALL)
allparcels$perc.less.5<-c(allparcels$AGE_LESS_5/allparcels$TTL_POPULATION_ALL)
allparcels$perc.bach<-c(allparcels$BACHELORS_OR_HIGHER_EDU/allparcels$TTL_POPULATION_ALL)



allpd<-allparcels %>% as.data.frame() %>% select(D_CLASS_CN,CODE_YEAR,PAR_YEAR,RES_ABOVE_GRADE_AREA,RES_ORIG_YEAR_BUILT,APPRAISE_1,hisprate,mech.permit01,prob_hispanic,perc.bach,AGE65PLUS,AGE_LESS_5,TTL_POPULATION_ALL,MED_HH_INCOME,perc.less.5,perc.over.65,perc.hisp,SCHEDNUMCHAR)


basicmodel<-lm(as.numeric(mech.permit01)~as.factor(D_CLASS_CN)+factor(CODE_YEAR)+PAR_YEAR+as.numeric(RES_ABOVE_GRADE_AREA)+RES_ORIG_YEAR_BUILT+APPRAISE_1+MED_HH_INCOME+perc.less.5+perc.hisp+perc.over.65+prob_hispanic+perc.bach+hi,data=allpd)

basicmodel2<-glm(as.numeric(mech.permit01)~
                   as.factor(D_CLASS_CN)+
                   factor(CODE_YEAR)+
                   PAR_YEAR+
                   as.numeric(RES_ABOVE_GRADE_AREA)+
                   RES_ORIG_YEAR_BUILT+
                   APPRAISE_1+
                   MED_HH_INCOME+
                   perc.less.5+
                   perc.hisp+
                   perc.over.65+
                   prob_hispanic+
                   perc.bach+
                   hi,
                 data=allpd,
                 family="gaussian")
head(allpd)


summary(basicmodel2)
install.packages("pROC")
myroc <- pROC::roc(allpd$mech.permit01,predict(basicmodel2, allpd, type = "response")) 

plot(myroc)

#buildfoot<-esri2sf::esri2sf("https://services1.arcgis.com/zdB7qR0BtYrg0Xpl/ArcGIS/rest/services/Final_Draft_Building_Footprint3/FeatureServer/1")
#saveRDS(buildfoot,"Documents/qt-local/denmet/Denver/controldata/buildingfootprint.rds")
buildfoot<-readRDS("Documents/qt-local/denmet/Denver/controldata/buildingfootprint.rds")
library(sf)

nearestbuild<-st_nearest_feature(allparcels,buildfoot %>% st_make_valid)
hi<-buildfoot$Urban_Heat_Island_Score[nearestbuild]
allparcels$heatanomoly<-hi
#subdivisions<-esri2sf::esri2sf("https://services1.arcgis.com/zdB7qR0BtYrg0Xpl/ArcGIS/rest/services/ODC_ENG_SRVSUBDIVISIONS_A/FeatureServer/54")
#saveRDS(subdivisions,"Documents/qt-local/denmet/Denver/controldata/subdivisions.rds")
#subdivisions<-readRDS("Documents/qt-local/denmet/Denver/controldata/subdivisions.rds")
#sbd<-st_intersects(allparcels,subdivisions %>% st_make_valid)
#allparcels$subdivision<-subdivisions$SUBNAME[sapply(sbd,function(X) X[1])]

str<-read.csv("Documents/qt-local/denmet/Denver/controldata/City_of_Denver_Short_Term_Rental_Licenses_20240305.csv")
allparcels$shortermrental<-allparcels$SCHEDNUM%in%str$Parcel.Number



#buslis<-read.csv("Documents/qt-local/denmet/Denver/controldata/Active_Business_Licenses_Denver_20240305.csv")
#buslis<-filter(buslis,LICENSE_TYPE%in%"Residential Rental Property")
head(buslis)

library(INLA)
allparcels$Class[is.na(allparcels$Class)]<-"no permit"



sows %>% head()


allparcels_sows<-left_join(allparcels,sows)

sows %>% saveRDS("Documents/GitHub/RESPECT-QTS/denver_housing_data/processed_data/permits_scopes_of_work_20_23.rds")

allparcels %>% saveRDS("Documents/GitHub/RESPECT-QTS/denver_housing_data/processed_data/allparcels_20_23.rds")

