library(basemaps)
library(ggspatial)
library(sf)



puebr4<-readRDS("/Users/rpscott/Documents/GitHub/RESPECT-QTS/pueblo_housing_data/processed_data/full_s_2018.version3.rds")
puebr5<-readRDS("/Users/rpscott/Documents/GitHub/RESPECT-QTS/pueblo_housing_data/processed_data/permit_s_2018.version3.rds")
pueblo_contractors<-read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vRj8AkwjfguXDbr14v80dLPcOFzQxjKZ9ZMKbln08X7gQPNDgT94mS7alolTmwH6mCais4Nd0g2T5nK/pub?output=csv")
pueblo_contractors$ID<-paste0("c",1:nrow(pueblo_contractors))

ebg<-sf::read_sf("Downloads/Colorado_EnviroScreen_v1_BlockGroup.geojson")
ebg<-filter(ebg,Cnt_N=="Pueblo County")
ebnb<-ebg %>% spdep::poly2nb()
ebg$placeidinla<-1:nrow(ebg)

puebflat<-rbind(pueblo_contractors %>% select(-Name.2),pueblo_contractors %>% select(-Name) %>% filter(nchar(Name.2)>0) %>% rename(Name=Name.2))
puebr5<-puebr5 %>% left_join(.,puebflat  %>% select(Name, ID) %>% rename(Contractor=Name,ContID=ID) %>% unique(),na_matches = c("never"))

puebflat$hisp<-rethnicity::predict_ethnicity(firstnames=puebflat$Examinee %>% stringr::str_trim() %>% stringr::str_split_fixed(.,"\\W",2) %>% .[,1],lastnames=puebflat$Examinee %>% stringr::str_trim()  %>% stringr::str_split_fixed(.,"\\W",2) %>% .[,2]) %>% select(prob_hispanic) %>% unlist()

puebflat$GovNGO[is.na(puebflat$GovNGO)]<-0

concounts<-puebr5$Contractor %>% table() %>% as.data.frame()

colnames(concounts)[1]<-"Name"

puebflat<-left_join(puebflat,concounts)
c_unique<-puebflat %>% select(-Name) 
c_unique$Freq[is.na(c_unique$Freq)]<-0
c_unique<-c_unique %>% select(-Freq) %>% unique() %>% left_join(c_unique %>% select(ID,Freq) %>% group_by(ID) %>% summarise(Freq=sum(Freq)))

phones<-paste("%2B1",c_unique$Phone) %>% .[nchar(.)>5] 
phones<-stringr::str_trim(phones)
phones<-phones[phones %>% gsub("%2B1","",.) %>% stringr::str_trim() %>% gsub(" |-","",.) %>% paste0(.,".rds") %in% list.files("Documents/GitHub/RESPECT-QTS/pueblo_housing_data/company_rating/")==F]
getphone<-function(phonenumber) {
  placefind<-RCurl::getURL(URLencode(paste0("https://maps.googleapis.com/maps/api/place/findplacefromtext/json?fields=place_id&input=",phonenumber %>% gsub(" |-","",.),"&inputtype=phonenumber&key=",Y)))
  id1<-placefind %>% jsonlite::fromJSON() %>% .$candidates %>% .$place_id %>% .[1]
  if(is.null(id1)==F){
    out<-paste0("https://maps.googleapis.com/maps/api/place/details/json?place_id=",id1,"&fields=","rating%2Cuser_ratings_total&key=",Y)
    ratings<-RCurl::getURL(URLencode(out))
    ratings %>% fromJSON() %>% .$result %>% as.data.frame() %>% mutate(phone=gsub("%2B1","",phonenumber) %>% stringr::str_trim() %>% gsub(" |-","",.)) %>% saveRDS(paste0("Documents/GitHub/RESPECT-QTS/pueblo_housing_data/company_rating/",gsub("%2B1","",phonenumber) %>% stringr::str_trim() %>% gsub(" |-","",.),".rds"))} else {data.frame("rating"=NA,user_ratings_total=NA,"phone"=gsub("%2B1","",phonenumber) %>% stringr::str_trim() %>% gsub(" |-","",.)) %>% saveRDS(paste0("Documents/GitHub/RESPECT-QTS/pueblo_housing_data/company_rating/",gsub("%2B1","",phonenumber) %>% stringr::str_trim() %>% gsub(" |-","",.),".rds"))}
}
for(i in 1:length(phones)) {getphone(phones[i])
  Sys.sleep(1)}

googleratings<-list.files("/Users/rpscott/Documents/GitHub/RESPECT-QTS/pueblo_housing_data/company_rating",full.names=T) %>% lapply(.,readRDS) %>% bind_rows()

c_unique$phone<-c_unique$Phone %>% gsub("-","",.) %>% stringr::str_trim()
c_unique<-left_join(c_unique,googleratings,na_matches = c("never"))
c_unique$addstring<-paste(c_unique$Address, c_unique$City, c_unique$State) %>% stringr::str_trim()
c_unique$addstring[c_unique$addstring==""]<-NA
addsout<-c_unique$addstring[is.na(c_unique$rating)] %>% na.omit()
addsout<-addsout[paste0(addsout,".rds")%in%list.files("Documents/GitHub/RESPECT-QTS/pueblo_housing_data/company_rating_address/")==F]

addsout<-addsout %>% gsub("/",".",.,fixed=T) %>% gsub("\n",".",.,fixed=T)
getaddress<-function(address) {
  placefind<-RCurl::getURL(URLencode(paste0("https://maps.googleapis.com/maps/api/place/findplacefromtext/json?fields=place_id&input=",address,"&inputtype=textquery&key=",Y)))
  id1<-placefind %>% jsonlite::fromJSON() %>% .$candidates %>% .$place_id %>% .[1]
  if(is.null(id1)==F){
    out<-paste0("https://maps.googleapis.com/maps/api/place/details/json?place_id=",id1,"&fields=","rating%2Cuser_ratings_total&key=",Y)
    ratings<-RCurl::getURL(URLencode(out))
    ratings %>% fromJSON() %>% .$result %>% as.data.frame() %>% mutate(address=address) %>% saveRDS(paste0("Documents/GitHub/RESPECT-QTS/pueblo_housing_data/company_rating_address/",stringr::str_squish(address),".rds"))} else {data.frame("rating"=NA,user_ratings_total=NA,"address"=address) %>% saveRDS(paste0("Documents/GitHub/RESPECT-QTS/pueblo_housing_data/company_rating_address/",stringr::str_squish(address),".rds"))}
}

for(i in 1:length(addsout)) {getaddress(addsout[i])
  Sys.sleep(1)}
lapply(list.files("Documents/GitHub/RESPECT-QTS/pueblo_housing_data/company_rating_address/",full.names=T),readRDS) %>% .[[1]]
addsin<-lapply(list.files("Documents/GitHub/RESPECT-QTS/pueblo_housing_data/company_rating_address/",full.names=T),readRDS) %>% bind_rows()
addsin<-filter(addsin,is.na(rating)==F)
colnames(addsin)[2:3]<-paste0(colnames(addsin)[2:3],".1")
colnames(addsin)[1]<-"addstring"
c_unique<-left_join(c_unique,addsin,na_matches = c("never"))
c_unique$rating.joined<-coalesce(c_unique$rating,c_unique$rating.1)


c_unique$norating<-is.na(c_unique$rating.joined)
c_unique$hisp<-cunique$c_hisp
c_unique %>% select(ID,EStarACCABPINCI)
puebr5<-left_join(puebr5,c_unique %>% rename(ContID=ID) %>% select(ContID,EStarACCABPINCI,GovNGO),na_matches = c("never"))
puebr5$EStarACCABPINCI_cont<-puebr5$EStarACCABPINCI
puebr5$EStarACCABPINCI<-as.numeric(puebr5$EStarACCABPINCI>0)


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



ebg %>% select(OBJECTID,placeidinla) %>% left_join(.,data.frame("placeidinla"=m1$summary.random$placeidinla$ID,"mean"=m1$summary.random$placeidinla$mean[1:134]+m1$summary.random$placeidinla$mean[135:268])) %>% st_crop(census_data, xmin = -104.6, xmax = -104.7, ymin = 38.2, ymax = 38.3) %>% ggplot()+annotation_map_tile(zoom=13,type="stamenbw")+geom_sf(aes(fill=mean),alpha=.5)+scale_fill_viridis_c()+theme_minimal()+geom_sf_text(aes(label=placeidinla,colour=ifelse(placeidinla%in%c(30, 134, 68, 103,76,10,12,106,96,97,11,100,46,27,105,27,37,16,17,14,9,13),"selected","not selected")))+scale_colour_manual(name="sample", values=c("black","red"))


ebg %>% select(OBJECTID,placeidinla) %>% left_join(.,data.frame("placeidinla"=m1$summary.random$placeidinla$ID,"mean"=m1$summary.random$placeidinla$mean[1:134]+m1$summary.random$placeidinla$mean[135:268])) %>% ggplot(aes(x=placeidinla,y=mean))+geom_point()+geom_text(aes(label=placeidinla,colour=ifelse(placeidinla%in%c(30, 134, 68, 103,76,10,12,106,96,97,11,100,46,27,105,27,37,16,17,14,9,13),"selected","not selected")))+scale_colour_manual(name="sample", values=c("black","red"))


set.1218<-c(25,41,79,111,39,19,21,126,121,38,34,84,88,110,130,58,38,34,120,116,121,126,131)

filter(puebr5, placeidinla%in%set.1218) %>% as.data.frame() %>% select(-geometry,-geoms) %>% filter(Year==2022) %>% mutate(permitlink=paste0("https://www.prbd.com/searches/inspectionsearch1.php?pno=",Permit_No)) %>% unique() %>% filter(Permit_No%in%c(lapply(list.files("Documents/qt-local/qualsamples",full.names = T),function(X) {read.csv(X) %>% select(Permit_No)}) %>% bind_rows() %>% unique() %>% .$Permit_No)==F) %>% write.csv("neighborhood_sample_contacts_batch_1218.csv")
read.csv("neighborhood_sample_contacts_batch_1218.csv") %>% select(ADDRID ,Permit_No) %>% unique()

filter(puebr5, placeidinla%in%set.1218) %>% as.data.frame() %>% select(-geometry,-geoms) %>% filter(Year==2022) %>% nrow()


ggplot(ebg)+geom_sf()



ebg %>% select(OBJECTID,placeidinla) %>% left_join(.,data.frame("placeidinla"=m1a$summary.random$placeidinla$ID,"mean"=m1a$summary.random$placeidinla$mean[1:134]+m1a$summary.random$placeidinla$mean[135:268])) %>% mutate(sampled=placeidinla%in%c(11,100,12,96,97,106,30,134,68,103,76,10,)) %>% st_crop(census_data, xmin = -104.6, xmax = -104.7, ymin = 38.2, ymax = 38.3) %>% ggplot()+annotation_map_tile(zoom=13,type="stamenbw")+geom_sf(aes(fill=mean,colour=sampled,size=sampled),alpha=.9)+scale_fill_viridis_c()+theme_minimal()+scale_colour_manual(values=c(NA,"white"))+scale_size_manual(values=c(.1,3))




probmodel<-m1$summary.fixed  %>% mutate(id=rownames(m1$summary.fixed)) %>% mutate(id=forcats::fct_recode(id,"house, hispanic owner, prob"="scale(prob_hispanic)","house, sqft,ln"="scale(log(as.numeric(SquareFootage)))","house, assessor value"="scale(log(as.numeric(ActualValue)))","house, sold in period"="saleinperiodTRUE","house, poor condition"="lowfairconditionTRUE","house, sold last pre-2012"="lastsalepre2012TRUE","block group, housing cost burdened"="scale(HCB)","house, evaporative cooler"="evapcooler","house, electric baseboard"="ebaseboard","house, central air"="centralair","block group, less than hs education"="scale(Pr_____)","house, built in period"="newbuildTRUE"),var="hvac mechanical permit")  %>% ggplot()+geom_hline(aes(yintercept=0),lty=2)+geom_pointrange(aes(x=id,y=mean,ymin=`0.025quant`,ymax=`0.975quant`))+coord_flip()+theme_minimal()+ylab("95% fixed effect credible interval (DV=Mechanical Permit in 2022)")+xlab("variable")

probmodel2<-m1a$summary.fixed  %>% mutate(id=rownames(m1$summary.fixed)) %>% mutate(id=forcats::fct_recode(id,"house, hispanic owner, prob"="scale(prob_hispanic)","house, sqft,ln"="scale(log(as.numeric(SquareFootage)))","house, assessor value"="scale(log(as.numeric(ActualValue)))","house, sold in period"="saleinperiodTRUE","house, poor condition"="lowfairconditionTRUE","house, sold last pre-2012"="lastsalepre2012TRUE","block group, housing cost burdened"="HCB","house, evaporative cooler"="evapcooler","house, electric baseboard"="ebaseboard","house, central air"="centralair","block group, less than hs education"="Pr_____","house, built in period"="newbuildTRUE"),var="permit, quality installation")  %>% ggplot()+geom_hline(aes(yintercept=0),lty=2)+geom_pointrange(aes(x=id,y=mean,ymin=`0.025quant`,ymax=`0.975quant`))+coord_flip()+theme_minimal()+ylab("95% fixed effect credible interval (DV=Mechanical Permit in 2022)")+xlab("variable")
options(warn=1)
jointplot<-rbind(m1a$summary.fixed  %>% mutate(id=rownames(m1a$summary.fixed)) %>% mutate(id=forcats::fct_recode(id,"house, hispanic owner, prob"="scale(prob_hispanic)","house, sqft,ln"="scale(log(as.numeric(SquareFootage)))","house, assessor value"="scale(log(as.numeric(ActualValue)))","house, sold in period"="saleinperiodTRUE","house, poor condition"="lowfairconditionTRUE","house, sold last pre-2012"="lastsalepre2012TRUE","block group, housing cost burdened"="scale(HCB)","house, evaporative cooler"="evapcooler","house, electric baseboard"="ebaseboard","house, central air"="centralair","block group, less than hs education"="scale(Pr_____)"),var="permit, quality installation"),m1$summary.fixed  %>% mutate(id=rownames(m1$summary.fixed)) %>% mutate(id=forcats::fct_recode(id,"house, hispanic owner, prob"="scale(prob_hispanic)","house, sqft,ln"="scale(log(as.numeric(SquareFootage)))","house, assessor value"="scale(log(as.numeric(ActualValue)))","house, sold in period"="saleinperiodTRUE","house, poor condition"="lowfairconditionTRUE","house, sold last pre-2012"="lastsalepre2012TRUE","block group, housing cost burdened"="scale(HCB)","house, evaporative cooler"="evapcooler","house, electric baseboard"="ebaseboard","house, central air"="centralair","block group, less than hs education"="scale(Pr_____)"),var="hvac mechanical permit")) %>% filter(id!="(Intercept)") %>% ggplot()+geom_hline(aes(yintercept=0),lty=2)+geom_pointrange(aes(x=id,y=mean,ymin=`0.025quant`,ymax=`0.975quant`,colour=var),position = position_dodge(width=.5))+coord_flip()+theme_minimal()+ylab("95% fixed effect credible interval (DV=Mechanical Permit 2018:2022)")+xlab("variable")+theme(legend.position="top")
