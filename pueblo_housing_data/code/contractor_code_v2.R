library(plyr)
library(tidyverse)
library(jsonlite)

recodeCATS2<-function(X){ 
  lookupt<-c("house, hispanic owner, prob"="prob_hispanic","house, sold in period"="saleinperiod","house, poor condition"="lowfaircondition","house, actual value"="ActualValue","house, Square Footage"="SquareFootage","house, sold last pre-2012"="lastsalepre2012","block group, housing cost burdened"="HCB","house, evaporative cooler"="evapcooler","house, electric baseboard"="ebaseboard","house, central air"="centralair","block group, less than hs education"="Pr_____","house, built in period"="newbuild","google rating missing"="norating","google rating"="rating.joined","prob. hispanic, ln"="hisp","total installs, ln"="Freq","government/non-profit"="GovNGO","house, corporate owner"="institional","block group, people of color (percent)"="POC")
  X %>% dplyr::rename(any_of(lookupt))}


puebr4<-readRDS("/Users/rpscott/Documents/GitHub/RESPECT-QTS/pueblo_housing_data/processed_data/full_s_2018.version3.rds")
puebr5<-readRDS("/Users/rpscott/Documents/GitHub/RESPECT-QTS/pueblo_housing_data/processed_data/permit_s_2018.version3.rds")
pueblo_contractors<-read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vRj8AkwjfguXDbr14v80dLPcOFzQxjKZ9ZMKbln08X7gQPNDgT94mS7alolTmwH6mCais4Nd0g2T5nK/pub?output=csv")
pueblo_contractors$ID<-paste0("c",1:nrow(pueblo_contractors))

puebflat<-rbind(pueblo_contractors %>% select(-Name.2),pueblo_contractors %>% select(-Name) %>% filter(nchar(Name.2)>0) %>% rename(Name=Name.2))

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

head(c_unique)

library(igraph)
g1<-c_unique %>% select(ID,EnergyStar,BPI,NCI,RENU,NATE,ACCA,PHCC,SMACNA,EStarACCABPINCI) %>% filter(EStarACCABPINCI>0) %>% select(-EStarACCABPINCI)
head(g1)
g1<-g1 %>% reshape2::melt() %>% na.omit()  %>% .[,1:2] %>% as.matrix() %>% graph_from_edgelist()

library(INLA)
t1<-inla(as.numeric(EStarACCABPINCI>0)~norating+scale(rating.joined)+scale(log(Freq+1))+scale(log(hisp))+GovNGO, data=c_unique,family="binomial")


puebr5<-readRDS("/Users/rpscott/Documents/GitHub/RESPECT-QTS/pueblo_housing_data/processed_data/permit_s_2018.version3.rds")


puebr5<-puebr5 %>% left_join(.,puebflat  %>% select(Name, ID) %>% rename(Contractor=Name,ContID=ID) %>% unique(),na_matches = c("never"))

puebr5<-left_join(puebr5,c_unique %>% rename(ContID=ID) %>% select(ContID,EStarACCABPINCI,GovNGO,rating.joined,Freq),na_matches = c("never"))

puebr5$EStarACCABPINCI_cont<-puebr5$EStarACCABPINCI
puebr5$EStarACCABPINCI<-as.numeric(puebr5$EStarACCABPINCI>0)

puebr5$LandClassDescription %>% table(useNA = "always")
filter(puebr5, is.na(LandClassDescription))$ASSESSOR_PARID


puebr5<-filter(puebr5,LandClassDescription=="Residential")
puebr5<-unique(puebr5)

filter(puebr5, ASSESSOR_PARID==9531027001)



inlatable<-function(model) {
  model$summary.fixed %>% mutate(id=rownames(.)) %>% mutate(id=as.character(forcats::fct_recode(id,"google rating missing"="noratingTRUE","google rating"="scale(rating.joined)","prob. hispanic, ln"="scale(log(hisp))","total installs, ln"="scale(log(Freq + 1))","government/non-profit"="GovNGO"))) %>% ggplot()+geom_vline(aes(xintercept=0),lty=2)+geom_pointrange(aes(y=id,x=mean,xmin=`0.025quant`,xmax=`0.975quant`))+theme_minimal()+xlab("95% credible interval and mean")+ylab("predictor")
}
inlatable(t2)

library(INLA)


m1a<-inla(EStarACCABPINCI_cont~scale(HCB)+
            scale(Pr_____)+
            f(YearBuilt,model="rw2")+
            lowfaircondition+
            centralair+
            scale(prob_hispanic)+
            scale(as.numeric(ActualValue)/as.numeric(SquareFootage))+
            saleinperiod+
            lastsalepre2012+
            f(placeidinla,model="bym",graph="Documents/qt-local/mechanicals/ebnb.adj")+
            f(ASSESSOR_PARID,model="iid"),
          data=puebr5,
          family="nbinomial")

m1a$summary.fixed  %>% mutate(id=rownames(m1a$summary.fixed)) %>% mutate(id=as.character(forcats::fct_recode(id,"house, hispanic owner, prob"="scale(prob_hispanic)","house, sqft,ln"="scale(log(as.numeric(SquareFootage)))","house, assessor value"="scale(log(as.numeric(ActualValue)))","house, sold in period"="saleinperiodTRUE","house, poor condition"="lowfairconditionTRUE","house, assessed value per sqft"="scale(as.numeric(ActualValue)/as.numeric(SquareFootage))","house, sold last pre-2012"="lastsalepre2012TRUE","block group, housing cost burdened"="scale(HCB)","house, evaporative cooler"="evapcooler","house, electric baseboard"="ebaseboard","house, central air"="centralair","block group, less than hs education"="scale(Pr_____)","house, built in period"="newbuildTRUE")),var="permit, quality installation")  %>% ggplot()+geom_hline(aes(yintercept=0),lty=2)+geom_pointrange(aes(x=id,y=mean,ymin=`0.025quant`,ymax=`0.975quant`))+coord_flip()+theme_minimal()+ylab("95% fixed effect credible interval (DV=Quality Installer Points)")+xlab("variable")


puebr5$LandClassDescription %>% table()
puebr5<-filter(puebr5,LandClassDescription=="Residential")
table(puebr5$Grantees)
puebr5$institutional<-stringr::str_detect(puebr5$Grantees,"LLC|LLP|LP|INC|CORP")
head(puebr5)
puebr5$repair.replace<-str_detect(puebr5$Workclass,"^004R|REPLACE")
puebr5$ac<-str_detect(puebr5$Workclass,"AC|A\\/C")

puebr5[str_detect(puebr5$Workclass,"^004 "),]
puebr5$ducts<-str_detect(puebr5$Workclass,"DUCT|VENT")
filter(puebr5, ducts==T) %>% .$Workclass
puebr5$EStarACCABPINCI_cont[which(puebr5$Contractor=="SELF")]<-0
puebr5<-puebr5[-which(is.na(puebr5$EStarACCABPINCI_cont)),]
#puebr5$Contractor[which(is.na(puebr5$EStarACCABPINCI_cont))]



#puebr5$institutional[which(puebr5$saleinperiod==TRUE)]<-NA
puebr5$SaleYear<-puebr5$SaleDate %>% lubridate::ymd() %>% lubridate::year()
puebr5$saleyearp.p1<-puebr5$SaleYear%in%c(puebr5$Year,puebr5$Year+1)
puebr5$saleyearm1<-puebr5$SaleYear%in%c(puebr5$Year-1)

puebr5 %>% select(EStarACCABPINCI_cont,HCB, Pr_____,lowfaircondition,centralair,prob_hispanic,ActualValue,SquareFootage,saleinperiod,lastsalepre2012,POC,saleyearm1,saleyearp.p1) %>% recodeCATS2 %>%  gtsummary::tbl_summary(statistic = list(
  gtsummary::all_continuous() ~ "{mean} ({sd})",
  gtsummary::all_categorical() ~ "{n} / {N} ({p}%)"
),
digits = gtsummary::all_continuous() ~ 2) %>% gtsummary::as_hux_xlsx("Documents/GitHub/RESPECT-QTS/pueblo_housing_data/results/resident_descriptives_gtsummary1.xlsx")

pc.prec1 = list(prec = list(prior = "pc.prec", param = c(5, 0.01)))

modelformula<-as.numeric(EStarACCABPINCI_cont)~1+scale(HCB)+
  scale(Pr_____)+
  scale(YearBuilt)+
  centralair+
  f(Year,model="iid")+
  scale(prob_hispanic)+
  scale(POC)+
  scale(as.numeric(ActualValue))+
  scale(as.numeric(SquareFootage))+
  saleyearp.p1+
  saleyearm1+
  f(placeidinla,model="bym2",graph="Documents/qt-local/mechanicals/ebnb.adj")+
  f(ASSESSOR_PARID,model="iid",hyper=pc.prec1)

modelformula1<-as.numeric(EStarACCABPINCI_cont>0)~1+scale(HCB)+
  institutional+
  scale(Pr_____)+
  scale(YearBuilt)+
  centralair+
  f(Year,model="iid")+
  scale(prob_hispanic)+
  scale(POC)+
  scale(as.numeric(ActualValue))+
  scale(as.numeric(SquareFootage))+
  saleinperiod+
  f(placeidinla,model="bym",graph="Documents/qt-local/mechanicals/ebnb.adj")+
  f(ASSESSOR_PARID,model="iid")


#remotes::install_github("inbo/inlatools")



model.binomial <- 
  inla(modelformula1,data=puebr5,family="binomial")
model.poisson <- 
  inla(modelformula,data=puebr5,family="poisson")
model.gaussian<-inla(modelformula,
                     data=puebr5,family="gaussian",control.compute=list(config = TRUE),
                     control.family = list(hyper = pc.prec1))


model.gaussian$summary.fixed  %>% mutate(id=rownames(model.gaussian$summary.fixed)) %>% mutate(id=as.character(forcats::fct_recode(id,"house, hispanic owner prob"="scale(prob_hispanic)","house, square footage"="scale(as.numeric(SquareFootage))","house, assessor value"="scale(as.numeric(ActualValue))","house, sold in period"="saleinperiodTRUE","house, poor condition"="lowfairconditionTRUE","house, year built"="scale(YearBuilt)","house, assessed value per sqft"="scale(as.numeric(ActualValue)/as.numeric(SquareFootage))","house, sold last pre-2012"="lastsalepre2012TRUE","block group, housing cost burdened"="scale(HCB)","house, evaporative cooler"="evapcooler","house, electric baseboard"="ebaseboard","house, central air"="centralair","block group, less than hs education"="scale(Pr_____)","house, built in period"="newbuildTRUE","block group, % of color"="scale(POC)","house, corporate owner"="institutionalTRUE","house,sold in period"="saleinperiod","house, sold year or year+1"="saleyearp.p1TRUE","house, sold year prior"="saleyearm1TRUE")),var="permit, quality installation") %>% select(id,mean,`0.025quant`,`0.975quant`) %>% write.csv("Documents/GitHub/RESPECT-QTS/pueblo_housing_data/results/qualitymodeloutput.fixed.csv")
model.gaussian$summary.hyperpar %>% mutate(id=row.names(.)) %>% select(id,mean,sd) %>% write.csv("Documents/GitHub/RESPECT-QTS/pueblo_housing_data/results/qualitymodeloutput.hyper.csv")

#Binomial tests
binomialtest<-function(){
model.base <- 
  inla(as.numeric(EStarACCABPINCI_cont>0)~scale(HCB)+
         institutional+
         scale(Pr_____)+
         f(YearBuilt,model="rw2")+
         centralair+
         f(Year,model="iid")+
         scale(log(prob_hispanic))+
         scale(POC)+
         scale(as.numeric(ActualValue))+
         scale(as.numeric(SquareFootage))+
         saleinperiod+
         lastsalepre2012+
         f(placeidinla,model="bym",graph="Documents/qt-local/mechanicals/ebnb.adj")+
         f(ASSESSOR_PARID,model="iid"),
       data=puebr5,family="binomial",control.family = list(variant=1),control.compute=list(config = TRUE),control.predictor=list(link = 1))

model.l1 <-inla(as.numeric(EStarACCABPINCI_cont>1)~scale(HCB)+
         institutional+
         scale(Pr_____)+
         f(YearBuilt,model="rw2")+
         centralair+
         f(Year,model="iid")+
         scale(log(prob_hispanic))+
         scale(POC)+
         scale(as.numeric(ActualValue))+
         scale(as.numeric(SquareFootage))+
         saleinperiod+
         lastsalepre2012+
         f(placeidinla,model="bym",graph="Documents/qt-local/mechanicals/ebnb.adj")+
         f(ASSESSOR_PARID,model="iid"),
       data=puebr5,family="binomial",control.family = list(variant=1),control.compute=list(config = TRUE),control.predictor=list(link = 1))
  
model.l2 <- inla(as.numeric(EStarACCABPINCI_cont>1)~scale(HCB)+
         institutional+
         scale(Pr_____)+
         f(YearBuilt,model="rw2")+
         centralair+
         f(Year,model="iid")+
         scale(log(prob_hispanic))+
         scale(POC)+
         scale(as.numeric(ActualValue))+
         scale(as.numeric(SquareFootage))+
         saleinperiod+
         lastsalepre2012+
         f(placeidinla,model="bym",graph="Documents/qt-local/mechanicals/ebnb.adj")+
         f(ASSESSOR_PARID,model="iid"),
       data=puebr5,family="binomial",control.family = list(variant=1),control.compute=list(config = TRUE),control.predictor=list(link = 1))
  
model.l3 <- inla(as.numeric(EStarACCABPINCI_cont>3)~1+scale(HCB)+
         institutional+
         scale(Pr_____)+
         f(YearBuilt,model="rw2")+
         centralair+
         f(Year,model="iid")+
         scale(log(prob_hispanic))+
         scale(POC)+
         scale(as.numeric(ActualValue))+
         scale(as.numeric(SquareFootage))+
         saleinperiod+
         lastsalepre2012+
         f(placeidinla,model="bym",graph="Documents/qt-local/mechanicals/ebnb.adj")+
         f(ASSESSOR_PARID,model="iid"),
       data=puebr5,family="binomial",control.family = list(variant=1),control.compute=list(config = TRUE),control.predictor=list(link = 1))
  


model.base$summary.fixed$m<-"base"
model.l1$summary.fixed$m<-">1"
model.l2$summary.fixed$m<-">2"
model.l3$summary.fixed$m<-">3"

rbind(model.base$summary.fixed  %>% mutate(id=rownames(.)),model.l1$summary.fixed %>% mutate(id=rownames(.))) %>% rbind(model.l2$summary.fixed %>% mutate(id=rownames(.))) %>% rbind(model.l3$summary.fixed %>% mutate(id=rownames(.))) %>% mutate(id=as.character(forcats::fct_recode(id,"house, hispanic owner, prob"="scale(log(prob_hispanic))","house, sqft"="scale(as.numeric(SquareFootage))","house, assessor value"="scale(as.numeric(ActualValue))","house, sold in period"="saleinperiodTRUE","house, poor condition"="lowfairconditionTRUE","house, assessed value per sqft"="scale(as.numeric(ActualValue)/as.numeric(SquareFootage))","house, sold last pre-2012"="lastsalepre2012TRUE","block group, housing cost burdened"="scale(HCB)","house, evaporative cooler"="evapcooler","house, electric baseboard"="ebaseboard","house, central air"="centralair","block group, less than hs education"="scale(Pr_____)","house, built in period"="newbuildTRUE","house, corporate owner"="institutionalTRUE","house, sold within time period"="saleinperiod","block group, percent of color"="scale(POC)")),var="permit, quality installation")  %>% ggplot()+geom_hline(aes(yintercept=0),lty=2)+geom_pointrange(aes(x=id,y=mean,ymin=`0.025quant`,ymax=`0.975quant`,colour=m),position=position_dodge(width=.4))+coord_flip()+theme_minimal()+ylab("95% fixed effect credible interval (DV=Quality Installer Points)")+xlab("variable")+theme(text=element_text(size=14))+ggthemes::scale_colour_tableau(name="Binomial cut point")+theme(legend.position=c(.3,.8),legend.background = element_rect(fill="white"))
}
binomialtest()

library(ggplot2)
plot1<-model.gaussian$summary.fixed  %>% mutate(id=rownames(model.gaussian$summary.fixed)) %>% mutate(id=as.character(forcats::fct_recode(id,"house, hispanic owner prob"="scale(prob_hispanic)","house, square footage"="scale(as.numeric(SquareFootage))","house, assessor value"="scale(as.numeric(ActualValue))","house, sold in period"="saleinperiodTRUE","house, poor condition"="lowfairconditionTRUE","house, year built"="scale(YearBuilt)","house, assessed value per sqft"="scale(as.numeric(ActualValue)/as.numeric(SquareFootage))","house, sold last pre-2012"="lastsalepre2012TRUE","block group, housing cost burdened"="scale(HCB)","house, evaporative cooler"="evapcooler","house, electric baseboard"="ebaseboard","house, central air"="centralair","block group, less than hs education"="scale(Pr_____)","house, built in period"="newbuildTRUE","block group, % of color"="scale(POC)","house, corporate owner"="institutionalTRUE","house,sold in period"="saleinperiod","house, sold year or year+1"="saleyearp.p1TRUE","house, sold year prior"="saleyearm1TRUE")),var="permit, quality installation")  %>% mutate(sig=ifelse(c(c(model.gaussian$summary.fixed$`0.025quant`<0) & c(model.gaussian$summary.fixed$`0.975quant`<0) ) | c(c(model.gaussian$summary.fixed$`0.025quant`>0) & c(model.gaussian$summary.fixed$`0.975quant`>0))==T,"CI excludes 0","CI surrounds 0")) %>% ggplot()+geom_hline(aes(yintercept=0),lty=2)+geom_pointrange(aes(x=id,y=mean,ymin=`0.025quant`,ymax=`0.975quant`,colour=sig))+coord_flip()+theme_minimal()+ylab("95% fixed effect credible interval\n (DV=Quality Installer Points)")+xlab("variable")+theme(text=element_text(size=18))+ggthemes::scale_color_colorblind(name="stat. signif.")+theme(legend.position=c(.75,.75),legend.background=element_rect(fill="white"),plot.background =element_rect(fill="white") ) 
plot1 %>% ggsave(filename="Documents/GitHub/RESPECT-QTS/pueblo_housing_data/results/plot1.tiff",plot=.,width=8,height=4,units="in")
plot1 %>% ggsave(filename="Documents/GitHub/RESPECT-QTS/pueblo_housing_data/results/plot1.png",plot=.,width=8,height=4,units="in")


ggplot(model.gaussian$summary.random$placeidinla)+geom_pointrange(aes(x=ID,y=mean,ymax=`0.975quant`,ymin=`0.025quant`))+theme_minimal()

ebg<-sf::read_sf("Downloads/Colorado_EnviroScreen_v1_BlockGroup.geojson")
ebg<-filter(ebg,Cnt_N=="Pueblo County")
ebg$placeidinla<-1:nrow(ebg)


nrow(model.gaussian$summary.random$placeidinla)
library(sf)
library(ggspatial)
sp0<-ebg %>% select(OBJECTID,placeidinla) %>% left_join(.,data.frame("placeidinla"=model.gaussian$summary.random$placeidinla$ID,"mean"=model.gaussian$summary.random$placeidinla$mean[1:134]+model.gaussian$summary.random$placeidinla$mean[135:268])) %>% st_crop(census_data, xmin = -104.6, xmax = -104.7, ymin = 38.2, ymax = 38.3) %>% ggplot()+annotation_map_tile(zoom=13,type="stamenbw")+geom_sf(aes(fill=mean),alpha=.5)+scale_fill_viridis_c(name="mean intercept")+ggthemes::theme_map()

sp1<-ebg %>% select(OBJECTID,placeidinla) %>% left_join(.,data.frame("placeidinla"=model.gaussian$summary.random$placeidinla$ID,"mean"=model.gaussian$summary.random$placeidinla$mean[1:134]+model.gaussian$summary.random$placeidinla$mean[135:268])) %>% ggplot()+geom_sf(aes(fill=mean),alpha=.5)+scale_fill_viridis_c(name="mean intercept")+ggthemes::theme_map()

is1<-model.gaussian$summary.random$placeidinla[1:134,] %>% arrange(mean) %>% .$ID
sp2<-ggplot()+geom_pointrange(data=model.gaussian$summary.random$placeidinla[1:134,] %>% arrange(mean),aes(x=1:134,y=mean,ymin=`0.025quant`,ymax=`0.975quant`),size=.1)+theme_minimal()+xlab("iid component (ranked)")+ylab("intercept credible interval")
sp3<-ggplot()+geom_pointrange(data=model.gaussian$summary.random$placeidinla[135:268,] %>% .[is1,],aes(x=.3+c(1:134),y=mean,ymin=`0.025quant`,ymax=`0.975quant`),size=.1)+xlab("spatial component  (iid ranked)")+ylab("intercept credible interval")+theme_minimal()

library(cowplot)
spaceplot<-plot_grid(sp1, sp0, sp2,sp3, nrow=2,align="none")
spaceplot %>% ggsave(filename="Documents/GitHub/RESPECT-QTS/pueblo_housing_data/results/plot2.png",plot=.,width=10,height=6,units="in")
spaceplot %>% ggsave(filename="Documents/GitHub/RESPECT-QTS/pueblo_housing_data/results/plot2.tiff",plot=.,width=10,height=6,units="in")



iidplot<-table(puebr5$ASSESSOR_PARID)[c(table(puebr5$ASSESSOR_PARID)>1)] %>% reshape2::melt() %>% rename(ID=Var1) %>% left_join(.,model.gaussian$summary.random$ASSESSOR_PARID) %>% arrange(mean) %>% mutate(newid=1:nrow(.)) %>% ggplot()+geom_hline(aes(yintercept=0),lty=2)+geom_pointrange(aes(x=newid,y=mean,ymin=`0.025quant`,ymax=`0.975quant`,alpha=log(value)),size=.1)+theme_minimal()+scale_alpha(name="ln count observations per parcel")+theme(legend.position=c(.3,.8))+xlab("parcels by ranked posterior mean")+ylab("95% CI and mean, count intermediary points")+theme(text=element_text(size=18))
iidplot %>% ggsave(filename="Documents/GitHub/RESPECT-QTS/pueblo_housing_data/results/plot3.png",plot=.,width=10,height=6,units="in")
iidplot %>% ggsave(filename="Documents/GitHub/RESPECT-QTS/pueblo_housing_data/results/plot3.tiff",plot=.,width=10,height=6,units="in")

sd(puebr5$YearBuilt %>% na.omit())

k1<-table(puebr5$ASSESSOR_PARID)[c(table(puebr5$ASSESSOR_PARID)>1)] %>% reshape2::melt() %>% rename(ID=Var1) %>% left_join(.,model.gaussian$summary.random$ASSESSOR_PARID) %>% arrange(mean) %>% mutate(newid=1:nrow(.)) %>% filter(newid>625) %>% select(ID) %>% rename(ASSESSOR_PARID=ID)
puebr5 %>% filter(ASSESSOR_PARID%in%k1$ASSESSOR_PARID)
summary(model.gaussian)
dsummary(m1a)
m1_binomial<-inla(EStarACCABPINCI_cont~scale(HCB)+institutional+
                    scale(Pr_____)+
                    f(YearBuilt,model="rw2",hyper=pc.prec1)+
                    centralair+
                    f(Year,model="iid",hyper=pc.prec1)+
                    scale(prob_hispanic)+
                    scale(POC)+
                    scale(log(as.numeric(ActualValue)))+
                    scale(as.numeric(SquareFootage))+
                    saleinperiod+
                    lastsalepre2012+
                    f(placeidinla,model="bym",graph="Documents/qt-local/mechanicals/ebnb.adj",hyper=pc.prec1)+
                    f(ASSESSOR_PARID,model="iid",hyper=pc.prec1),
            f(placeidinla,model="bym",graph="Documents/qt-local/mechanicals/ebnb.adj"),
          data=puebr5,
          family="binomial")

m1_cfull$summary.random$ContID
m1_cfull$summary.fixed
m1_cfull %>% inlatable
inla.list.models()

nongovmodel<-inla(EStarACCABPINCI_cont~scale(HCB)+
                 scale(Pr_____)+
                 f(YearBuilt,model="rw2")+
                 lowfaircondition+
                 centralair+
                 scale(prob_hispanic)+
                 scale(as.numeric(ActualValue)/as.numeric(SquareFootage))+
                 saleinperiod+
                 lastsalepre2012+
                 f(placeidinla,model="bym",graph="Documents/qt-local/mechanicals/ebnb.adj"),
               data=puebr5 %>% filter(GovNGO==0) %>% filter(is.na(YearBuilt)==F),
               family="nbinomial")


nobuckmodel<-inla(EStarACCABPINCI_cont~scale(HCB)+
                    scale(Pr_____)+
                    f(YearBuilt,model="rw2")+
                    lowfaircondition+
                    centralair+
                    scale(prob_hispanic)+
                    scale(as.numeric(ActualValue)/as.numeric(SquareFootage))+
                    saleinperiod+
                    lastsalepre2012+
                    f(placeidinla,model="bym",graph="Documents/qt-local/mechanicals/ebnb.adj"),
                  data=puebr5 %>% filter(ContID!="c240") %>% filter(is.na(YearBuilt)==F),
                  family="nbinomial")


contractors_listmodel<-lapply(unique(puebr5$ContID)
,function(X){try({inla(modelformula,
                  data=filter(puebr5,ContID%in%X==F),
                  family="gaussian",
                  control.family = list(hyper = pc.prec1)) %>% .$summary.fixed  %>% mutate(id=rownames(.)) %>% select(id,mean,`0.025quant`,`0.975quant`)})})

names(contractors_listmodel)<-as.character(unique(puebr5$ContID))

c2<-dplyr::bind_rows(contractors_listmodel,.id="ContID")
head(c2)
#c2$ContID<-unique(puebr5$ContID)[as.numeric(c2$ContID)]

#c2$ContID<-unique(puebr5$ContID)[as.numeric(c2$ContID)]
"largest installer"
table(puebr5$ContID) %>% sort(decreasing=T) %>% .[1:6]
table(puebr5$Contractor) %>% sort(decreasing=T) %>% .[1:6]
puebr5$ContID %>% table() %>% sort()
c2<-c2 %>% mutate(observation=ifelse(ContID%in%unique(filter(puebr5, GovNGO==T)$ContID),"government installers",ifelse(ContID%in%c("c240","c296","c22","c123","c239"),"largest installers",ifelse(ContID%in%c("c261"),"self installs","other installers"))))
table(c2$observation)

recodeCATS<-function(X){ X %>% mutate(id=as.character(forcats::fct_recode(id,"house, hispanic owner, prob"="scale(prob_hispanic)","house, sqft"="scale(as.numeric(SquareFootage))","house, assessor value"="scale(as.numeric(ActualValue))","house, sold in period"="saleinperiodTRUE","house, poor condition"="lowfairconditionTRUE","house, assessed value per sqft"="scale(as.numeric(ActualValue)/as.numeric(SquareFootage))","house, sold last pre-2012"="lastsalepre2012TRUE","block group, housing cost burdened"="scale(HCB)","house, evaporative cooler"="evapcooler","house, electric baseboard"="ebaseboard","house, central air"="centralair","block group, less than hs education"="scale(Pr_____)","house, built in period"="newbuildTRUE","block group, people of color"="scale(POC)","house, year built"="scale(YearBuilt)","house,sold in period"="saleinperiod","house, sold year or year+1"="saleyearp.p1TRUE","house, sold year prior"="saleyearm1TRUE")))}
which.min(filter(c2, id=="house, sqft")$mean)
filter(c2, id=="house, sqft")[135,]
filter(puebr5, ContID=="c287") 
min(mean)
c2<-c2 %>% mutate(model="leave one out") %>% recodeCATS 

lwo<-ggplot()+geom_vline(aes(xintercept=0),lty=2)+geom_boxplot(data=filter(c2, observation=="other installers"),aes(x=mean,y=id,colour=observation))+geom_pointrange(data=filter(c2,observation!="other installers"),alpha=.8,aes(x=mean,y=id,colour=observation,xmin=`0.025quant`,xmax=`0.975quant`),position=position_dodge(width=.5))+theme_minimal()+theme(legend.position=c(.6,.6))+ggthemes::scale_color_colorblind(name="observation omitted")
lwo %>% ggsave(filename="Documents/GitHub/RESPECT-QTS/pueblo_housing_data/results/plot4.tiff",plot=.,width=10,height=6,units="in")
lwo %>% ggsave(filename="Documents/GitHub/RESPECT-QTS/pueblo_housing_data/results/plot4.png",plot=.,width=10,height=6,units="in")



ggplot()+geom_point(aes(x=log(c_unique$Freq+1),y=c_unique$EStarACCABPINCI))+geom_smooth(aes(x=log(c_unique$Freq+1),y=c_unique$EStarACCABPINCI))+ylab("Quality Installer Points")+xlab("ln count installations by company")+theme_minimal()



ggplot()+geom_vline(aes(xintercept=0),lty=2)+geom_boxplot(data=filter(c2, observation=="other installers"),aes(x=mean,y=id,colour=observation))+geom_pointrange(data=filter(c2,observation=="government installers"),alpha=.8,aes(x=mean,y=id,colour=observation,xmin=`0.025quant`,xmax=`0.975quant`))+theme_minimal()+theme(legend.position=c(.6,.6))

ggplot()+geom_vline(aes(xintercept=0),lty=2)+geom_boxplot(data=filter(c2, observation=="other installers"),aes(x=mean,y=id,colour=observation))+geom_pointrange(data=filter(c2,observation=="largest installers"),alpha=.8,aes(x=mean,y=id,colour=observation,xmin=`0.025quant`,xmax=`0.975quant`))+theme_minimal()+theme(legend.position=c(.6,.6))



puebr5$Contractor %>% table(.) %>% sort(decreasing=T) %>% .[1:10]
puebr5$ContID[puebr5$Contractor=="FLOW RIGHT PLUMBING HEATING & IRR, INC"]
c2 %>% mutate(model="leave one out") %>% recodeCATS %>% ggplot(.,aes(x=mean,y=id,colour=observation,xmin=`0.025quant`,xmax=`0.975quant`))+geom_vline(aes(xintercept=0),lty=2)+geom_pointrange(alpha=.4)+theme_minimal()

c2 %>% mutate(model="leave one out") %>% recodeCATS %>% ggplot(.,aes(x=mean,y=ContID,colour=observation,xmin=`0.025quant`,xmax=`0.975quant`),size=.1)+geom_vline(aes(xintercept=0),lty=2)+geom_pointrange(alpha=.4)+theme_minimal()+facet_wrap(~id,nrow=2,scale="free_x")


c2 %>% mutate(model="leave one out") %>% recodeCATS %>% ggplot(.,aes(x=mean,y=id,colour=model))+geom_boxplot(outlier.size=1)+geom_point(data=nongovmodel$summary.fixed %>% mutate(ContID="no governments",id=as.character(rownames(.)),model="no governments") %>% select(ContID,id,mean,model) %>% rbind(m1a$summary.fixed %>% mutate(ContID="full",id=as.character(rownames(.)),model="full") %>% select(ContID,id,mean,model)) %>% rbind(m1_binomial$summary.fixed %>% mutate(ContID="basic binomial",id=as.character(rownames(.)),model="basic binomial") %>% select(ContID,id,mean,model)) %>% recodeCATS,size=3)+theme_minimal()+geom_vline(aes(xintercept=0),lty=2)+ggthemes::scale_color_colorblind()+xlab("fixed effect mean")+ylab("variable")+theme(legend.position=c(.7,.5),legend.background = element_rect(colour="black"))

recodeCATS2<-function(X){ 
  lookupt<-c("house, hispanic owner, prob"="prob_hispanic","house, sold in period"="saleinperiod","house, poor condition"="lowfaircondition","house, actual value"="ActualValue","Square Footage"="SquareFootage","house, sold last pre-2012"="lastsalepre2012","block group, housing cost burdened"="HCB","house, evaporative cooler"="evapcooler","house, electric baseboard"="ebaseboard","house, central air"="centralair","block group, less than hs education"="Pr_____","house, built in period"="newbuild","google rating missing"="norating","google rating"="rating.joined","prob. hispanic, ln"="hisp","total installs, ln"="Freq","government/non-profit"="GovNGO","house,sold in period"="saleinperiod","house, sold year or year+1"="saleyearp.p1TRUE")
  X %>% dplyr::rename(any_of(lookupt))}
library(gtsummary)


c_unique %>% select(EStarACCABPINCI,GovNGO,hisp,Freq,rating.joined,norating) %>% recodeCATS2 %>%  gtsummary::tbl_summary(data=.,
  statistic = list(
    all_continuous() ~ "{mean} ({sd})",
    all_categorical() ~ "{n} / {N} ({p}%)"
  ),
  digits = all_continuous() ~ 2) %>%
  # export to Excel
  gtsummary::as_hux_xlsx("Documents/GitHub/RESPECT-QTS/pueblo_housing_data/results/company_descriptives_gtsummary1.xlsx")

c_unique$es

filter(c2,id=="lowfairconditionTRUE") %>% filter(abs(mean-.125)>.01) %>% ggplot()+geom_point(aes(x=mean,y=ContID)) 


table(puebr5$ContID) %>% sort(decreasing=T) %>% as.data.frame() %>% ggplot()+geom_bar(aes(x=Var1,y=Freq),stat="identity")+theme_bw()+coord_flip()+xlab("Contractor ID")+ylab("count of installations")+theme(axis.text.y=element_text(size=.1))




table(puebr5$ContID) %>% sort(decreasing=T) %>% as.data.frame() %>% ggplot()+geom_bar(aes(x=Var1,y=Freq),stat="identity")+theme_bw()+coord_flip()+xlab("Contractor ID")+ylab("count of installations")+theme(axis.text.y=element_text(size=.1))
hist(puebr5$EStarACCABPINCI_cont)


hist(puebr5$EStarACCABPINCI_cont)


filter(c_unique,ID=="c240")
filter(c_unique,ID=="c296")
table(puebr5$Contractor) %>% sort(decreasing=T) %>% .[1:10]


govmodel<-govmodel$summary.fixed  %>% mutate(id=rownames(govmodel$summary.fixed)) %>% mutate(id=as.character(forcats::fct_recode(id,"house, hispanic owner, prob"="scale(prob_hispanic)","house, sqft,ln"="scale(log(as.numeric(SquareFootage)))","house, assessor value"="scale(log(as.numeric(ActualValue)))","house, sold in period"="saleinperiodTRUE","house, poor condition"="lowfairconditionTRUE","house, asssed value per sqft"="scale(as.numeric(ActualValue)/as.numeric(SquareFootage))","house, sold last pre-2012"="lastsalepre2012TRUE","block group, housing cost burdened"="scale(HCB)","house, evaporative cooler"="evapcooler","house, electric baseboard"="ebaseboard","house, central air"="centralair","block group, less than hs education"="scale(Pr_____)","house, built in period"="newbuildTRUE")),var="permit, quality installation")  %>% ggplot()+geom_hline(aes(yintercept=0),lty=2)+geom_pointrange(aes(x=id,y=mean,ymin=`0.025quant`,ymax=`0.975quant`))+coord_flip()+theme_minimal()+ylab("95% fixed effect credible interval (DV=Mechanical Permit in 2022)")+xlab("variable")

nongovmodel<-nongovmodel$summary.fixed  %>% mutate(id=rownames(nongovmodel$summary.fixed)) %>% mutate(id=as.character(forcats::fct_recode(id,"house, hispanic owner, prob"="scale(prob_hispanic)","house, sqft,ln"="scale(log(as.numeric(SquareFootage)))","house, assessor value"="scale(log(as.numeric(ActualValue)))","house, sold in period"="saleinperiodTRUE","house, poor condition"="lowfairconditionTRUE","house, asssed value per sqft"="scale(as.numeric(ActualValue)/as.numeric(SquareFootage))","house, sold last pre-2012"="lastsalepre2012TRUE","block group, housing cost burdened"="scale(HCB)","house, evaporative cooler"="evapcooler","house, electric baseboard"="ebaseboard","house, central air"="centralair","block group, less than hs education"="scale(Pr_____)","house, built in period"="newbuildTRUE")),var="permit, quality installation")  %>% ggplot()+geom_hline(aes(yintercept=0),lty=2)+geom_pointrange(aes(x=id,y=mean,ymin=`0.025quant`,ymax=`0.975quant`))+coord_flip()+theme_minimal()+ylab("95% fixed effect credible interval (DV=Mechanical Permit in 2022)")+xlab("variable")


nobuckmodel<-nobuckmodel$summary.fixed  %>% mutate(id=rownames(nobuckmodel$summary.fixed)) %>% mutate(id=as.character(forcats::fct_recode(id,"house, hispanic owner, prob"="scale(prob_hispanic)","house, sqft,ln"="scale(log(as.numeric(SquareFootage)))","house, assessor value"="scale(log(as.numeric(ActualValue)))","house, sold in period"="saleinperiodTRUE","house, poor condition"="lowfairconditionTRUE","house, asssed value per sqft"="scale(as.numeric(ActualValue)/as.numeric(SquareFootage))","house, sold last pre-2012"="lastsalepre2012TRUE","block group, housing cost burdened"="scale(HCB)","house, evaporative cooler"="evapcooler","house, electric baseboard"="ebaseboard","house, central air"="centralair","block group, less than hs education"="scale(Pr_____)","house, built in period"="newbuildTRUE")),var="permit, quality installation")  %>% ggplot()+geom_hline(aes(yintercept=0),lty=2)+geom_pointrange(aes(x=id,y=mean,ymin=`0.025quant`,ymax=`0.975quant`))+coord_flip()+theme_minimal()+ylab("95% fixed effect credible interval (DV=Mechanical Permit in 2022)")+xlab("variable")


t1$summary.fixed %>% mutate(id=as.character(rownames(.)),model="leave one out") %>% rbind(m1a$summary.fixed %>% mutate(id=as.character(rownames(.))) %>% select(id,mean,model="full")) %>% ggplot()+geom_pointrange(aes(x=mean,y=id,xmin=`0.025quant`,xmax=`0.975quant`,colour=model))+theme_minimal()+geom_vline(aes(xintercept=0),lty=2)

puebr5<-left_join(puebr5,puebflat %>% select(Name,ID) %>% unique() %>% rename(Contractor=Name),na_matches="never")
linkedids<-c_unique$ID[c_unique$EStarACCABPINCI>0]
hcount<-puebr5 %>% select(YearBuilt,lowfaircondition,centralair,prob_hispanic,ActualValue,saleinperiod) %>% mutate(prob_hispanic=as.numeric(prob_hispanic)>.5,ActualValue=as.numeric(ActualValue)>300000, YearBuilt=YearBuilt>1976) %>% na.omit() %>% plyr::count() %>% rename(housecount=freq)
hcount$HID<-paste0("H",1:nrow(hcount))
hcount[1,]
hcount[2,]
#bycont<-bycont %>% select(-freq)
#ifaddid, include ,ID=ifelse(ID%in%linkedids,ID,"c000 (nogroup)")
ccount<-puebr5 %>% select(ContID,YearBuilt,lowfaircondition,centralair,prob_hispanic,ActualValue,saleinperiod) %>% mutate(prob_hispanic=as.numeric(prob_hispanic)>.5,ActualValue=as.numeric(ActualValue)>300000, YearBuilt=YearBuilt>1976) %>% na.omit() %>% plyr::count() %>% rename(contcount=freq)
fullc<-left_join(ccount,hcount)

g1<-g1 %>% rename(ContID=ID)

certhouse<-left_join(g1 %>% reshape2::melt(),table(puebr5$ContID) %>% as.data.frame() %>% rename(ContID=Var1)) %>% na.omit

houseframe<-data.frame("from"=fullc$ContID,"to"=fullc$HID,"count"=fullc$contcount) %>% rbind(data.frame("from"=certhouse$variable,"to"=certhouse$ContID,"count"=certhouse$Freq)) %>% uncount(count)
nodes<-data.frame("name"=c(houseframe$from,houseframe$to)) %>% count(name)
nodes$class=ifelse(stringr::str_detect(nodes$name,"^c"),"Contractor",ifelse(stringr::str_detect(nodes$name,"^H"),"House","Intermediary"))

hoedge<-houseframe %>% as.matrix() %>% igraph::graph_from_edgelist(directed=T)
E(hoedge)$weight <- 1
hoedge<-simplify(hoedge, edge.attr.comb=list(weight="sum"))
housenetwork<-igraph_to_networkD3(hoedge)
housenetwork$nodes<-left_join(housenetwork$nodes,nodes)

sankeyNetwork(Links=housenetwork$links,Nodes=housenetwork$nodes,nodeWidth=30,NodeGroup="class")
#install.packages("networkD3")
networkD3::forceNetwork(Links = housenetwork$links, Nodes = housenetwork$nodes,
             Source = "source", Target = "target",
             Value = "value", NodeID = "name",Nodesize="n",
             Group = "class", opacity = 0.8,charge=-200,fontSize = 12,opacityNoHover = 1)
install.packages("ggsci")
library(scales)
show_col(pal_d3("category10")(10))
install.packages("multiplex")
houseframe2<-houseframe
houseframe2$layer_from<-stringr::str_extract(houseframe$from,"^\\w")
houseframe2$layer_from[houseframe2$layer_from%in%c("c","H")==F]<-"I"
houseframe2$layer_to<-stringr::str_extract(houseframe$to,"^\\w")
houseframe2$layer_to[houseframe2$layer_to%in%c("c","H")==F]<-"I"
table(houseframe2$layer_from)
library(multinet)
net <- ml_empty()
add_layers_ml(net, c("I","c", "H"))
layers_ml(net)
dir1 <- data.frame(layer1 = c("I","c"), layer2 = c("c","H"),dir = 1)
set_directed_ml(net, dir1)
is_directed_ml(net)

actorframe<-rbind(houseframe2 %>% select(from, layer_from) %>% unique() %>% rename(actor=from,layer=layer_from),houseframe2 %>% select(to, layer_to) %>% unique() %>% rename(actor=to,layer=layer_to)) %>% unique()
add_vertices_ml(net, actorframe)
vertices_ml(net)
houseframe2<-houseframe2 %>% rename(actors_from=from,actors_to=to) 
houseframe2 %>% head()

houseframe2<-houseframe2[,c(1,3,2,4)]
houseframe2<-houseframe2 %>% left_join(.,houseframe2 %>% unique() %>% mutate(ID=1:nrow(.))) %>% group_by(ID) %>% add_count() %>% unique()

add_vertices_ml(net, actorframe)
vertices_ml(net)

add_edges_ml(net, houseframe2[,c(1:4)])
edges_ml(net)
houseframe2 %>% head()
add_attributes_ml(net, "weight", target = "edge", type = "numeric",layer1="c",layer2="H")

ml_clust <- glouvain_ml(net,omega=1)

head(ml_clust)

#fa<-houseframe %>% uncount(count)
#fb<-left_join(fa,fa %>% count(to))
fb<-unique(fb)

sankeyNetwork(Links=houseframe,Nodes=nodes,Source="from",Target="to",NodeID="nodeid",nodeWidth=30,fontSize=12)

View(flatframe %>% filter(to=="c000 (nogroup)"))
s

#E(hoedge)$weight<-houseframe$count
hoedge<-as.matrix(flatframe) %>% igraph::graph_from_edgelist(directed=T)

#devtools::install_github("fbreitwieser/sankeyD3")
library(sankeyD3)

hoedge.sankey<-sankeyD3::igraph_to_networkD3(hoedge)
?sankeyNetwork
head(hoedge.sankey)
?sankeyNetwork
sankeyNetwork(Links=hoedge.sankey$links,Nodes=hoedge.sankey$nodes,orderByPath=T)

sankeyNetwork(Links=,Nodes=hoedge.sankey$nodes,orderByPath=T)

g1<-c_unique %>% select(ID,EnergyStar,BPI,NCI,RENU,NATE,ACCA,PHCC,SMACNA,EStarACCABPINCI) %>% filter(EStarACCABPINCI>0) %>% select(-EStarACCABPINCI)
head(g1)
g1<-g1 %>% reshape2::melt() %>% na.omit()  %>% .[,1:2]

data.frame("to"=g1$ID,"from"=g1$variable,"count"=1)


sd(puebr5$YearBuilt, na.rm=T)
