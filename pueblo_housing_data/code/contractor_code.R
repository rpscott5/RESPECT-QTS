library(plyr)
library(tidyverse)
library(jsonlite)
puebr4<-readRDS("/Users/rpscott/Documents/GitHub/RESPECT-QTS/pueblo_housing_data/processed_data/full_s_2018.rds")
puebr5<-readRDS("/Users/rpscott/Documents/GitHub/RESPECT-QTS/pueblo_housing_data/processed_data/permit_s_2018.rds")
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
summary(t1)
t2<-inla(as.numeric(EStarACCABPINCI)~norating+scale(rating.joined)+scale(log(Freq+1))+scale(log(hisp))+GovNGO, data=c_unique,family="nbinomial")
summary(t2)


puebr5<-readRDS("/Users/rpscott/Documents/GitHub/RESPECT-QTS/pueblo_housing_data/processed_data/permit_s_2018.rds")


puebr5<-puebr5 %>% left_join(.,puebflat  %>% select(Name, ID) %>% rename(Contractor=Name,ContID=ID) %>% unique(),na_matches = c("never"))

puebr5<-left_join(puebr5,c_unique %>% rename(ContID=ID) %>% select(ContID,EStarACCABPINCI,GovNGO,rating.joined,Freq),na_matches = c("never"))
puebr5$EStarACCABPINCI_cont<-puebr5$EStarACCABPINCI
puebr5$EStarACCABPINCI<-as.numeric(puebr5$EStarACCABPINCI>0)
puebr5 %>% filter(ADDRID=="1055")
puebr5<-unique(puebr5)

puebr5<-puebr5 %>% select(-entityname)
puebr5 %>% select(Owner,GranteesLast)
puebr5[is.na(puebr5$LandClassDescription),]
readRDS("Documents/qt-local/building_api_calls/1510111001.rds") %>% head()
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
            f(placeidinla,model="bym",graph="Documents/qt-local/mechanicals/ebnb.adj"),
          data=puebr5,
          family="nbinomial")

UN.prior = "expression:
  log_dens = 0 - log(2) - theta / 2;
  return(log_dens);
"

inla.doc("nbinomial")
trypriors.models <- 
  inla(EStarACCABPINCI_cont~scale(HCB)+
    scale(Pr_____)+
    f(YearBuilt,model="rw2")+
    lowfaircondition+
    centralair+
    scale(prob_hispanic)+
    scale(as.numeric(ActualValue)/as.numeric(SquareFootage))+
    saleinperiod+
    lastsalepre2012+
    f(placeidinla,model="bym",graph="Documents/qt-local/mechanicals/ebnb.adj"),
  data=puebr5,family="nbinomial",control.family = list(variant=1))
trypriors.models$summary.fixed
puebr5 %>% select(EStarACCABPINCI_cont,HCB, Pr_____,lowfaircondition,centralair,prob_hispanic,ActualValue,SquareFootage,saleinperiod,lastsalepre2012) %>% recodeCATS2 %>%  gtsummary::tbl_summary(data=.,statistic = list(all_continuous() ~ "{mean} ({sd})",                   all_categorical() ~ "{n} / {N} ({p}%)"),digits = all_continuous() ~ 2) %>%
  # export to Excel
  gtsummary::as_hux_xlsx("Documents/GitHub/RESPECT-QTS/pueblo_housing_data/results/resident_descriptives_gtsummary1.xlsx")

dsummary(m1a)
m1_binomial<-inla(EStarACCABPINCI~scale(HCB)+
            scale(Pr_____)+
            f(YearBuilt,model="rw2")+
            lowfaircondition+
            centralair+
            scale(prob_hispanic)+
            scale(as.numeric(ActualValue)/as.numeric(SquareFootage))+
            saleinperiod+
            lastsalepre2012+
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
,function(X){ inla(EStarACCABPINCI_cont~scale(HCB)+
                    scale(Pr_____)+
                    f(YearBuilt,model="rw2")+
                    lowfaircondition+
                    centralair+
                    scale(prob_hispanic)+
                    scale(as.numeric(ActualValue)/as.numeric(SquareFootage))+
                    saleinperiod+
                    lastsalepre2012+
                    f(placeidinla,model="bym",graph="Documents/qt-local/mechanicals/ebnb.adj"),
                  data=filter(puebr5,ContID%in%X==F),
                  family="nbinomial") %>% .$summary.fixed  %>% mutate(id=rownames(.)) %>% select(id,mean)})

names(contractors_listmodel)<-unique(puebr5$ContID)
c2<-bind_rows(contractors_listmodel,.id="ContID")
c2$ContID<-unique(puebr5$ContID)[as.numeric(c2$ContID)]


recodeCATS<-function(X){ X %>% mutate(id=as.character(forcats::fct_recode(id,"house, hispanic owner, prob"="scale(prob_hispanic)","house, sqft,ln"="scale(log(as.numeric(SquareFootage)))","house, assessor value"="scale(log(as.numeric(ActualValue)))","house, sold in period"="saleinperiodTRUE","house, poor condition"="lowfairconditionTRUE","house, assessed value per sqft"="scale(as.numeric(ActualValue)/as.numeric(SquareFootage))","house, sold last pre-2012"="lastsalepre2012TRUE","block group, housing cost burdened"="scale(HCB)","house, evaporative cooler"="evapcooler","house, electric baseboard"="ebaseboard","house, central air"="centralair","block group, less than hs education"="scale(Pr_____)","house, built in period"="newbuildTRUE")))}

c2 %>% mutate(model="leave one out") %>% recodeCATS %>% ggplot(.,aes(x=mean,y=id,colour=model))+geom_boxplot(outlier.size=1)+geom_point(data=nongovmodel$summary.fixed %>% mutate(ContID="no governments",id=as.character(rownames(.)),model="no governments") %>% select(ContID,id,mean,model) %>% rbind(m1a$summary.fixed %>% mutate(ContID="full",id=as.character(rownames(.)),model="full") %>% select(ContID,id,mean,model)) %>% rbind(m1_binomial$summary.fixed %>% mutate(ContID="basic binomial",id=as.character(rownames(.)),model="basic binomial") %>% select(ContID,id,mean,model)) %>% recodeCATS,size=3)+theme_minimal()+geom_vline(aes(xintercept=0),lty=2)+ggthemes::scale_color_colorblind()+xlab("fixed effect mean")+ylab("variable")+theme(legend.position=c(.7,.5),legend.background = element_rect(colour="black"))

recodeCATS2<-function(X){ 
  lookupt<-c("house, hispanic owner, prob"="prob_hispanic","house, sold in period"="saleinperiod","house, poor condition"="lowfaircondition","house, actual value"="ActualValue","Square Footage"="SquareFootage","house, sold last pre-2012"="lastsalepre2012","block group, housing cost burdened"="HCB","house, evaporative cooler"="evapcooler","house, electric baseboard"="ebaseboard","house, central air"="centralair","block group, less than hs education"="Pr_____","house, built in period"="newbuild","google rating missing"="norating","google rating"="rating.joined","prob. hispanic, ln"="hisp","total installs, ln"="Freq","government/non-profit"="GovNGO")
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
hist(puebr5$EStarACCABPINCI_cont)


probmodel2<-m1a$summary.fixed  %>% mutate(id=rownames(m1a$summary.fixed)) %>% mutate(id=as.character(forcats::fct_recode(id,"house, hispanic owner, prob"="scale(prob_hispanic)","house, sqft,ln"="scale(log(as.numeric(SquareFootage)))","house, assessor value"="scale(log(as.numeric(ActualValue)))","house, sold in period"="saleinperiodTRUE","house, poor condition"="lowfairconditionTRUE","house, assessed value per sqft"="scale(as.numeric(ActualValue)/as.numeric(SquareFootage))","house, sold last pre-2012"="lastsalepre2012TRUE","block group, housing cost burdened"="scale(HCB)","house, evaporative cooler"="evapcooler","house, electric baseboard"="ebaseboard","house, central air"="centralair","block group, less than hs education"="scale(Pr_____)","house, built in period"="newbuildTRUE")),var="permit, quality installation")  %>% ggplot()+geom_hline(aes(yintercept=0),lty=2)+geom_pointrange(aes(x=id,y=mean,ymin=`0.025quant`,ymax=`0.975quant`))+coord_flip()+theme_minimal()+ylab("95% fixed effect credible interval (DV=Quality Installer Mechanical Permit in 2022)")+xlab("variable")
probmodel2

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
