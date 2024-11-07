library(plyr)
library(dplyr)
library(tidyverse)

sows<-readRDS("Documents/GitHub/RESPECT-QTS/denver_housing_data/processed_data/permits_scopes_of_work_20_23.rds")

allparcels<-readRDS("Documents/GitHub/RESPECT-QTS/denver_housing_data/processed_data/allparcels_20_23.rds")

sows$PAR_YEAR<-as.numeric(sows$Permit.. %>% stringr::str_extract(.,"^[0-9]+"))

sows<-sows %>% select(PAR_YEAR,SCHEDNUM,ac,ac_efficiency,ac_size,furnace,furnace_efficiency,furnace_size,heatpump.central,heatpump.mini,Valuation,Contractor.s.Name,inspector,sow) %>% unique() 
 

tops<-read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vT5w_SsHqNpDlkzD2CLaEkB0cejW0M0iurmPKQT5Benw98WTrQn9hCeSU8HvA4KLDijQAbZI2MZyDAV/pub?gid=1645890024&single=true&output=csv")

sows<-sows %>% group_by(PAR_YEAR,SCHEDNUM) %>% 
  reframe(
    ac=as.numeric(TRUE%in%ac),
    furnace=as.numeric(TRUE%in%furnace),
    furnace_efficiency=max(as.numeric(gsub("%","",furnace_efficiency))),
    ac_efficiency=max(as.numeric(ac_efficiency)),  
    heatpump.central=as.numeric(TRUE%in%heatpump.central),
    heatpump.mini=as.numeric(TRUE%in%heatpump.mini),
    Valuation=sum(Valuation,na.rm = T),
    furnace_size=paste(furnace_size,collapse=";"),
    ac_size=paste(ac_size,collapse=";"),
    Contractor.s.Name=paste(Contractor.s.Name,collapse=";"),
    sow=paste(sow,collapse=";"),
    inspector=paste(inspector,sep=";"))

allparcels2<-left_join(allparcels %>% as.data.frame() %>% select(-geometry),sows)
allparcels2$furnace[is.na(allparcels2$furnace)]<-0
allparcels2$ac[is.na(allparcels2$ac)]<-0
allparcels2$heatpump.central[is.na(allparcels2$heatpump.central)]<-0
allparcels2$Valuation[is.na(allparcels2$Valuation)]<-0
allparcels2<-filter(allparcels2,RES_ABOVE_>0)


allparcels2$ac_efficiency %>% table()
allparcels2$furnace_efficiency[allparcels2$furnace_efficiency==800]<-80
allparcels2$noncondense_furnace<-allparcels2$furnace_efficiency<=80
allparcels2$min_effic_ac<-allparcels2$ac_efficiency<=13
allparcels2$furnace_efficiency[allparcels2$furnace_efficiency==9200]<-92
allparcels2[which(allparcels2$Valuation>150000)[1],]$Permit..
allparcels2<-allparcels2 %>% mutate(surname=OWNER_LAST,state="CO",county=substr(GEOID20,3,5),tract=substr(GEOID20,6,11),blockgroup=substr(GEOID20,12,12),block=substr(GEOID20,12,15))

for_race<-allparcels2 %>% as.data.frame() %>% select(surname,state,county,tract,block) 

#cdatawru<-get_census_data(key=ckey,states="CO",year=2020,age=F,sex=F,census.geo="block",county.list=c("CO"=c("031")),retry=10)
#cdatawru %>% saveRDS(.,"Documents/GitHub/RESPECT-QTS/denver_housing_data/scratch/cdatawru.rds")

cdatawru<-readRDS("Documents/GitHub/RESPECT-QTS/denver_housing_data/scratch/cdatawru.rds")

library(wru)
prediction_race<-wru::predict_race(voter.file=for_race,census.surname=T,census.data=cdatawru,census.geo="block",age=F,impute.missing=T,use.counties=T,skip_bad_geos=T)
allparcels2<-left_join(allparcels2,unique(prediction_race))
allparcels2 %>% colnames %>% write.csv("makingdictionary.csv")

allparcels2 %>% write.csv("Documents/GitHub/RESPECT-QTS/denver_housing_data/processed_data/permits_merged_on_parcels_20_23.csv")

allparcels2 %>% saveRDS("Documents/GitHub/RESPECT-QTS/denver_housing_data/processed_data/permits_merged_on_parcels_20_23.rds")


allparcels2<-readRDS("Documents/GitHub/RESPECT-QTS/denver_housing_data/processed_data/permits_merged_on_parcels_20_23.rds")


permitted<-filter(allparcels2,mech.permit01==1)
permitted$noncondense_furnace<-as.numeric(permitted$noncondense_furnace)
permitted$min_effic_ac<-as.numeric(permitted$min_effic_ac)
permitted$acfurnheat<-permitted$furnace+permitted$ac+permitted$heatpump.central>=1
permitted<-filter(permitted,acfurnheat==T)
permitted$obsid<-1:nrow(permitted)
permitted$tract<-permitted$GEOID20 %>% substr(1,11)
head(permitted)
permitted$res_bin.copy<-permitted$res_bin
permitted$week<-permitted$Date.Issued %>% lubridate::mdy() %>% lubridate::week()
permitted$DATE<-permitted$Date.Issued %>% lubridate::mdy()
permitted$inspector[permitted$inspector=="NA"]<-NA
repeatc<-permitted %>% as.data.frame() %>% select(Contractor.s.Name,inspector,obsid,DATE) 


repeatc<-repeatc %>% arrange(DATE) %>% group_by(Contractor.s.Name,inspector) %>% mutate(count = seq(n()))
head(repeatc)
permitted<-left_join(permitted,repeatc %>% filter(is.na(inspector)==F))


contrecode<-read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vT5w_SsHqNpDlkzD2CLaEkB0cejW0M0iurmPKQT5Benw98WTrQn9hCeSU8HvA4KLDijQAbZI2MZyDAV/pub?gid=1645890024&single=true&output=csv")

permitted<-left_join(permitted,contrecode,na_matches = c("never"))

permitted$Contractor.s.Name<-ifelse(permitted$newcode=="",permitted$Contractor.s.Name,permitted$newcode)

permitted$xcelpartner.hp.2024[is.na(permitted$xcelpartner.hp.2024)]<-0


fp<-list.files("Documents/qt-local/denmet/Denver/permit_pages",full.names=T)
fp<-fp[basename(fp) %in% paste0(permitted$Permit..,".html")]
library(plyr)
library(dplyr)
denied<-lapply(fp, function(X) rvest::read_html(X) %>% rvest::html_text() %>% stringr::str_detect(.,"Denied"))
resultby<-lapply(fp, function(X) rvest::read_html(X) %>% rvest::html_text() %>% stringr::str_extract_all(.,"Result by\\: (\\w+ \\w+)"))
names(resultby)<-basename(fp[basename(fp) %in% paste0(permitted$Permit..,".html")])
resultby<-lapply(resultby, function(X) ifelse(length(X)>2, X[[2]],X[[1]]))
resultby<-reshape2::melt(resultby)
colnames(resultby)<-c("real_inspector","Permit..")
resultby$Permit..<-gsub(".html","",resultby$Permit..)
permitted<-left_join(permitted,resultby)
names(denied)<-basename(fp[basename(fp) %in% paste0(permitted$Permit..,".html")])
denied<-denied %>% reshape2::melt()
colnames(denied)[1]<-"failed_inspection"
colnames(denied)[2]<-"Permit.."
denied$Permit..<-gsub(".html","",denied$Permit..)
head(denied)
permitted<-left_join(permitted,denied)



permitted$failed_inspection<-as.numeric(permitted$failed_inspection)

permitted$sowschar<-nchar(permitted$sow)

cooling$RES_ORIG_YEAR_BUILT %>% min(na.rm=T)

permitted %>% filter(.,heatpump.central==1) %>% xtabs(~xcelpartner.hp.2024+PAR_YEAR,data=.) %>% as.data.frame() %>% ggplot()+geom_point(aes(x=PAR_YEAR,y=Freq,shape=xcelpartner.hp.2024,group="ac"))

#uptake of xcel energy partners
permitted %>% filter(.,ac==1) %>% xtabs(~xcelpartner.hp.2024+PAR_YEAR,data=.) %>% as.data.frame() %>% ggplot()+geom_point(aes(x=PAR_YEAR,y=Freq,shape=xcelpartner.hp.2024,group="ac"))

cooling %>% mutate(.,partnerlevel=topstotal+xcelpartner.hp.2024) %>% xtabs(~partnerlevel+PAR_YEAR,data=.) %>% as.data.frame() %>% ggplot()+geom_point(aes(x=PAR_YEAR,y=Freq,shape=partnerlevel,group="ac"))+facet

permitted$geo<-stringr::str_detect(tolower(permitted$sow),"geothermal|ground source|gshp")

cooling<-filter(permitted, ac==1|heatpump.central==1)
stringr::str_which(tolower(cooling$sow),"seer2")

#cooling$ac_efficiency<-floor(cooling$ac_efficiency) 
cooling$ac_efficiency[which(cooling$ac_efficiency==13.4)]<-14
cooling$ac_efficiency[which(cooling$ac_efficiency==13.8)]<-14.5
cooling$ac_efficiency[which(cooling$ac_efficiency==14.3)]<-15
cooling$ac_efficiency[which(cooling$ac_efficiency==14.8)]<-15.5
cooling$ac_efficiency[which(cooling$ac_efficiency==15.2)]<-16
cooling$ac_efficiency[which(cooling$ac_efficiency==16.2)]<-17
cooling$ac_efficiency[which(cooling$ac_efficiency==16.7)]<-17.5
cooling$ac_efficiency[which(cooling$ac_efficiency==17.2)]<-18
cooling$ac_efficiency[which(cooling$ac_efficiency==18.1)]<-19
cooling$ac_efficiency<-floor(cooling$ac_efficiency)
cooling$ac_efficiency[which(cooling$ac_efficiency<13)]<-NA



cooling$topstotal<-0
cooling$topstotal[which(cooling$top2020==1 & cooling$PAR_YEAR%in%c(2020,2021)==1)]<-1
cooling$topstotal[which(cooling$top2022==1 & cooling$PAR_YEAR==2022)]<-1
cooling$topstotal[which(cooling$top2023==1 & cooling$PAR_YEAR==2023)]<-1



library(INLA)
pc.prec_zib = list(prec = list(prior = "pc.prec", param = c(5, 0.01)))
pc.prec_bin = list(prec = list(prior = "pc.prec", param = c(.5, 0.01)))
pc.prec_gaus = list(prec = list(prior = "pc.prec", param = c(5, 0.01)))



cooling$ac_efficiency<-cooling$ac_efficiency-13
cooling$Contractor.s.Name.copy<-cooling$Contractor.s.Name


cooling$PAR_YEAR.copy<-cooling$PAR_YEAR
cooling$duct<-cooling$sow %>% tolower() %>% stringr::str_detect(.,"duct")
cooling$vent<-cooling$sow %>% tolower() %>% stringr::str_detect(.,"vent") 
cooling$replace<-cooling$sow %>% tolower() %>% stringr::str_detect(.,"replace") 
cooling$multiton<-cooling$Permit.. %in% c(cooling %>% group_by(Permit..) %>% tidytext::unnest_tokens(words,sow,token="words")  %>% filter(words=="ton") %>% count() %>% filter(n>1) %>% .$Permit..)
cooling$real_inspector[121]<-NA
cooling$failed_inspection[is.na(cooling$real_inspector)]<-NA
contcount<-xtabs(~Contractor.s.Name+PAR_YEAR,  data=permitted) %>% as.data.frame() %>% rename(contractortotal=Freq) %>% mutate(PAR_YEAR=as.numeric(as.character(PAR_YEAR)))
cooling<-left_join(cooling,contcount,na_matches="never")

#cooling<-cooling %>% select(-contractortotal)
cooling$contractortotal

cooling$ac_size[which(as.numeric(cooling$ac_size)>8)]<-NA
cooling$ac_size<-abs(as.numeric(cooling$ac_size))

cooling<-filter(cooling, geo==F)

mcool<-inla(ac_efficiency~1+
              scale(log(APPRAISE_1+1))+
              +scale(log(RES_ABOVE_))+
              scale(heatanomoly)+
              scale(log(sowschar+1))+
              f(Class, model="iid",hyper=pc.prec_zib)+
              furnace+
              multiton+
              heatpump.central+
              duct+
              vent+
              scale(log(contractortotal))+
              scale(MED_HH_INCOME)+
              scale(pred.bla)+
              scale(perc.over.65)+
              scale(pred.his)+
              as.factor(xcelpartner.hp.2024)+
              f(PAR_YEAR,model="iid",hyper=pc.prec_zib)+
              f(real_inspector,model="iid",hyper=pc.prec_gaus)+
              f(D_CLASS,model="iid",hyper=pc.prec_zib)+
              f(Contractor.s.Name,model="iid",hyper=pc.prec_zib)+
              f(RES_ORIG_YEAR_BUILT,model="rw1",hyper=pc.prec_zib)+
              f(tract,model="iid",hyper=pc.prec_zib)+
              f(week, model="rw1",cyclic=T,hyper=pc.prec_zib),
            data=filter(cooling,Valuation<=50000),
            family="nbinomial", 
control.predictor=list(link= 1))


mcost<-inla(log(c(Valuation+1))~1+heatpump.central+
              scale(log(APPRAISE_1+1))+
              +scale(log(RES_ABOVE_))+
              scale(heatanomoly)+
              heatpump.central+
              scale(log(sowschar+1))+
              scale(log(contractortotal))+
              scale(ac_efficiency)+
              scale(ac_size)+
              f(Class, model="iid",hyper=pc.prec_gaus)+
              furnace+
              multiton+
              duct+
              vent+
              scale(perc.over.65)+
              scale(pred.his)+
              scale(pred.bla)+
              scale(MED_HH_INCOME)+
              as.factor(xcelpartner.hp.2024)+
              f(PAR_YEAR,model="iid",hyper=pc.prec_gaus)+
              f(real_inspector,model="iid",hyper=pc.prec_gaus)+
              f(D_CLASS,model="iid",hyper=pc.prec_gaus)+
              f(Contractor.s.Name,model="iid",hyper=pc.prec_gaus)+
              f(tract,model="iid",hyper=pc.prec_gaus)+
              f(RES_ORIG_YEAR_BUILT,model="rw1",hyper=pc.prec_gaus)+
              f(week, model="rw1",cyclic=T,hyper=pc.prec_gaus),
            data=filter(cooling, Valuation<=50000),
            ,control.predictor=list(link= 1),
            family="gaussian")


msize<-inla(ac_size~1+heatpump.central+
              scale(log(APPRAISE_1+1))+
              +scale(log(RES_ABOVE_))+
              scale(heatanomoly)+
              heatpump.central+
              scale(log(sowschar+1))+
              scale(log(contractortotal))+
              f(Class, model="iid",hyper=pc.prec_gaus)+
              furnace+
              multiton+
              duct+
              vent+
              scale(perc.over.65)+
              scale(pred.his)+
              scale(pred.bla)+
              scale(MED_HH_INCOME)+
              as.factor(xcelpartner.hp.2024)+
              f(PAR_YEAR,model="iid",hyper=pc.prec_gaus)+
              f(real_inspector,model="iid",hyper=pc.prec_gaus)+
              f(D_CLASS,model="iid",hyper=pc.prec_gaus)+
              f(Contractor.s.Name,model="iid",hyper=pc.prec_gaus)+
              f(tract,model="iid",hyper=pc.prec_gaus)+
              f(RES_ORIG_YEAR_BUILT,model="rw1",hyper=pc.prec_gaus)+
              f(week, model="rw1",cyclic=T,hyper=pc.prec_gaus),
            data=filter(cooling, Valuation<=50000),
            ,control.predictor=list(link= 1),
            family="gaussian")

mfail<-inla(failed_inspection~heatpump.central+
              scale(log(APPRAISE_1+1))+
              +scale(log(RES_ABOVE_))+
              scale(heatanomoly)+
              scale(Valuation)+
              heatpump.central+
              scale(log(sowschar+1))+
              f(Class, model="iid",hyper=pc.prec_bin)+
              furnace+
              multiton+
              duct+
              vent+
              scale(log(contractortotal))+
              scale(perc.over.65)+
              scale(pred.his)+
              scale(pred.bla)+
              scale(MED_HH_INCOME)+
              as.factor(xcelpartner.hp.2024)+
              f(PAR_YEAR,model="iid",hyper=pc.prec_bin)+
              f(real_inspector,model="iid",hyper=pc.prec_bin)+
              f(D_CLASS,model="iid",hyper=pc.prec_bin)+
              f(Contractor.s.Name,model="iid",hyper=pc.prec_bin)+
              f(tract,model="iid",hyper=pc.prec_bin)+
              f(RES_ORIG_YEAR_BUILT,model="rw1",hyper=pc.prec_bin)+
              f(week, model="rw1",cyclic=T,hyper=pc.prec_bin),
            data=filter(cooling, Valuation<=50000),
            family="binomial",
            control.predictor=list(link= 1))


missingef<-inla(as.numeric(is.na(ac_efficiency))~heatpump.central+
              scale(log(APPRAISE_1+1))+
              +scale(log(RES_ABOVE_))+
              scale(heatanomoly)+
              scale(Valuation)+
              heatpump.central+
              scale(log(sowschar+1))+
              f(Class, model="iid",hyper=pc.prec_bin)+
              furnace+
              multiton+
              duct+
              vent+
              scale(perc.over.65)+
              scale(pred.his)+
              scale(pred.bla)+
              scale(MED_HH_INCOME)+
              as.factor(xcelpartner.hp.2024)+
              f(PAR_YEAR,model="iid",hyper=pc.prec_bin)+
              f(real_inspector,model="iid",hyper=pc.prec_bin)+
              f(D_CLASS,model="iid",hyper=pc.prec_bin)+
              f(Contractor.s.Name,model="iid",hyper=pc.prec_bin)+
              f(tract,model="iid",hyper=pc.prec_bin)+
              f(RES_ORIG_YEAR_BUILT,model="rw1",hyper=pc.prec_bin)+
              f(week, model="rw1",cyclic=T,hyper=pc.prec_bin),
            data=filter(cooling, Valuation<=50000),
            family="binomial",
            control.predictor=list(link= 1))
summary(missingef)


varrename_1<-function(X) case_match(X,"(Intercept)"~"Intercept",
                                    "scale(log(contractortotal))" ~ "demographics: contractor permit count, ln",
                                    "scale(Valuation)" ~ "permit:  valuation",
                                    "scale(pred.his)"~"demographics: Hispanic, pred.",
                                    "scale(pred.bla)"~"demographics: Black, pred.",
                                    "scale(perc.over.65)"~"demographics: Over 65, %",
                                    "scale(MED_HH_INCOME)"~"demographics: median hhi",
                                    "scale(log(sowschar + 1))"~ "permit: scope length (chars), ln + 1",
                                    "scale(log(RES_ABOVE_))"~"parcel: house area, ln sqft",
                                    "scale(log(APPRAISE_1 + 1))"~"parcel: appraisal value, ln $",
                                    "scale(heatanomoly)"~"parcel: heat island anomoly",
                                    "scale(ac_size)"~"permit: system size tons",
                                    "heatpump.central"~"permit: heat pump",
                                    "furnace"~"permit: furnace",
                                    "as.factor(xcelpartner.hp.2024)1"~"test: utility Q.I. partner",
                                    "scale(ac_efficiency)"~"permit: SEER",
                                    "venTRUE"~"permit: vent",
                                    "multitonTRUE"~"permit: 2x tons",
                                    "geoTRUE"~"permit: geothermal",
                                    "ductTRUE"~"permit: duct",
                                    "ventTRUE"~"permit: vent",.default=X)





failroc <- pROC::roc(filter(cooling, Valuation<=50000)$failed_inspection,mfail$summary.fitted.values$mean) 
failro2c <- pROC::roc(filter(cooling, Valuation<=50000)$failed_inspection,mfail.0$summary.fitted.values$mean) 


ggplot(mcool$summary.fixed %>% mutate(ID=rownames(.)))+geom_hline(aes(yintercept=0),lty=2)+geom_pointrange(aes(x=ID,y=mean,ymax=`0.975quant`,ymin=`0.025quant`))+coord_flip()+theme_minimal()



cooling$sowschar<-nchar(cooling$sow)


library(INLA)

ggplot()+ylab("fitted permit value")+xlab("ln actual permit value")+geom_hex(aes(x=log(c(filter(cooling,Valuation<=50000)$Valuation+1)),y=mcost$summary.fitted.values$mean,alpha=log(..count..)))+geom_abline(aes(intercept=0,slope=1))+scale_fill_viridis_c()+theme_minimal()




mcost$summary.random$D_CLASS %>% ggplot()+geom_pointrange(aes(x=mean,y=mean,ymax=`0.975quant`,ymin=`0.025quant`))+geom_hline(aes(yintercept=0))+coord_flip()+theme_minimal()


mcost$summary.random$week %>% ggplot()+geom_pointrange(aes(x=ID,y=mean,ymax=`0.975quant`,ymin=`0.025quant`))+geom_hline(aes(yintercept=0))+theme_minimal()

mcool$summary.random$week %>% ggplot()+geom_pointrange(aes(x=ID,y=mean,ymax=`0.975quant`,ymin=`0.025quant`))+geom_hline(aes(yintercept=0))+theme_minimal()

mfail$summary.random$week %>% ggplot()+geom_pointrange(aes(x=ID,y=mean,ymax=`0.975quant`,ymin=`0.025quant`))+geom_hline(aes(yintercept=0))+theme_minimal()


mcost$summary.random$PAR_YEAR %>% ggplot()+geom_pointrange(aes(x=ID,y=mean,ymax=`0.975quant`,ymin=`0.025quant`))+geom_hline(aes(yintercept=0))+theme_minimal()

mcost$summary.random$RES_ORIG_YEAR_BUILT %>% ggplot()+geom_pointrange(aes(x=ID,y=mean,ymax=`0.975quant`,ymin=`0.025quant`))+geom_hline(aes(yintercept=0))+theme_minimal()

mcool$summary.random$RES_ORIG_YEAR_BUILT %>% ggplot()+geom_pointrange(aes(x=ID,y=mean,ymax=`0.975quant`,ymin=`0.025quant`))+geom_hline(aes(yintercept=0))+theme_minimal()

mcool$summary.random$Contractor.s.Name %>% left_join(.,unique(cooling %>% select(Contractor.s.Name,xcelpartner.hp.2024)) %>% rename(ID=Contractor.s.Name)) %>% filter(`0.025quant`>0|`0.975quant`<0) %>% ggplot()+geom_pointrange(aes(x=ID,y=mean,ymax=`0.975quant`,ymin=`0.025quant`,colour=as.factor(xcelpartner.hp.2024)))+geom_hline(aes(yintercept=0))+coord_flip()+theme_minimal()+theme(legend.position="top")


mcool$summary.random$Contractor.s.Name %>% left_join(.,unique(cooling %>% select(Contractor.s.Name,xcelpartner.hp.2024)) %>% rename(ID=Contractor.s.Name)) %>% filter(`0.025quant`>0|`0.975quant`<0) %>% ggplot()+geom_pointrange(aes(x=rank(mean),y=mean,ymax=`0.975quant`,ymin=`0.025quant`,colour=as.factor(xcelpartner.hp.2024)))+geom_hline(aes(yintercept=0))+coord_flip()+theme_minimal()+theme(legend.position=c(.8,.2))+geom_text(aes(x=rank(mean),y=-4,label=ID),hjust="left",size=2.5)+ggthemes::scale_colour_few(name="QI Partner")
mcost$summary.random$Contractor.s.Name %>% left_join(.,unique(cooling %>% select(Contractor.s.Name,xcelpartner.hp.2024)) %>% rename(ID=Contractor.s.Name)) %>% filter(`0.025quant`>0|`0.975quant`<0) %>% ggplot()+geom_pointrange(aes(x=rank(mean),y=mean,ymax=`0.975quant`,ymin=`0.025quant`,colour=as.factor(xcelpartner.hp.2024)))+geom_hline(aes(yintercept=0))+coord_flip()+theme_minimal()+theme(legend.position=c(.9,.2))+geom_text(aes(x=rank(mean),y=-4,label=ID),hjust="left",size=2)+ggthemes::scale_colour_few(name="QI Partner")

mfail$summary.random$Contractor.s.Name %>% left_join(.,unique(cooling %>% select(Contractor.s.Name,xcelpartner.hp.2024)) %>% rename(ID=Contractor.s.Name)) %>% filter(`0.025quant`>0|`0.975quant`<0) %>% ggplot()+geom_pointrange(aes(x=rank(mean),y=mean,ymax=`0.975quant`,ymin=`0.025quant`,colour=as.factor(xcelpartner.hp.2024)))+geom_hline(aes(yintercept=0))+coord_flip()+theme_minimal()+theme(legend.position=c(.9,.2))+geom_text(aes(x=rank(mean),y=-4,label=ID),hjust="left",size=2)+ggthemes::scale_colour_few(name="QI Partner")


mcool$summary.random$PAR_YEAR %>% ggplot()+geom_pointrange(aes(x=ID,y=mean,ymax=`0.975quant`,ymin=`0.025quant`))+geom_hline(aes(yintercept=0))+theme_minimal()

mcool$summary.random$ZONE_ID %>% ggplot()+geom_pointrange(aes(x=ID,y=mean,ymax=`0.975quant`,ymin=`0.025quant`))+geom_hline(aes(yintercept=0))+theme_minimal()

mcool$summary.random$D_CLASS %>% ggplot()+geom_pointrange(aes(x=ID,y=mean,ymax=`0.975quant`,ymin=`0.025quant`))+geom_hline(aes(yintercept=0))+theme_minimal()

mcool$summary.random$week %>% ggplot()+geom_pointrange(aes(x=ID,y=mean,ymax=`0.975quant`,ymin=`0.025quant`))+geom_hline(aes(yintercept=0))+theme_minimal()

left_join(tractmap,mcool$summary.random$tract %>% rename(GEOID=ID)) %>% ggplot(.)+geom_sf(aes(fill=`0.5quant`))+scale_fill_viridis_c()

mcool$summary.random %>% ggplot()+geom_pointrange(aes(x=ID,y=mean,ymax=`0.975quant`,ymin=`0.025quant`))+geom_hline(aes(yintercept=0))+theme_minimal()

mcost$summary.random$inspector %>% ggplot()+geom_pointrange(aes(x=ID,y=mean,ymax=`0.975quant`,ymin=`0.025quant`))+geom_hline(aes(yintercept=0))+theme_minimal()+coord_flip()


table(cooling$PAR_YEAR)
cooling$ac_efficiency.inputed<-cooling$ac_efficiency
cooling$ac_efficiency.inputed[is.na(cooling$ac_efficiency)]<-mcool$summary.fitted.values$mean[is.na(cooling$ac_efficiency)]
summary(mcool)
library(sf)
tractmap<-tigris::tracts(state="CO",county="Denver",cb=T)

left_join(tractmap,mcool$summary.random$tract %>% mutate(GEOID=as.character(ID))) %>% ggplot(.)+geom_sf(aes(fill=mean))+scale_fill_viridis_c()+theme_minimal()

permitted<-left_join(permitted,cooling %>% as.data.frame() %>% select(obsid,ac_efficiency.inputed))
permitted$inspector.copy<-permitted$inspector
permitted$OWNER_LAST
permitted$RES_ABOVE_
permitted$continsp.int<-interaction(permitted$Contractor.s.Name,permitted$inspector)
permitted$continsp.int.copy<-permitted$continsp.int
permitted$RES_ORIG_YEAR_BUILT.copy<-permitted$RES_ORIG_YEAR_BUILT
permitted$CODE_YEAR.copy<-permitted$CODE_YEAR











fc<-fcfiles<-list.files("Documents/qt-local/fortcollins/fc_record_files/",full.names=T)
fcfiles<-lapply(fcfiles,read.csv)
fcfiles<-bind_rows(fcfiles)
fcfiles<-unique(fcfiles)
fcfiles<-filter(fcfiles,Record.Type%in%c("Residential Mechanical","Residential Electrical","Residential Water Heater","Residential Fireplace-Wood Burning Stove","Electric Service Change"))
fcfiles<-filter(fcfiles,paste0(Record..,".txt") %in% list.files("Documents/qt-local/fortcollins/permit_links/")==T)

prmpg<-list.files("Documents/qt-local/fortcollins/permit_pages/",full.names=T)
prmpg.out<-join(sapply(prmpg,function(X){
  read_html(X) %>% rvest::html_elements(xpath='//*[@id="ctl00_PlaceHolderMain_PermitDetailList1_updatePanel"]/div[1]') %>% html_children() %>% html_table() %>% .[[1]] %>% .$X1 %>% .[stringr::str_which(.,"Work Description")] }) %>% reshape2::melt(),sapply(prmpg,function(X){
    read_html(X) %>% rvest::html_elements(xpath='//*[@id="ctl00_PlaceHolderMain_PermitDetailList1_updatePanel"]/div[1]') %>% html_children() %>% html_table() %>% .[[1]] %>% .$X1 %>% .[stringr::str_which(.,"Applicant")]
  }) %>% reshape2::melt() %>% rename(value2=value))

head(prmpg.out)
prmpg.out<-cbind(prmpg.out,prmpg.out$value2 %>% stringr::str_match(.,"^Applicant: (?<applicant>\\w+ \\w+) (?<company>[\\w\\W]+?[0-9])") %>% as.data.frame() %>% mutate(company=stringr::str_squish(gsub("[0-9]$","",company))) %>% select(applicant, company))
prmpg.out$Record..<-basename(prmpg.out$L1) %>% gsub(".html","",.)
prmpg.out<-left_join(prmpg.out,fcfiles)
head(prmpg.out)
prmpg.out$value %>% stringr::str_which("HEAT PUMP")

tblocks<-tigris::block_groups(state="CO",county="031",year="2020")

left_join(tblocks,m3$summary.random$blockgroup1 %>% mutate(GEOID=as.character(ID))) %>% select(-mode) %>% ggplot(.)+geom_sf(aes(fill=mean))+scale_fill_viridis_c()+theme_minimal()


myroc <- pROC::roc(permitted$min_effic_ac,m3$summary.fitted.values$mean)
plot(myroc)




m3<-inla(heatpump.central~scale(log(APPRAISE_1+1))+
           scale(pred.bla)+
           scale(pred.his)+
           f(D_CLASS,model="iid",hyper=pc.prec_bin)+
           f(PAR_YEAR,model="iid",hyper=pc.prec_bin)+
           f(RES_ORIG_YEAR_BUILT,model="rw1",hyper=pc.prec_bin)+
           scale(RES_ABOVE_)*scale(Valuation)+
           shortermrental+
           f(Contractor.s.Name,model="iid",hyper=pc.prec_bin)+
           is.na(inspector)+
           f(week, model="rw1",cyclic=T),
         data=permitted,
         family="binomial", 
         control.predictor=list(link= 1))

m4<-inla(heatpump.mini~scale(log(APPRAISE_1+1))+
           scale(pred.bla)+
           scale(pred.his)+
           f(D_CLASS,model="iid",hyper=pc.prec_bin)+
           f(PAR_YEAR,model="iid",hyper=pc.prec_bin)+
           f(RES_ORIG_YEAR_BUILT,model="rw1",hyper=pc.prec_bin)+
           scale(RES_ABOVE_)*scale(Valuation)+
           shortermrental+
           heatanomoly+
           f(Contractor.s.Name,model="iid",hyper=pc.prec_bin)+
           is.na(inspector)+
           f(week, model="rw1",cyclic=T),
         data=permitted,
         family="binomial", 
         control.predictor=list(link= 1))

allparcels<-filter(allparcels,RES_ABOVE_>0)

points1<-allparcels %>% filter(mech.permit01==T) %>% select(SCHEDNUM,PAR_YEAR,geometry)
points2<-points1[-which(st_is_empty(points1)),]

domain <- inla.nonconvex.hull(as.matrix(st_coordinates(points2)),concave = -.07,convex = -0.05, resolution=c(100,100))

mesh0 <- inla.mesh.2d(loc = sf::st_coordinates(points2), max.edge=diff(range(st_coordinates(points2)[,1]))/(3*5),boundary=domain)

mesh0 <- inla.mesh.2d(max.edge=.015,boundary=domain)

library(inlabru)

ggplot() + geom_sf(data=points1)+gg(mesh0) 
























uhouse<-as.data.frame(permitted) %>% select(res_bin,appraise_bin,CODE_YEAR,tract) %>% unique() %>% mutate(hid=1:nrow(.))
permitted<-as.data.frame(permitted) %>% select(res_bin,appraise_bin,CODE_YEAR,tract,ac,furnace,heatpump.central,noncondense_furnace,min_effic_ac,Contractor.s.Name,inspector,PAR_YEAR) %>% left_join(.,uhouse)
permitted<-filter(permitted,is.na(inspector)==F,inspector!="NA")
nrow(permitted)
fulledge<-rbind(data.frame("from"=permitted$Contractor.s.Name,"to"=permitted$inspector), data.frame("from"=permitted$inspector,"to"=permitted$hid), data.frame("from"=permitted$hid,"to"=permitted$Contractor.s.Name))
fulledge<-na.omit(fulledge)
fulledge<-igraph::graph_from_data_frame(fulledge)
library(igraph)
E(fulledge)$weight <- 1
fulledge <- simplify(fulledge, edge.attr.comb=list(weight="sum"))
d3p<-fulledge %>% networkD3::igraph_to_networkD3() 

networkD3::forceNetwork(Links=d3p$links,Nodes=d3p$nodes,NodeID='name',Group=1)


rt<-esri2sf::esri2sf("https://services1.arcgis.com/zdB7qR0BtYrg0Xpl/ArcGIS/rest/services/Rebate_Census_Tracts/FeatureServer/0")



mfail$summary.fixed
library(ggplot2)

varrename_1<-function(X) case_match(X,"(Intercept)"~"Intercept",
           "scale(Valuation)" ~ "Permit Valuation",
           "scale(pred.his)"~"Hispanic, prediction",
           "scale(pred.bla)"~"Black, prediction",
           "scale(perc.over.65)"~"Over 65, % of blockgroup",
           "scale(MED_HH_INCOME)"~"HH Income, Median, of blockgroup",
           "scale(log(sowschar + 1))"~ "Scope length, ln + 1",
           "scale(log(RES_ABOVE_))"~"SQFT Home, ln",
           "scale(log(APPRAISE_1 + 1))"~"Appraisal value, ln",
           "scale(heatanomoly)"~"Heat island anomoly",
           "heatpump.central"~"Central heat pump in permit",
           "furnace"~"Furnace install in permit",
           "as.factor(xcelpartner.hp.2024)1"~"Utility Q.I. Partner",
           "scale(ac_efficiency)"~"Efficiency of ac in permit",default=X)


ggplot(mcool$summary.fixed %>% mutate(ID=varrename_1(rownames(.))) %>% filter(ID!="Intercept"))+geom_hline(aes(yintercept=0))+geom_pointrange(aes(x=ID,y=mean,ymax=`0.975quant`,ymin=`0.025quant`))+coord_flip()+theme_minimal()+ylab("ln permit valuation")+theme(text=element_text(size=20))+xlab("fixed effect")

ggplot(mcool$summary.fixed %>% mutate(ID=varrename_1(rownames(.))) %>% filter(ID!="Intercept"))+geom_hline(aes(yintercept=0))+geom_pointrange(aes(x=ID,y=mean,ymax=`0.975quant`,ymin=`0.025quant`),colour="dark blue")+coord_flip()+theme_minimal()+ylab("SEER Rating")+theme(text=element_text(size=20))+xlab("fixed effect")



rbind(mcool$summary.fixed %>% mutate(ID=varrename_1(rownames(.)),model="SEER"),
mcost$summary.fixed %>% mutate(ID=varrename_1(rownames(.)),model="Value"),
mfail$summary.fixed %>% mutate(ID=varrename_1(rownames(.)),model="Denial")) %>% filter(ID!="Intercept") %>% ggplot()+geom_hline(aes(yintercept=0))+geom_pointrange(aes(x=ID,y=mean,ymax=`0.975quant`,ymin=`0.025quant`))+facet_wrap(~model,scale="free_x")+coord_flip()+theme_minimal()+ylab("SEER Rating")+theme(text=element_text(size=20))+xlab("fixed effect")


mfail$marginals.fixed$`as.factor(xcelpartner.hp.2024)1` %>% as.data.frame() %>% ggplot()+geom_line(aes(x=exp(x),y=y))+geom_vline(aes(xintercept=1),lty=2)+theme_minimal()

mcool$marginals.fixed$`as.factor(xcelpartner.hp.2024)1` %>% as.data.frame() %>% ggplot()+geom_line(aes(x=exp(x),y=y))+geom_vline(aes(xintercept=1),lty=2)+theme_minimal()
mcost$summary.fixed$mean 
exp(.1)
mcost$marginals.fixed$`as.factor(xcelpartner.hp.2024)1` %>% as.data.frame() %>% ggplot()+geom_line(aes(x=exp(x),y=y))+geom_vline(aes(xintercept=0),lty=2)+theme_minimal()



for1<-httr::POST("https://hvacree.net/xcel-co/public_search_proc.cfm")
httr::content(for1) %>% View()
exp(-.3)
  




rbind(rbind(mcool$summary.fixed %>% mutate(ID=varrename_1(rownames(.)),model="SEER"),
      mcost$summary.fixed %>% mutate(ID=varrename_1(rownames(.)),model="Value"),
      mfail$summary.fixed %>% mutate(ID=varrename_1(rownames(.)),model="Denial"),
      msize$summary.fixed %>% mutate(ID=varrename_1(rownames(.)),model="Size")) %>% filter(ID=="test: utility Q.I. partner") %>% mutate(term="Q.I."),rbind(mcool$summary.random$Contractor.s.Name %>% mutate(model="SEER"),
      mcost$summary.random$Contractor.s.Name %>% mutate(model="Value"),
      mfail$summary.random$Contractor.s.Name %>% mutate(model="Denial"),msize$summary.random$Contractor.s.Name %>% mutate(model="Size"))  %>% mutate(term="company random")) %>% ggplot()+geom_hline(aes(yintercept=0))+geom_pointrange(aes(x=mean,y=mean,ymax=`0.975quant`,ymin=`0.025quant`,colour=term))+facet_grid(term~model,scale="free_x")+theme_bw()+ylab("posterior value")+theme(text=element_text(size=20))+xlab("fixed effect")+theme(legend.position="top")+ggthemes::scale_colour_colorblind()

exp(mcost$summary.fixed$mean[[1]]+mcost$summary.fixed$mean[[17]])-exp(mcost$summary.fixed$mean[[1]])
ggplot(mfail$summary.random$RES_ORIG_YEAR_BUILT)+geom_pointrange(aes(x=ID,y=mean,ymin=`0.025quant`,ymax=`0.975quant`))+theme_minimal()


cooling.sub<-filter(cooling, Contractor.s.Name%in%c("BLUE VALLEY ENERGY LLC","COLORADO ECO-MECHANICAL","EFFICIENT COMFORT","TRUE HEATING & COOLING LLC","ULTIMATE HEATING & COOLING INC","UNICOLORADO LLC"))
cooling.sub<-filter(cooling.sub,PAR_YEAR%in%c("2020","2022"))
lm(cooling.sub$Valuation~as.factor(cooling.sub$PAR_YEAR)) %>% summary()
lm(cooling.sub$failed_inspection~as.factor(cooling.sub$PAR_YEAR)) %>% summary()
lm(cooling.sub$ac_efficiency~as.factor(cooling.sub$PAR_YEAR)) %>% summary()
xtabs(~cooling$week+cooling$failed_inspection)





rbind(rbind(mcool$summary.fixed %>% mutate(ID=varrename_1(rownames(.)),model="SEER"),
            mcost$summary.fixed %>% mutate(ID=varrename_1(rownames(.)),model="Value"),
            mfail$summary.fixed %>% mutate(ID=varrename_1(rownames(.)),model="Denial")) %>% filter(ID=="test: utility Q.I. partner") %>% mutate(term="Q.I."), rbind(mcool$summary.random$real_inspector %>% mutate(model="SEER"),
                                                                                                                                                                   mcost$summary.random$real_inspector %>% mutate(model="Value"),
                                                                                                                                                                   mfail$summary.random$real_inspector %>% mutate(model="Denial"))  %>% mutate(term="company random")) %>% ggplot()+geom_hline(aes(yintercept=0))+geom_pointrange(aes(x=ID,y=mean,ymax=`0.975quant`,ymin=`0.025quant`,colour=term))+facet_grid(~model,scale="free_x")+theme_bw()+ylab("posterior value")+theme(text=element_text(size=20))+xlab("fixed effect")+theme(legend.position="top")+ggthemes::scale_colour_colorblind()+coord_flip()




mcool2<-inla(ac_efficiency~1+
              scale(log(APPRAISE_1+1))+
              +scale(log(RES_ABOVE_))+
              scale(heatanomoly)+
              scale(log(sowschar+1))+
              f(Class, model="iid",hyper=pc.prec_zib)+
              furnace+
              multiton+
              heatpump.central+
              duct+
              vent+
              scale(MED_HH_INCOME)+
              scale(pred.bla)+
              scale(pred.his)+
              as.factor(xcelpartner.hp.2024+topstotal)+
              f(PAR_YEAR,model="iid",hyper=pc.prec_zib)+
              f(real_inspector,model="iid",hyper=pc.prec_gaus)+
              f(D_CLASS,model="iid",hyper=pc.prec_zib)+
              f(Contractor.s.Name,model="iid",hyper=pc.prec_zib)+
              f(RES_ORIG_YEAR_BUILT,model="rw1",hyper=pc.prec_zib)+
              f(tract,model="iid",hyper=pc.prec_zib)+
              f(week, model="rw1",cyclic=T,hyper=pc.prec_zib),
            data=filter(cooling,Valuation<=50000),
            family="nbinomial", 
            control.predictor=list(link= 1))
summary(mcool2)


mcost2<-inla(log(c(Valuation+1))~1+heatpump.central+
               scale(log(APPRAISE_1+1))+
               +scale(log(RES_ABOVE_))+
               scale(heatanomoly)+
               heatpump.central+
               scale(log(sowschar+1))+
               scale(log(contractortotal))+
               scale(ac_efficiency)+
               scale(ac_size)+
               f(Class, model="iid",hyper=pc.prec_gaus)+
               furnace+
               multiton+
               duct+
               vent+
               scale(perc.over.65)+
               scale(pred.his)+
               scale(pred.bla)+
               scale(MED_HH_INCOME)+
               as.factor(xcelpartner.hp.2024+topstotal)+
               f(PAR_YEAR,model="iid",hyper=pc.prec_gaus)+
               f(real_inspector,model="iid",hyper=pc.prec_gaus)+
               f(D_CLASS,model="iid",hyper=pc.prec_gaus)+
               f(Contractor.s.Name,model="iid",hyper=pc.prec_gaus)+
               f(tract,model="iid",hyper=pc.prec_gaus)+
               f(RES_ORIG_YEAR_BUILT,model="rw1",hyper=pc.prec_gaus)+
               f(week, model="rw1",cyclic=T,hyper=pc.prec_gaus),
             data=filter(cooling, Valuation<=50000),
             ,control.predictor=list(link= 1),
             family="gaussian")
summary(mcost2)

mfail2<-inla(failed_inspection~heatpump.central+
              scale(log(APPRAISE_1+1))+
              +scale(log(RES_ABOVE_))+
              scale(heatanomoly)+
              scale(Valuation)+
              heatpump.central+
              scale(log(sowschar+1))+
              f(Class, model="iid",hyper=pc.prec_bin)+
              furnace+
              multiton+
              duct+
              vent+
              scale(perc.over.65)+
              scale(pred.his)+
              scale(pred.bla)+
              scale(MED_HH_INCOME)+
              as.factor(xcelpartner.hp.2024+topstotal)+
              f(PAR_YEAR,model="iid",hyper=pc.prec_bin)+
              f(real_inspector,model="iid",hyper=pc.prec_bin)+
              f(D_CLASS,model="iid",hyper=pc.prec_bin)+
              f(Contractor.s.Name,model="iid",hyper=pc.prec_bin)+
              f(tract,model="iid",hyper=pc.prec_bin)+
              f(RES_ORIG_YEAR_BUILT,model="rw1",hyper=pc.prec_bin)+
              f(week, model="rw1",cyclic=T,hyper=pc.prec_bin),
            data=filter(cooling, Valuation<=50000),
            family="binomial",
            control.predictor=list(link= 1))


summary(msize)
msize2<-inla(ac_size~1+heatpump.central+
              scale(log(APPRAISE_1+1))+
              +scale(log(RES_ABOVE_))+
              scale(heatanomoly)+
              heatpump.central+
              scale(log(sowschar+1))+
              scale(log(contractortotal))+
              f(Class, model="iid",hyper=pc.prec_gaus)+
              furnace+
              multiton+
              duct+
              vent+
              geo+
              scale(perc.over.65)+
              scale(pred.his)+
              scale(pred.bla)+
              scale(MED_HH_INCOME)+
              scale(ac_efficiency)+
              as.factor(xcelpartner.hp.2024+topstotal)+
              f(PAR_YEAR,model="iid",hyper=pc.prec_gaus)+
              f(real_inspector,model="iid",hyper=pc.prec_gaus)+
              f(D_CLASS,model="iid",hyper=pc.prec_gaus)+
              f(Contractor.s.Name,model="iid",hyper=pc.prec_gaus)+
              f(tract,model="iid",hyper=pc.prec_gaus)+
              f(RES_ORIG_YEAR_BUILT,model="rw1",hyper=pc.prec_gaus)+
              f(week, model="rw1",cyclic=T,hyper=pc.prec_gaus),
            data=filter(cooling, Valuation<=50000),
            ,control.predictor=list(link= 1),
            family="gaussian")


summary(msize2)
cooling$D_CLASS %>% table()
cooling$tract %>% table() %>% length()
which(filter(cooling, Valuation<=50000)$failed_inspection %>% is.na()==F) %>% length()

table(cooling$real_inspector) %>% length()
t11<-rbind(mcool2$summary.fixed %>% mutate(ID=rownames(.),model="SEER"),mcost2$summary.fixed%>% mutate(ID=rownames(.),model="Valuation"),mfail2$summary.fixed %>% mutate(ID=rownames(.),model="Denial"),msize2$summary.fixed%>% mutate(ID=rownames(.),model="Size")) %>% filter(.,stringr::str_detect(ID,"xcel")) 
t11$ID[stringr::str_which(t11$ID,"\\)2")]<-"top rebate earners" 
t11$ID[stringr::str_which(t11$ID,"1")]<-"other program partners" 

t11 %>% ggplot()+geom_hline(aes(yintercept=0),lty=2)+geom_pointrange(aes(x=ID,y=mean,ymin=`0.025quant`,ymax=`0.975quant`))+coord_flip()+facet_wrap(~model)+theme_bw()+xlab("program partners, compared to non-partner reference")+ylab("mean and 95% credible interval")
 


mfail2<-inla(failed_inspection~heatpump.central+
               scale(log(APPRAISE_1+1))+
               +scale(log(RES_ABOVE_))+
               scale(heatanomoly)+
               scale(Valuation)+
               heatpump.central+
               scale(log(sowschar+1))+
               f(Class, model="iid",hyper=pc.prec_bin)+
               furnace+
               multiton+
               duct+
               vent+
               scale(perc.over.65)+
               scale(pred.his)+
               scale(pred.bla)+
               scale(MED_HH_INCOME)+
               as.factor(xcelpartner.hp.2024+topstotal)+
               f(PAR_YEAR,model="iid",hyper=pc.prec_bin)+
               f(real_inspector,model="iid",hyper=pc.prec_bin)+
               f(D_CLASS,model="iid",hyper=pc.prec_bin)+
               f(Contractor.s.Name,model="iid",hyper=pc.prec_bin)+
               f(tract,model="iid",hyper=pc.prec_bin)+
               f(RES_ORIG_YEAR_BUILT,model="rw1",hyper=pc.prec_bin)+
               f(week, model="rw1",cyclic=T,hyper=pc.prec_bin),
             data=filter(cooling, Valuation<=50000),
             family="binomial",
             control.predictor=list(link= 1))


summary(msize)
msize3<-inla(ac_size~1+heatpump.central+
               scale(log(APPRAISE_1+1))+
               +scale(log(RES_ABOVE_))+
               scale(heatanomoly)+
               heatpump.central+
               scale(log(sowschar+1))+
               scale(log(contractortotal))+
               f(Class, model="iid",hyper=pc.prec_gaus)+
               furnace+
               multiton+
               duct+
               vent+
               geo+
               scale(perc.over.65)+
               scale(pred.his)+
               scale(pred.bla)+
               scale(MED_HH_INCOME)+
               as.factor(xcelpartner.hp.2024+topstotal)+
               f(PAR_YEAR,model="iid",hyper=pc.prec_gaus)+
               f(real_inspector,model="iid",hyper=pc.prec_gaus)+
               f(D_CLASS,model="iid",hyper=pc.prec_gaus)+
               f(Contractor.s.Name,model="iid",hyper=pc.prec_gaus)+
               f(tract,model="iid",hyper=pc.prec_gaus)+
               f(RES_ORIG_YEAR_BUILT,model="rw1",hyper=pc.prec_gaus)+
               f(week, model="rw1",cyclic=T,hyper=pc.prec_gaus),
             data=filter(cooling, Valuation<=50000,ac_efficiency==0),
             ,control.predictor=list(link= 1),
             family="gaussian")


mfail3<-inla(failed_inspection~heatpump.central+
               scale(log(APPRAISE_1+1))+
               +scale(log(RES_ABOVE_))+
               scale(heatanomoly)+
               scale(Valuation)+
               heatpump.central+
               scale(log(sowschar+1))+
               f(Class, model="iid",hyper=pc.prec_bin)+
               furnace+
               multiton+
               duct+
               vent+
               scale(perc.over.65)+
               scale(pred.his)+
               scale(pred.bla)+
               scale(MED_HH_INCOME)+
               as.factor(xcelpartner.hp.2024+topstotal)+
               f(PAR_YEAR,model="iid",hyper=pc.prec_bin)+
               f(real_inspector,model="iid",hyper=pc.prec_bin)+
               f(D_CLASS,model="iid",hyper=pc.prec_bin)+
               f(Contractor.s.Name,model="iid",hyper=pc.prec_bin)+
               f(tract,model="iid",hyper=pc.prec_bin)+
               f(RES_ORIG_YEAR_BUILT,model="rw1",hyper=pc.prec_bin)+
               f(week, model="rw1",cyclic=T,hyper=pc.prec_bin),
             data=filter(cooling, Valuation<=50000,ac_efficiency<=1),
             family="binomial",
             control.predictor=list(link= 1))

t12<-rbind(mfail3$summary.fixed %>% mutate(ID=rownames(.),model="Denial"),msize3$summary.fixed%>% mutate(ID=rownames(.),model="Size")) %>% filter(.,stringr::str_detect(ID,"xcel")) 
t12$ID[stringr::str_which(t12$ID,"\\)2")]<-"top rebate earners" 
t12$ID[stringr::str_which(t12$ID,"1")]<-"other program partners" 

t12 %>% ggplot()+geom_hline(aes(yintercept=0),lty=2)+geom_pointrange(aes(x=ID,y=mean,ymin=`0.025quant`,ymax=`0.975quant`))+coord_flip()+facet_wrap(~model)+theme_bw()+xlab("program partners, compared to non-partner reference")+ylab("mean and 95% credible interval")



cooling %>% saveRDS("coolingworking.rds")


rbind(mcool$summary.fixed %>% mutate(ID=varrename_1(rownames(.)),model="SEER"),
      mcost$summary.fixed %>% mutate(ID=varrename_1(rownames(.)),model="Value"),
      mfail$summary.fixed %>% mutate(ID=varrename_1(rownames(.)),model="Denial"),
      msize$summary.fixed %>% mutate(ID=varrename_1(rownames(.)),model="Size")) %>% write.csv("model_out_10.10.denver.fixed.csv")



rbind(mcool$summary.hyperpar %>% mutate(ID=rownames(.),model="SEER"),
      mcost$summary.hyperpar %>% mutate(ID=rownames(.),model="Value"),
      mfail$summary.hyperpar  %>% mutate(ID=rownames(.),model="Denial"),
      msize$summary.hyperpar  %>% mutate(ID=rownames(.),model="Size")) %>% write.csv("model_out_10.10.denver.random.csv")

      