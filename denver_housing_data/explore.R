




















sowsheat.basic<-filter(sows, 313%in%c(stat1,stat2,stat3))
sowsheat.basic$heatingpermit<-1

sowsheat.basic$noncondfurnace<-sowsheat.basic$furnace_efficiency=="80%"

allparcels2<-left_join(allparcels,sowsheat.basic %>% select(Permit..,heatingpermit,noncondfurnace),na_matches = c("never"))


head(allparcels2)









allparcels2 %>% saveRDS("allparcelsout37.rds")

allparcels2<-readRDS("allparcelsout37.rds")
head(allparcels2)
library(plyr)
library(dplyr)
ap1<-allparcels2 %>% select(SCHEDNUM, SALE_YEAR, APPRAISE_1,ZONE_10,PAR_YEAR, SCHEDNUMCHAR,RES_ORIG_YEAR_BUILT,RES_ABOVE_GRADE_AREA,Permit..,OWNER_LAST, OWNER_FIRST,GEOID20,hisprate,whiterate,mech.permit01,yb.decade,res_bin,appraise_bin,CODE_YEAR,blockgroup,tract,MED_HH_INCOME,BACHELORS_OR_HIGHER_EDU,heatanomoly,shortermrental,heatingpermit,noncondfurnace)








p23<-filter(allparcels, PAR_YEAR==2023, Class!="New Building")
p23<-p23 %>% as.data.frame() %>% select(mech.permit01,RES_ABOVE_GRADE_AREA,APPRAISE_1,MED_HH_INCOME,perc.less.5,perc.hisp,perc.over.65,prob_hispanic,perc.bach,heatanomoly,D_CLASS_CN,CODE_YEAR,subdivision,blockgroup,shortermrental) %>% unique()
p23<-p23 %>% filter(is.na(D_CLASS_CN)==F)
pc.prec1 = list(prec = list(prior = "pc.prec", param = c(5, 0.01)))

sowsheat<-filter(sows, 313%in%c(stat1,stat2,stat3),PAR_YEAR==2023)
sowsheat<-sowsheat %>% left_join(.,p23)
head(sowsheat)
sowsheat<-filter(sowsheat, is.na(filename)==F)
test1<-inla(as.numeric(mech.permit01)~1+
              #fixed portion
              scale(as.numeric(RES_ABOVE_GRADE_AREA))+
              scale(APPRAISE_1)+
              scale(MED_HH_INCOME)+
              scale(perc.less.5)+
              scale(perc.hisp)+
              scale(perc.over.65)+
              scale(prob_hispanic)+
              scale(perc.bach)+
              scale(heatanomoly)+
              shortermrental+
              #random portion
              f(D_CLASS_CN,model="iid",hyper=pc.prec1)+
              f(CODE_YEAR,model="iid",hyper=pc.prec1)+
              f(blockgroup,model="iid",hyper=pc.prec1)
            #  f(subdivision,model="iid")+
            ,
            #modelling choices
            family="binomial",
            data=as.data.frame(p23), 
            control.predictor = list(link = 1),
            control.compute=list(config = TRUE))

sowsheat$noncondfurnace<-as.numeric(sowsheat$noncondfurnace)
sowsheat$CODE_YEAR.copy<-sowsheat$CODE_YEAR
sowsheat$noncondfurnace<-sowsheat$furnace_efficiency=="80%"

test2<-inla(noncondfurnace~1+
              #fixed portion
              scale(as.numeric(RES_ABOVE_GRADE_AREA))+
              scale(log(APPRAISE_1+1))+
              scale(MED_HH_INCOME)+
              scale(perc.less.5)+
              scale(perc.hisp)+
              scale(perc.over.65)+
              scale(prob_hispanic)+
              scale(perc.bach)+
              scale(heatanomoly)+
              shortermrental+
              #random portion
              f(D_CLASS_CN,model="iid",hyper=pc.prec1)+
              f(yb.decade,model="rw1",hyper=pc.prec1)+
              f(blockgroup,model="iid",hyper=pc.prec1)+
              f(Contractor.s.Name,model="iid")
            ,
            #modelling choices
            family="binomial",
            data=as.data.frame(sowsheat), 
            control.predictor = list(link = 1),
            control.compute=list(config = TRUE))

test2$summary.random$yb.decade %>% ggplot()+geom_pointrange(aes(x=ID,y=mean,ymin=`0.025quant`,ymax=`0.975quant`))

test2$summary.random$Contractor.s.Name %>% filter(`0.025quant`>0|`0.975quant`<0) %>% ggplot()+geom_pointrange(aes(x=ID,y=mean,ymin=`0.025quant`,ymax=`0.975quant`))+coord_flip()

test2$summary.random$blockgroup %>% ggplot()+geom_pointrange(aes(x=ID,y=mean,ymin=`0.025quant`,ymax=`0.975quant`))

test2$summary.random$D_CLASS_CN %>% ggplot()+geom_pointrange(aes(x=ID,y=mean,ymin=`0.025quant`,ymax=`0.975quant`))+coord_flip()

test2$summary.fixed %>% mutate(ID=rownames(.)) %>% filter(stringr::str_detect(ID,"Intercept")==F) %>% ggplot()+geom_pointrange(aes(x=ID,y=mean,ymin=`0.025quant`,ymax=`0.975quant`))+coord_flip()+geom_hline(yintercept=0,lty=2)+theme_minimal()

sowsheat$Contractor.s.Name.copy<-sowsheat$Contractor.s.Name
sowsheat$month<-sowsheat$Date.Issued %>% lubridate::mdy() %>% lubridate::month()
sowsheat$week<-sowsheat$Date.Issued %>% lubridate::mdy() %>% lubridate::week()

costmodel<-inla(Valuation~noncondfurnace+ac+scale(as.numeric(RES_ABOVE_))+heatpump.central+heatpump.mini+f(month,model="rw1")+f(Contractor.s.Name,model="iid"),data=sowsheat,family="gaussian")

sowsheat %>% head()
test3<-inla(noncondfurnace~1+
              #fixed portion
              scale(as.numeric(RES_ABOVE_GRADE_AREA))+
              scale(log(APPRAISE_1+1))+
              scale(MED_HH_INCOME)+
              scale(perc.less.5)+
              scale(perc.hisp)+
              scale(prob_hispanic)+
              scale(perc.over.65)+
              scale(perc.bach)+
              scale(heatanomoly)+
              shortermrental+
              ac+
              f(week,model="rw2",cyclic=T)+
              #random portion
              f(D_CLASS_CN,model="iid",hyper=pc.prec1)+
              f(yb.decade,model="rw1",hyper=pc.prec1)+
              f(blockgroup,model="iid",hyper=pc.prec1)+
              f(Contractor.s.Name,model="iid")+
              f(SCHEDNUMCHAR,model="iid")
            ,
            #modelling choices
            family="binomial",
            data=as.data.frame(sowsheat), 
            control.predictor = list(link = 1,compute = TRUE),
            control.compute=list(config = TRUE))


sowsheat %>% select(Permit..,prob_hispanic,noncondfurnace,RES_ABOVE_GRADE_AREA,APPRAISE_1,MED_HH_INCOME,perc.less.5,perc.hisp,perc.over.65,perc.bach,heatanomoly,shortermrental,ac,week,D_CLASS_CN,yb.decade,blockgroup,Contractor.s.Name,SCHEDNUMCHAR,sow,inspector,GEOID20) %>% write.csv("scopes_of_work.csv")



test3$summary.fixed %>% mutate(ID=rownames(.)) %>% filter(stringr::str_detect(ID,"Intercept")==F) %>% ggplot()+geom_pointrange(aes(x=ID,y=mean,ymin=`0.025quant`,ymax=`0.975quant`))+coord_flip()+geom_hline(yintercept=0,lty=2)+theme_minimal()


library(ggplot2)
test3$summary.random$Contractor.s.Name %>% filter(`0.025quant`>0|`0.975quant`<0) %>% ggplot()+geom_pointrange(aes(x=ID,y=mean,ymin=`0.025quant`,ymax=`0.975quant`))+coord_flip()

test3$summary.random$week %>% ggplot()+geom_pointrange(aes(x=ID,y=mean,ymin=`0.025quant`,ymax=`0.975quant`))+xlab("week")+theme_minimal()



summary(test3)

test3$summary.random$blockgroup %>% ggplot()+geom_pointrange(aes(x=ID,y=mean,ymin=`0.025quant`,ymax=`0.975quant`))



test1$summary.fixed %>% mutate(ID=rownames(.)) %>% filter(stringr::str_detect(ID,"Intercept")==F) %>% ggplot()+geom_pointrange(aes(x=ID,y=mean,ymin=`0.025quant`,ymax=`0.975quant`))+coord_flip()+geom_hline(yintercept=0,lty=2)+theme_minimal()

mcomp<-inla.make.lincombs(month=1:12)




parfiles<-list.files("Documents/qt-local/denmet/Denver/parcel_details/",full.names=T)
length(parfiles)
pardets<-parfiles %>% lapply(function(D) {
  d1<-readRDS(D) 
  if(length(d1)>1){
    d1<-d1 %>% .[[2]] 
    data.frame("style"=d1$X2[1],"year_effective"=d1$X2[3],"rooms"=d1$X2[2],"baths"=d1$X4[2])} else data.frame("style"=NA,"year_effective"=NA,"rooms"=NA,"baths"=NA)}) %>% bind_rows()


pardets$SCHEDNUMCHAR<-basename(parfiles) %>% gsub(".rds","",.)
pardets$style %>% table()

table(allparcels$PROP_CLASS)

sows<-left_join(sows,pardets)



ggplot()+geom_bar(aes(x=as.factor(sows$CODE_YEAR)))+theme_minimal()+ggtitle("Building Code Vintage of Home (original)")+xlab("published year")


m1<-inla(as.numeric(heatpump.central)~f(Contractor.s.Name,model="iid")+f(as.factor(CODE_YEAR),model="iid")+scale(APPRAISE_1)+as.factor(PAR_YEAR)+scale(RES_ABOVE_)+zonesimple+scale(prob_hispanic),data=sows)
summary(m1)

m1$summary.random$Contractor.s.Name %>% filter(`0.025quant`>0) %>% ggplot()+geom_linerange(aes(x=ID,y=mean,ymin=`0.025quant`,ymax=`0.975quant`))+coord_flip()

m1$summary.random$Contractor.s.Name %>% filter(`0.975quant`<0) %>% ggplot()+geom_linerange(aes(x=ID,y=mean,ymin=`0.025quant`,ymax=`0.975quant`))+coord_flip()

ls()
rm(list=ls()[-c(1,28,9)])

uhouse<-sows %>% select(likelihisp,res_bin,yb.decade,appraise_bin) %>% na.omit() %>% unique() %>% mutate(huid=paste0("h",1:nrow(.))) 
sows<-sows %>% left_join(.,uhouse)
cnet<-sows %>% select(huid,Contractor.s.Name,heatpump.central,ac) %>% filter(is.na(huid)==F)
cnet2<-cnet %>% select(huid,Contractor.s.Name,heatpump.central) %>% count()
cnet2<-rbind.fill(cnet %>% select(huid,Contractor.s.Name,ac) %>% count(),cnet2)
cnet2<-filter(cnet2,ac|heatpump.central==T)
cnet2$heatpump.central<-ifelse(is.na(cnet2$heatpump.central),0,1)
cnet2<-cnet2 %>% select(-ac)

cnet2<-filter(cnet2,freq>3)
cnetg<-cnet2 %>% igraph::graph_from_data_frame(directed=F)
igraph::E(cnetg)$weight<-cnet2$freq
library(igraph)
wt <- cluster_leiden(cnetg)
members <- membership(wt)
cnetd3<-networkD3::igraph_to_networkD3(cnetg,group=members)
cnetd3$links$weight<-E(cnetg)$weight
cnetd3$nodes$group<-"black"
cnetd3$links$group1<-ifelse(igraph::edge.attributes(cnetg)$heatpump.central==0,"orange","green")
networkD3::forceNetwork(Links=cnetd3$links,Nodes=cnetd3$nodes,Value="weight",Source="source",Target="target",Group="group",NodeID="name")
my_color <- 'd3.scaleOrdinal() .domain(["black","orange","green"]) .range(["black","orange", "green"])'

networkD3::sankeyNetwork(Links=cnetd3$links,Nodes=cnetd3$nodes,Value="weight",Source="source",Target="target",NodeID="name",LinkGroup="group1",NodeGroup="group", colourScale=my_color) 


unique(permit1$Contractor.s.Name) %>% sort() %>% write.csv("contractordenver.csv")
uhouse<-sows %>% select(res_bin,yb.decade,appraise_bin) %>% na.omit() %>% count() %>% mutate(huid=paste0("h",1:nrow(.))) 

head(uhouse)
ggplot(uhouse,aes(x=res_bin,y=appraise_bin))+geom_tile(aes(fill=log(freq)))+facet_wrap(~yb.decade)+geom_label(aes(label=huid))+theme_minimal()
whead(uhouse)
#sows<-sows %>% left_join(.,uhouse)

cnet<-sows %>% select(huid,Contractor.s.Name) %>% filter(is.na(huid)==F)
cnet<-cnet %>% dplyr::count(huid,Contractor.s.Name) 
cnet<-filter(cnet, n>1)
cnetg<-cnet %>% igraph::graph_from_data_frame(directed=F)
igraph::E(cnetg)$weight<-cnet$n
filter(sows,stringr::str_detect(Permit..,"0009567"))
library(networkD3)



ggplot(filter(sows,ac_size<10,PAR_YEAR%in%c(2020,2021,2022,2023)))+geom_density2d(aes(x=ac_size,y=as.numeric(ac_efficiency),colour=as.factor(PAR_YEAR),group=as.factor(PAR_YEAR)))+theme_minimal()+facet_wrap(~PAR_YEAR)


ggplot(filter(sows,ac_size<10,PAR_YEAR%in%c(2020,2021,2022,2023)))+geom_density2d(aes(x=Valuation,y=as.numeric(ac_efficiency),colour=PAR_YEAR,group=PAR_YEAR,alpha=PAR_YEAR))+theme_minimal()+xlab("Permit Value")+ylab("AC Efficiency")

ggplot(filter(sows,ac_size<10,PAR_YEAR%in%c(2020,2021,2022,2023),Valuation<75000))+geom_smooth(aes(x=Valuation,y=as.numeric(ac_efficiency),colour=PAR_YEAR,group=PAR_YEAR))+theme_minimal()+xlab("Permit Value (includes permits <$75000)")+ylab("AC Efficiency")+scale_colour_continuous(name="Permit Year")


ggplot(filter(sows,ac_size<10,PAR_YEAR%in%c(2020,2021,2022,2023)))+geom_point(aes(x=Valuation,y=as.numeric(ac_efficiency)),size=.1,position="dodge")+theme_minimal()+xlab("Permit Value")+ylab("AC Efficiency")+scale_colour_viridis_c()


dplyr::filter(sows, heatpump.central==T) %>% ggplot(.)+geom_histogram(aes(x=as.numeric(RES_ORIG_YEAR_BUILT)))+geom_density(aes(x=as.numeric(RES_ORIG_YEAR_BUILT)),colour="red")+theme_minimal()


dplyr::filter(sows, heatpump.central==T) %>% ggplot(.)+geom_histogram(aes(x=Valuation))+geom_density(aes(x=as.numeric(RES_ORIG_YEAR_BUILT)),colour="red")+theme_minimal()

sows %>% head()

permit.details<-readRDS("Documents/qt-local/denmet/Denver/permitdetails.rds")
permit.details$Permit..<-permit.details$filename %>% basename() %>% gsub(".html","",.)
sows<-left_join(sows,permit.details)
sows$stat1 %>% table()
sows$stat2 %>% table()
sows$stat3 %>% table()

ggplot(sows %>% filter(stat1%in%c(310,313),is.na(PAR_YEAR)==F,PAR_YEAR==2020) %>% st_sf)+geom_sf(aes(colour=inspector),size=.5)+theme_minimal()+ggthemes::scale_color_tableau(palette="Tableau 10")




dplyr::filter(sows, heatpump.central==T) %>% ggplot(.)+geom_histogram(aes(x=Valuation))

ggplot(dplyr::filter(sows, furnace==T))+geom_histogram(aes(x=as.numeric(RES_ABOVE_GRADE_AREA)))+geom_histogram(data=dplyr::filter(sows, heatpump.central==T),aes(x=as.numeric(RES_ABOVE_GRADE_AREA)),colour="red")



ggplot(dplyr::filter(sows, ac==T))+geom_histogram(aes(x=as.numeric(Valuation)))+geom_histogram(data=dplyr::filter(sows, heatpump.central==T),aes(x=as.numeric(Valuation)),colour="red")


ggplot(dplyr::filter(sows, ac==T,as.numeric(RES_ABOVE_)<5000,Valuation>1),aes(x=log(as.numeric(Valuation)),y=as.numeric(RES_ABOVE_)))+
  geom_point()+
  geom_point(data=dplyr::filter(sows, heatpump.central==T,as.numeric(RES_ABOVE_)<5000),colour="red")+
  geom_vline(aes(xintercept=log(mean(as.numeric(Valuation)))),colour="black")+
  geom_vline(data=dplyr::filter(sows, heatpump.central==T,as.numeric(RES_ABOVE_)<5000),aes(xintercept=log(mean(as.numeric(Valuation)))),colour="red")+
  theme_minimal()+ylab("home size")+xlab("ln $ value of permit")+geom_text(aes(x=5,y=1000,label=paste0("delta=",round(dplyr::filter(sows, heatpump.central==T,as.numeric(RES_ABOVE_)<5000)$Valuation %>% mean()-dplyr::filter(sows, ac==T,as.numeric(RES_ABOVE_)<5000)$Valuation %>% mean()))))


ggplot(dplyr::filter(sows,as.numeric(RES_ABOVE_)<5000,Valuation>1, heatpump.central|ac==T),aes(y=log(as.numeric(Valuation)),x=as.numeric(RES_ABOVE_)))+geom_point(aes(colour=heatpump.central))+facet_wrap(~PAR_YEAR)+geom_smooth(aes(colour=heatpump.central),method="lm")


ggplot(dplyr::filter(sows, ac==T,as.numeric(RES_ABOVE_)<5000,Valuation>1),aes(x=log(as.numeric(Valuation)),y=as.numeric(RES_ABOVE_)))+
  geom_point()+
  geom_point(data=dplyr::filter(sows, heatpump.central==T,as.numeric(RES_ABOVE_)<5000),colour="red")+
  geom_vline(aes(xintercept=log(mean(as.numeric(Valuation)))),colour="black")+
  geom_vline(data=dplyr::filter(sows, heatpump.central==T,as.numeric(RES_ABOVE_)<5000),aes(xintercept=log(mean(as.numeric(Valuation)))),colour="red")+
  theme_minimal()+ylab("home size")+xlab("ln $ value of permit")+facet_wrap(~PAR_YEAR,ncol=1)

filter(sows, heatpump.central==T,ac==T)$sow
filter(sows, heatpump.central==F,ac==F,furnace==F)$sow2
filter(sows, heatpump.central==T,ac==T,furnace==T)$sow2
filter(sows, heatpump.central==T,ac==F,furnace==T)$sow2




xtabs(~heatpump.central+furnace+ac,data=sows %>% mutate(furnace=ifelse(furnace==T,"Furnace","No Furnace"),ac=ifelse(ac==T,"AC","No AC"),heatpump.central=ifelse(heatpump.central==T,"heat pump","no heat pump"))) %>% as.data.frame() %>% ggplot(.)+geom_tile(aes(x=ac,y=heatpump.central,fill=Freq))+geom_label(aes(x=ac,y=heatpump.central,label=Freq),size=6)+facet_grid(~furnace)+scale_fill_viridis_c()+theme(legend.position="top",text=element_text(size=15))+theme_minimal()

xtabs(~heatpump.mini+furnace+ac,data=sows %>% mutate(furnace=ifelse(furnace==T,"Furnace","No Furnace"),ac=ifelse(ac==T,"AC","No AC"),heatpump.mini=ifelse(heatpump.mini==T,"heat pump mini","no heat pump mini"))) %>% as.data.frame() %>% ggplot(.)+geom_tile(aes(x=ac,y=heatpump.mini,fill=Freq))+geom_label(aes(x=ac,y=heatpump.mini,label=Freq))+facet_grid(~furnace)+scale_fill_viridis_c()+theme(legend.position="top")+theme_minimal()




xtabs(~stringr::str_detect(sows$sow,"replac")+sows$heatpump.mini)


sows %>% filter(furnace==T) %>% ggplot(.)+geom_bar(aes(x=furnace_efficiency,fill=heatpump.central),stat="count")
sows %>% mutate(furnace=ifelse(furnace==T,"Furnace","No Furnace"),ac=ifelse(ac==T,"AC","No AC"),heatpump.central=ifelse(heatpump.central==T,"heat pump","no heat pump"),hpac=interaction(heatpump.central,ac)) %>% filter(furnace=="Furnace") %>% ggplot(.)+geom_bar(aes(x=furnace_efficiency,colour=hpac,fill=hpac),stat="count")+theme_minimal()+coord_flip()+facet_wrap(~PAR_YEAR)+theme(legend.position=c(.8,.2))



dplyr::filter(sows, heatpump.central==T) %>% nrow()/dplyr::filter(sows, ac==T) 



ggplot(dplyr::filter(sows, ac==T))+geom_histogram(aes(x=as.numeric(RES_ORIG_YEAR_BUILT)))+geom_histogram(data=dplyr::filter(sows, heatpump.central==T),aes(x=as.numeric(RES_ORIG_YEAR_BUILT)),colour="red")+theme_minimal()+xlab("year home built")


ggplot(dplyr::filter(sows, ac==T))+geom_bar(aes(x=stringr::str_extract(ZONE_ID,"^[USE]")))+geom_bar(data=dplyr::filter(sows, heatpump.central==T),aes(x=stringr::str_extract(ZONE_ID,"^[USE]")),colour="red")+theme_minimal()+coord_flip()+xlab("neighborhood context")




allparcels2<-xtabs(~sows$heatpump.central+sows$GEOID20) %>% as.data.frame()
colnames(allparcels2)<-c("heatpump.central","GEOID20","hpfreq")
allparcels2<-filter(allparcels2,heatpump.central==T)
allparcels2<-left_join(ethdata, allparcels2)
allparcels2$hprate<-ifelse(is.na(allparcels2$hpfreq),0,allparcels2$hpfreq)/allparcels2$P0040001


ggplot(allparcels2 %>% filter(P0040001>0))+geom_sf(aes(fill=hprate,colour=hprate))+scale_fill_viridis_c()+scale_colour_viridis_c()





sows$tract<-sows$GEOID20 %>% gsub("08031","",.) %>% substr(.,1,6)









allparcels2<-xtabs(~sows$heatpump.central+sows$GEOID20) %>% as.data.frame()
allparcels3<-xtabs(~ac+GEOID20,data=sows) %>% as.data.frame()
colnames(allparcels3)[3]<-"acfreq"
allparcels3<-filter(allparcels3,ac==T) %>% select(ac,GEOID20,acfreq)
colnames(allparcels2)<-c("heatpump.central","GEOID20","hpfreq")
allparcels2<-filter(allparcels2,heatpump.central==T)
racedata<-sf::read_sf("Documents/qt-local/denmet/voter_blocks/co_pl2020_b/co_pl2020_p3_b.shp")
ethdata<-sf::read_sf("Documents/qt-local/denmet/voter_blocks/co_pl2020_b/co_pl2020_p4_b.shp")
ethdata<-ethdata %>% select(GEOID20,P0040001,P0040002,COUNTY) %>% mutate("hisprate"=P0040002/P0040001) %>% filter(COUNTY=="031")
racedata<-racedata %>% select(GEOID20,P0030001,P0030003,COUNTY) %>% mutate("whiterate"=P0030003/P0030001) %>% filter(COUNTY=="031")
ethdata<-ethdata %>% left_join(.,racedata %>% as.data.frame() %>% select(GEOID20,whiterate,P0030001,P0030003))
allparcels2<-left_join(ethdata, allparcels2)
allparcels2<-left_join(allparcels2,allparcels3)
allparcels2$hpfreq<-ifelse(is.na(allparcels2$hpfreq),0,allparcels2$hpfreq)
allparcels2$acfreq<-ifelse(is.na(allparcels2$acfreq),0,allparcels2$acfreq)

allparcels2$hprate<-ifelse(is.na(allparcels2$hpfreq),0,allparcels2$hpfreq)/allparcels2$P0040001
head(count(sows$GEOID20))
allparcels2<-left_join(allparcels2,count(sows$GEOID20) %>% dplyr::rename(GEOID20=x))
#allparcels2$hprate<-ifelse(is.na(allparcels2$hpfreq),0,allparcels2$hpfreq)/allparcels2$P0040001

ggplot(allparcels2 %>% filter(P0040001>0))+geom_sf(aes(fill=hprate,colour=hprate))+scale_fill_viridis_c()+scale_colour_viridis_c()+theme_minimal()
allparcels2$hpfreq


ggplot(allparcels2 %>% filter(P0040001>0))+geom_sf(aes(fill=hpfreq/acfreq,colour=hpfreq/acfreq))+scale_fill_viridis_c()+scale_colour_viridis_c()+theme_minimal()

ggplot(allparcels2 %>% filter(P0040001>0))+geom_sf(aes(fill=freq,colour=freq))+scale_fill_viridis_c()+scale_colour_viridis_c()+theme_minimal()

permtab<-pwb %>% select(GEOID20,P0040001) %>% count()
colnames(permtab)[3]<-"cooling.heating"
allparcels2<-left_join(allparcels2,permtab)
allparcels2<-allparcels2 %>% filter(P0030001>0) %>% filter(cooling.heating<100)


library(basemaps)
library(ggspatial)
library(rosm)


ggplot(allparcels2)+annotation_map_tile(zoom=11,type="cartolight")+geom_sf(aes(alpha=log(cooling.heating+1),alpha=log(cooling.heating+1)),fill="dark blue",colour=NA)+scale_alpha_continuous(name="heating/cooling permits 2020-23, ln")+ggthemes::theme_map()+theme(legend.position=c(.7,.4))

tc<-tigris::tracts(state="CO",county="Denver")


left_join(tc,sows %>% select(tract,heatpump.central,PAR_YEAR) %>% count() %>% filter(heatpump.central==T) %>% dplyr::rename(TRACTCE=tract)) %>% left_join(sows %>% select(tract,ac,PAR_YEAR) %>% count() %>% filter(ac==T) %>% dplyr::rename(TRACTCE=tract,acfreq=freq)) %>% na.omit() %>% ggplot(.)+annotation_map_tile(type="cartolight",zoom=11)+geom_sf(aes(alpha=freq/c(acfreq+1)),fill="dark green")+theme_minimal()+facet_wrap(~PAR_YEAR)+ggthemes::theme_map()+theme(legend.position="top")




hp.rebate<-read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vTTSZTvrrQz0Bh_1ucW6cRwUlUitln-YGlDeh1yniU6wu0uTKKlc6nNQRWuWTJnH9KNmvhhWvi0O9C-/pub?gid=1248401373&single=true&output=csv")
hp.rebate<-hp.rebate %>% dplyr::rename(Contractor.s.Name=Contractor) %>% mutate(Contractor.s.Name=substr(Contractor.s.Name,1,10)) 
hp.rebate<-unique(hp.rebate)
hp.rebate<-sows %>% select(Contractor.s.Name,PAR_YEAR,heatpump.central) %>% mutate(Contractor.s.Name=substr(Contractor.s.Name,1,10)) %>% count() %>% filter(heatpump.central==T) %>% filter(PAR_YEAR==2023) %>% left_join(hp.rebate)
hp.rebate$HeatPumpRebate23[is.na(hp.rebate$HeatPumpRebate23)]<-0
hp.rebate$rebaterate<-hp.rebate$HeatPumpRebate23/hp.rebate$freq
hp.rebate[hp.rebate$rebaterate>1,]
ggplot(hp.rebate %>% filter(rebaterate>0))+geom_bar(aes(x=Contractor.s.Name,y=rebaterate),stat="identity")+coord_flip()+theme_minimal()+ylab("rebated heat pumps/installed heatpumps")

rebatejoined<-left_join(filter(sows,PAR_YEAR==2023) %>% mutate(Contractor.s.Name=substr(Contractor.s.Name,1,10)),hp.rebate %>% select(Contractor.s.Name,rebaterate))
rebatejoined$rebaterate<-ifelse(rebatejoined$rebaterate>1,1,rebatejoined$rebaterate)
ggplot(rebatejoined %>% filter(heatpump.central==T) %>% st_sf)+geom_sf(aes(colour=as.numeric(heatpump.central)*rebaterate))+theme_minimal()+scale_colour_viridis_c("proportion of company\n installations\n with rebate")

rebatejoined %>% filter(heatpump.central==T) %>% head()


sows %>% write.csv("sowsexample")

allparcels2<-left_join(allparcels,sows %>% select(heatpump.central,SCHEDNUMCHAR,PAR_YEAR,Valuation,Contractor.s.Name))
head(allparcels2)                                                             
allparcels2$heatpump.central[is.na(allparcels2$heatpump.central)]<-FALSE
allparcels2$heatpump.central<-allparcels2$heatpump.central %>% as.numeric()
head(allparcels2)
lm(heatpump.central~prob_hispanic+RES_ABOVE_+APPRAISE_1+RES_ORIG_YEAR_BUILT+as.factor(D_CLASS),data=allparcels2) %>% summary()
library(INLA)
try1<-inla(heatpump.central~scale(prob_hispanic)+
             scale(RES_ABOVE_)+
             scale(APPRAISE_1)+
             as.factor(RES_ORIG_YEAR_BUILT<1940)+
             as.factor(D_CLASS)+
             f(PAR_YEAR,model="iid"),
           data=allparcels2)

head(allparcels)
allparcels$mech.permit01
sows$heatpump.central %>% table()
sows$ac %>% table()
sows$furnace %>% table()





try1<-inla(heatpump.central~scale(prob_hispanic)+
             scale(RES_ABOVE_)+
             scale(APPRAISE_1)+
             as.factor(RES_ORIG_YEAR_BUILT<1940)+
             as.factor(D_CLASS)+
             f(blockgroup,model="iid")+
             f(PAR_YEAR,model="iid"),family="binomial",
           data=)

try1$summary.random$blockgroup
summary(try1)



rm(allparrcels2)
allparcels2$heatingpermit[is.na(allparcels2$heatingpermit)]<-0
allparcels2$month<-allparcels2$Date.Issued %>% lubridate::mdy() %>% lubridate::month()
allparcels2$week<-allparcels2$Date.Issued %>% lubridate::mdy() %>% lubridate::week()

allparrcels2<-rbind(allparcels2,filter(allparcels2, PAR_YEAR==2023,heatingpermit==0) %>% mutate(heatingpermit=NA,PAR_YEAR=2024))

modelfit.stage1$summary.fitted.values$mean

allsamp<-sample_n(allparrcels2,50000)
table(allsamp$heatingpermit)
library(INLA)
pc.prec1 = list(prec = list(prior = "pc.prec", param = c(.5, 0.01)))

modelfit.stage1<-inla(heatingpermit~1+
                          #fixed portion of model (or pooled estimates)
                          scale(as.numeric(RES_ABOVE_GRADE_AREA))+
                          scale(log(APPRAISE_1+1))+
                          scale(MED_HH_INCOME)+
                          scale(perc.less.5)+
                          scale(perc.hisp)+
                          scale(prob_hispanic)+
                          scale(perc.over.65)+
                          scale(perc.bach)+
                          shortermrental+
                        scale(MED_HH_INCOME)+
                          scale(heatanomoly)+
                        f(PAR_YEAR,model="rw1",hyper=pc.prec1)+
                          f(D_CLASS_CN, model="iid", hyper=pc.prec1)+
                          f(yb.decade, model="rw1", hyper=pc.prec1)+
                          f(blockgroup, model="iid", hyper=pc.prec1)+
                          f(SCHEDNUMCHAR, model="iid",hyper=pc.prec1),
                        family="binomial",
                        data=as.data.frame(allparrcels2), 
                        control.predictor = list(link = 1, compute = TRUE),
                        control.compute=list(config = TRUE))



modelfit.stage1a<-inla(heatingpermit~1+
                        #fixed portion of model (or pooled estimates)
                        scale(as.numeric(RES_ABOVE_GRADE_AREA))+
                        scale(log(APPRAISE_1+1))+
                        scale(MED_HH_INCOME)+
                        scale(perc.less.5)+
                        scale(perc.hisp)+
                        scale(prob_hispanic)+
                        scale(perc.over.65)+
                        scale(perc.bach)+
                        shortermrental+
                        scale(MED_HH_INCOME)+
                        scale(heatanomoly)+
                        f(PAR_YEAR,model="rw1",hyper=pc.prec1)+
                        f(D_CLASS_CN, model="iid", hyper=pc.prec1)+
                        f(yb.decade, model="rw1", hyper=pc.prec1)+
                        f(blockgroup, model="iid", hyper=pc.prec1)+
                        f(SCHEDNUMCHAR, model="iid",hyper=pc.prec1),
                      family="binomial",
                      data=as.data.frame(allsamp), 
                      control.predictor = list(link = 1, compute = TRUE),
                      control.compute=list(config = TRUE))
allsamp$noncondfurnace<-as.numeric(allsamp$noncondfurnace)

modelfit.stage2<-inla(noncondfurnace~1+
                          scale(as.numeric(RES_ABOVE_GRADE_AREA))+
                          scale(log(APPRAISE_1+1))+
                          scale(MED_HH_INCOME)+
                          scale(perc.less.5)+
                          scale(perc.hisp)+
                          scale(prob_hispanic)+
                          scale(perc.over.65)+
                          scale(perc.bach)+
                          shortermrental+
                          scale(heatanomoly)+
                        scale(MED_HH_INCOME)+
                          f(PAR_YEAR,model="rw1",hyper=pc.prec1)+
                          f(week, model="rw2", cyclic=T,hyper=pc.prec1)+ 
                          f(D_CLASS_CN, model="iid", hyper=pc.prec1)+
                          f(yb.decade, model="rw1", hyper=pc.prec1)+
                          f(blockgroup, model="iid", hyper=pc.prec1)+
                          f(Contractor.s.Name, model="iid")+
                          f(SCHEDNUMCHAR, model="iid",hyper=pc.prec1),
                        family="binomial",
                        data=as.data.frame(allsamp), 
                        control.predictor = list(link = 1, compute = TRUE),
                        control.compute=list(config = TRUE))
allsamp$fitvalue<-modelfit.stage1$summary.fitted.values$mean*modelfit.stage2$summary.fitted.values$mean
library(ggplot2)
ggplot(allsamp  %>% filter(PAR_YEAR==2024))+geom_sf(aes(colour=fitvalue),size=.5)+scale_colour_viridis_c()

redlined<-esri2sf::esri2sf("https://services1.arcgis.com/zdB7qR0BtYrg0Xpl/ArcGIS/rest/services/ODC_HLTH_REDLINING_A/FeatureServer/307")

       