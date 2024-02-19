puebr4<-readRDS("/Users/rpscott/Documents/GitHub/RESPECT-QTS/pueblo_housing_data/processed_data/full_s_2018.version3.rds")
puebr5<-readRDS("/Users/rpscott/Documents/GitHub/RESPECT-QTS/pueblo_housing_data/processed_data/permit_s_2018.version3.rds")
pueblo_contractors<-read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vRj8AkwjfguXDbr14v80dLPcOFzQxjKZ9ZMKbln08X7gQPNDgT94mS7alolTmwH6mCais4Nd0g2T5nK/pub?output=csv")

#contractor_code

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

batch1<-c(30, 134, 68, 103,76,10,12,106,96,97,11,100)
set.1129<-c(46,27,105,27,37,16,17,14,9,13)
set.1218<-c(25,41,79,111,39,19,21,126,121,38,34,84,88,110,130,58,38,34,120,116,121,126,131)
oldsamps<-list.files("Documents/qt-local/qualsamples/",full.names=T) %>% lapply(read.csv)
oldamps<-lapply(oldsamps,function(X) X %>% select(Permit_No,ASSESSOR_PARID)) %>% bind_rows() %>% unique()
puebsamped<-filter(puebr5, Year==2022,SaleYear!=2023, placeidinla%in%c(batch1,set.1129,set.1218))
puebsamped.new<-puebsamped[which(puebsamped$Permit_No%in%oldamps$Permit_No==F),]
missed_parcels<-puebsamped.new
govaid.sample<-filter(puebr5, Year==2022, stringr::str_detect(Contractor,"FLOW RIGHT|PUEBLO COUNTY")) %>% filter(.,ASSESSOR_PARID%in%oldamps$ASSESSOR_PARID==F)
write.csv(missed_parcels,"general_sample_missing_in_hoods.2.6.csv")
write.csv(govaid.sample,"2022govaid.2.6.csv")



