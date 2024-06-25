library(plyr)
library(dplyr)
library(reticulate)

fcfiles<-list.files("/Users/rpscott/Documents/qt-local/fortcollins/fc_record_files/",full.names=T)
fcfiles<-lapply(fcfiles,read.csv)
fcfiles<-bind_rows(fcfiles)
fcfiles<-unique(fcfiles)
usadd <- import("usaddress")
addp<-lapply(fcfiles$Address,function(X) usadd$parse(X)) 

makeframeaddress<-function(K){
f1<-data.frame("values"=K %>% lapply(., function(X) X[[1]]) %>% unlist()) %>% t()
colnames(f1)<-K %>% lapply(., function(X) X[[2]]) %>% unlist()
f1
}
addp<-lapply(addp,makeframeaddress) 
addp<-lapply(addp,as.data.frame)
names(addp)<-fcfiles$Record..
for(i in 1:length(addp)) {try({addp[[i]]$Record..<-names(addp)[i]})}
#addp<-lapply(addp, function(X) mutate(X,Record..=names(X)))
addp_out<-addp %>% rbind.fill()
head(addp_out)
addp_out<-addp_out %>% select(Record..,AddressNumber,StreetNamePreDirectional,StreetName,StreetNamePostType, StreetNamePreType)
addp_out$unit<-NA
head(addp_out)

addp_out$unit[is.na(addp_out$StreetNamePreType)==F]<-addp_out$StreetName[is.na(addp_out$StreetNamePreType)==F]
addp_out$StreetName[is.na(addp_out$StreetNamePreType)==F]<-addp_out$StreetNamePreType[which(is.na(addp_out$StreetNamePreType)==F)]
assessments<-read.csv("Documents/qt-local/fortcollins/Assessments Completed 2020 2021.csv")
head(assessments)
fcfiles$firstline<-fcfiles$Address %>% stringr::str_split_fixed(",",2) %>% .[,1]

assess_perm<-filter(fcfiles,stringr::str_squish(tolower(firstline)) %in% tolower(stringr::str_squish(assessments$Customer...Site.Address.Line.1)))
assess_perm %>% head()
saveRDS(assess_perm,"Documents/GitHub/RESPECT-QTS/fort_collins/assess_permits.rds")

head(assess_perm)
prmpg<-list.files("Documents/qt-local/fortcollins/permit_pages/",full.names=T)
prmpg.out<-join(sapply(prmpg,function(X){
  read_html(X) %>% rvest::html_elements(xpath='//*[@id="ctl00_PlaceHolderMain_PermitDetailList1_updatePanel"]/div[1]') %>% html_children() %>% html_table() %>% .[[1]] %>% .$X1 %>% .[stringr::str_which(.,"Work Description")] }) %>% reshape2::melt(),sapply(prmpg,function(X){
    read_html(X) %>% rvest::html_elements(xpath='//*[@id="ctl00_PlaceHolderMain_PermitDetailList1_updatePanel"]/div[1]') %>% html_children() %>% html_table() %>% .[[1]] %>% .$X1 %>% .[stringr::str_which(.,"Applicant")]
  }) %>% reshape2::melt() %>% rename(value2=value))

head(prmpg.out)
prmpg.out<-cbind(prmpg.out,prmpg.out$value2 %>% stringr::str_match(.,"^Applicant: (?<applicant>\\w+ \\w+) (?<company>[\\w\\W]+?[0-9])") %>% as.data.frame() %>% mutate(company=stringr::str_squish(gsub("[0-9]$","",company))) %>% select(applicant, company))
prmpg.out$Record..<-basename(prmpg.out$L1) %>% gsub(".html","",.)
assess_perm<-left_join(assess_perm,prmpg.out %>% select(Record..,contname,applicant,company))
assess_perm$address_processed<-stringr::str_squish(tolower(assess_perm$firstline))
assess_perm$date_perm.processed<-assess_perm$Date.Submitted %>% lubridate::mdy()
assessments$date_processed<-assessments$Project...Creation.Date %>% lubridate::dmy()
assessments$address_processed<-tolower(stringr::str_squish(assessments$Customer...Site.Address.Line.1))

assessment_joined<-left_join(assessments, assess_perm)
assessment_joined<-filter(assessment_joined, date_processed<date_perm.processed)
ig1<-assessment_joined %>% select(Schd.Member...Name,company,Record.Type) %>% igraph::graph_from_data_frame() 
ig1<-na.omit(ig1)

igraph::E(ig1)$weight <- 1 
ig1<-igraph::delete.edges(ig1, igraph::E(ig1)$weight <= 1)
ig1<-igraph::simplify(ig1, edge.attr.comb=list(weight="sum"))
ig1$weight
igd3<-networkD3::igraph_to_networkD3(ig1) 
?networkD3::forceNetwork

networkD3::forceNetwork(Links=igd3$links,Nodes=igd3$nodes,NodeID="name",Value=igd3$links$,Group=1)
