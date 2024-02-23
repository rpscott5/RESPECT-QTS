library(tidyverse)
library(rvest)

pg1<-list.files("Documents/qt-local/denmet/Denver/permit_pages/",full.names=T)
pg1.1<-lapply(pg1,
              function(X){
                pg1.0<-read_html(X)
                data.frame("inspector"=pg1.0 %>% html_element(xpath='//*[@id="ctl00_PlaceHolderMain_InspectionList_gvListCompleted"]') %>% html_text() %>% stringr::str_extract("Result by\\: ([A-Z][a-z]+ [A-Z][a-z]+)",group=1),
                           "contact"=pg1.0 %>% html_element(xpath='//*[@id="ctl00_PlaceHolderMain_PermitDetailList1_RelatContactList"]/tbody/tr/td[1]/div/div[1]/ul/table[2]') %>% html_text(),"workclass"=pg1.0 %>% html_element(xpath='//*[@id="ctl00_PlaceHolderMain_PermitDetailList1_phPlumbingGroup"]/div[3]') %>% html_text() %>% stringr::str_squish(),"stat1"=pg1.0 %>% html_element(xpath='//*[@id="ctl00_PlaceHolderMain_PermitDetailList1_phPlumbingGroup"]') %>% html_text() %>% stringr::str_squish() %>% stringr::str_extract(.,pattern="STAT Code 1: ([0-9]+)",group=1),"stat2"=pg1.0 %>% html_element(xpath='//*[@id="ctl00_PlaceHolderMain_PermitDetailList1_phPlumbingGroup"]') %>% html_text() %>% stringr::str_squish() %>% stringr::str_extract(.,pattern="STAT Code 2: ([0-9]+)",group=1),"stat3"=pg1.0 %>% html_element(xpath='//*[@id="ctl00_PlaceHolderMain_PermitDetailList1_phPlumbingGroup"]') %>% html_text() %>% stringr::str_squish() %>% stringr::str_extract(.,pattern="STAT Code 3: ([0-9]+)",group=1))
              })

names(pg1.1)<-pg1
pg1.1<-bind_rows(pg1.1,.id="filename")
pg1.1 %>% saveRDS("Documents/qt-local/denmet/Denver/permitdetails.rds")