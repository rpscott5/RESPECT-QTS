library(tidyverse)
library(rvest)
library(RSelenium)

parlist<-rbind(readRDS('Documents/dbc/qts_data/pueblo_parcels.rds') %>% as.data.frame() %>% dplyr::select(PAR_NUM),readRDS('Documents/dbc/qts_data/pueblo_parcels_2.rds') %>% as.data.frame %>% dplyr::select(PAR_NUM)) %>% unique()

remDr <- remoteDriver(remoteServerAddr = "localhost",port = 4444L,browserName = "firefox")
remDr$open()

Sys.sleep(10)
for(i in 13163:nrow(parlist)){
  remDr$navigate(paste0("https://property.spatialest.com/co/pueblo/#/property/",parlist$PAR_NUM[i],"/print"))
  Sys.sleep(7)
  rawd = remDr$getPageSource()[[1]]
  rawd %>% read_html() %>% xml2::write_html(paste0("Documents/qt-local/property_pages/",parlist$PAR_NUM[i],".html"))
}

remDr$close()
rm(remDr)

