library(tidyverse)
library(rvest)
library(RSelenium)

parlist<-readRDS('Documents/qt-local/mechanicals/mech_j_pp.rds') %>% as.data.frame() %>% dplyr::select(ASSESSOR_PARID) %>% unique() 

remDr <- remoteDriver(remoteServerAddr = "localhost",port = 4444L,browserName = "firefox")
remDr$open()

Sys.sleep(10)
for(i in 4478:nrow(parlist)){
  remDr$navigate(paste0("https://property.spatialest.com/co/pueblo/#/property/",parlist$ASSESSOR_PARID[i],"/print"))
  Sys.sleep(7)
  rawd = remDr$getPageSource()[[1]]
  rawd %>% read_html() %>% xml2::write_html(paste0("Documents/qt-local/mechanicals/property_pages/",parlist$ASSESSOR_PARID[i],".html"))
}

remDr$close()
rm(remDr)

