

p1<-rvest::read_html("https://property.spatialest.com/co/pueblo/#/property/2631000003")
p1 %>% html_node(.,css=".img-fluid")
?html_element
parlist<-rbind(readRDS('Documents/dbc/qts_data/pueblo_parcels.rds') %>% as.data.frame() %>% dplyr::select(PAR_NUM),readRDS('Documents/dbc/qts_data/pueblo_parcels_2.rds') %>% as.data.frame %>% dplyr::select(PAR_NUM)) %>% unique()

remDr <- remoteDriver(remoteServerAddr = "localhost",port = 4444L,browserName = "firefox")
remDr$open()
remDr$close()
for(i in 1340:nrow(parlist)){
  remDr$navigate(paste0("https://property.spatialest.com/co/pueblo/#/property/",parlist$PAR_NUM[i],"/print"))
  Sys.sleep(5)
  rawd = remDr$getPageSource()[[1]]
  rawd %>% read_html() %>% xml2::write_html(paste0("Documents/qt-local/property_pages/",parlist$PAR_NUM[i],".html"))
}

parlist$PAR_NUM[1]
#docker run --rm -it -p 4444:4444 -p 5900:5900 -p 7900:7900 --shm-size 3g -v /Users/rpscott/dockerfun/ seleniarm/standalone-firefox

#http://localhost:7900/
remDr <- remoteDriver(remoteServerAddr = "localhost",port = 4444L,browserName = "firefox")
remDr$open()
remDr$close()
for(i in 1340:nrow(parlist)){
remDr$navigate(paste0("https://property.spatialest.com/co/pueblo/#/property/",parlist$PAR_NUM[i],"/print"))
Sys.sleep(5)
rawd = remDr$getPageSource()[[1]]
rawd %>% read_html() %>% xml2::write_html(paste0("Documents/qt-local/property_pages/",parlist$PAR_NUM[i],".html"))
}
