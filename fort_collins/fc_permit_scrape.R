#
library(plyr)
library(httr)
library(rvest)
library(tidyverse)
library(RSelenium)




for(k in 1:2000){
  try({
    gc()
    
    system("docker restart f133cb45c17757410b479cc6eed344e692e1afc2f36a5233e8041cff82ff524f")
    Sys.sleep(60)
    
    fcfiles<-list.files("Documents/qt-local/fortcollins/fc_record_files/",full.names=T)
    fcfiles<-lapply(fcfiles,read.csv)
    fcfiles<-bind_rows(fcfiles)
    fcfiles<-unique(fcfiles)
    fcfiles<-filter(fcfiles,Record.Type%in%c("Residential Mechanical","Residential Electrical","Residential Water Heater","Residential Fireplace-Wood Burning Stove","Electric Service Change"))
    fcfiles<-filter(fcfiles,paste0(Record..,".txt") %in% list.files("Documents/qt-local/fortcollins/permit_links/")==F)
    
    

    list.files()
    remDr <- remoteDriver(remoteServerAddr = "localhost",port = 4444L,browserName = "firefox")
    remDr$open()
    remDr$navigate("https://accela-aca.fcgov.com/CitizenAccess/Cap/CapHome.aspx?module=Building&TabName=HOME")
    Sys.sleep(3)
    
    downloadpermit<-function(permitnumber){
      remDr$navigate("https://accela-aca.fcgov.com/CitizenAccess/Cap/CapHome.aspx?module=Building&TabName=HOME")
      Sys.sleep(5)
      el1<-remDr$findElement(using="xpath",value='//*[(@id = "ctl00_PlaceHolderMain_generalSearchForm_txtGSPermitNumber")]')
      el1$clearElement()
      el1$clickElement()
      el1$sendKeysToElement(list(permitnumber,selKeys$enter))
      Sys.sleep(10)
      pageURL<-remDr$getCurrentUrl()[[1]]
      writeLines(pageURL,paste0("Documents/qt-local/fortcollins/permit_links/",permitnumber,".txt"))
      
      #remDr$findElement(using="xpath",'//*[contains(concat( " ", @class, " " ), concat( " ", "rec-downarrow", " " ))]')$clickElement()
      remDr$findElement(using="xpath",'//*[(@id = "imgMoreDetail")]')$clickElement()
      Sys.sleep(1)
      try({remDr$findElement(using="xpath",'//*[(@id = "imgAddtional")]')$clickElement()})
      try({remDr$findElement(using="xpath",'//*[(@id = "imgASI")]')$clickElement()})
      Sys.sleep(1)
      try({remDr$findElement(using="xpath",'//*[(@id = "imgRc")]')$clickElement()})
      Sys.sleep(1)
      try({remDr$findElement(using="xpath",'//*[(@id = "imgParcel")]')$clickElement()})
      Sys.sleep(1)
      htmlout<-remDr$getPageSource()[[1]]
      writeLines(htmlout,paste0("Documents/qt-local/fortcollins/permit_pages/",permitnumber,".html"))
      Sys.sleep(1)
    }

        for(i in k:length(fcfiles$Record..)) {downloadpermit(fcfiles$Record..[i])
      cat(i)}
    
    remDr$close()
    
  })
}

