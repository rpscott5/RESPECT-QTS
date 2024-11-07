#
library(plyr)
library(httr)
library(rvest)
library(tidyverse)
library(RSelenium)




for(k in 1:2000){
  try({
    gc()
    
    system("docker restart ed6737371086f49991eaa14a558999ed2a5162144a96e83fbcf1c5857d1edcb2")
    Sys.sleep(60)
    fcfiles<-list.files("Documents/qt-local/fortcollins/fc_record_files/",full.names=T)
    fcfiles<-lapply(fcfiles,read.csv)
    fcfiles<-bind_rows(fcfiles)
    fcfiles<-unique(fcfiles)
    fcfiles<-filter(fcfiles,Record.Type%in%c("Residential Mechanical","Residential Electrical","Residential Water Heater","Residential Fireplace-Wood Burning Stove","Electric Service Change"))
    perm.link<-list.files("Documents/qt-local/fortcollins/permit_links/",full.names=T)
    file1<-sapply(perm.link,readLines)
    file2<-perm.link[which(file1 %>% stringr::str_detect(.,"https")==F)]
    fcfiles<-filter(fcfiles, Record..%in% gsub(".html","",basename(perm.link))==F)
    fcfiles<-filter(fcfiles, Record.Type=="Residential Mechanical",lubridate::year(lubridate::mdy(Date.Submitted))>2017)
    rm(sess)
    gc()
    sess <- read_html_live("https://accela-aca.fcgov.com/CitizenAccess/Cap/CapHome.aspx?module=Building&TabName=HOME")
    sess$initialize("https://accela-aca.fcgov.com/CitizenAccess/Cap/CapHome.aspx?module=Building&TabName=HOME")
    sess$view()
    sess$click("#ctl00_PlaceHolderMain_generalSearchForm_txtGSPermitNumber")
    sess$press(css="#ctl00_PlaceHolderMain_generalSearchForm_txtGSPermitNumber",key_code=c("Backspace"))
    sess$press(css="#ctl00_PlaceHolderMain_generalSearchForm_txtGSPermitNumber",key_code=c("Backspace"))
    sess$press(css="#ctl00_PlaceHolderMain_generalSearchForm_txtGSPermitNumber",key_code=c("Backspace"))
    sess$press(css="#ctl00_PlaceHolderMain_generalSearchForm_txtGSPermitNumber",key_code=c("Backspace"))
    sess$press(css="#ctl00_PlaceHolderMain_generalSearchForm_txtGSPermitNumber",key_code=c("Backspace"))
    sess$press(css="#ctl00_PlaceHolderMain_generalSearchForm_txtGSPermitNumber",key_code=c("Backspace"))
    
    
    
    sess$type(text=fcfiles$Record..[1],css="#ctl00_PlaceHolderMain_generalSearchForm_txtGSPermitNumber")
    rvest:::live
    sess$press(css="#ctl00_PlaceHolderMain_generalSearchForm_txtGSPermitNumber",key_code=c("Enter"))
    
    sess$scroll_into_view(css="#ctl00_PlaceHolderMain_btnNewSearch span")
    sess$click(css="#ctl00_PlaceHolderMain_btnNewSearch span")
    
    sess$html_elements(css="a")
    sess$press(css=NULL,key_code = "KeyA",modifier="Alt")
    sess$click('#imgMoreDetail')
    sess$click('#imgAddtional')
    sess$click('#imgASI')
    sess$click('#imgRc')
    sess$click('#imgParcel')
    sess$html_elements()
    sessform<-sess$html_elements('#ctl00_PlaceHolderMain_PermitDetailList1_updatePanel :nth-child(1)')

        sess %>% xml2::url_absolute()
    sess$initialize()
    sessform %>% xml2::write_html("Downloads/templines.html")
    xml2::url_absolute(
    sess$print()
      xml2::write_html("Downloads/templines.html")
    sess$html_elements('#imgMoreDetail')
    listels$click()
    sess$click(css="//*[(@id = 'imgMoreDetail')]")
    
    sess$html_elements('#imgMoreDetail')

    rows %>% html_element(".grant-aid") %>% html_text()
    
    remDr <- remoteDriver(remoteServerAddr = "localhost",port = 4444L,browserName = "firefox")
    remDr$open()
    remDr$navigate("https://accela-aca.fcgov.com/CitizenAccess/Cap/CapHome.aspx?module=Building&TabName=HOME")
    Sys.sleep(3)
    
    serverReset<-function(){
      
      
    remDr <- remoteDriver(remoteServerAddr = "localhost",port = 4444L,browserName = "firefox")
    remDr$open()
    remDr$navigate("https://accela-aca.fcgov.com/CitizenAccess/Cap/CapHome.aspx?module=Building&TabName=HOME")
    }
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
      remDr$close()
    }

        for(i in 2:length(fcfiles$Record..)) {downloadpermit(fcfiles$Record..[i])
      cat(i)}
    
    remDr$close()
    
  })
}

