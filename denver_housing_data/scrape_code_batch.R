# Run this code --> docker run --rm -it -p 4444:4444 -p 5900:5900 -p 7900:7900 --shm-size 3g -v /Users/rpscott/dockerfun/ seleniarm/standalone-firefox

library(tidyverse)
library(RSelenium)
library(httr)
library(rvest)
for(k in 1001:2000){
  try({
    gc()
    
system("docker restart bcea2f1f20d4")
    Sys.sleep(60)
permit1<-read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vQvD__E0zyFKQJoIRlwsRlDtZzRpTA-eupD4jPyAWLrLIxz002jT3tCguh8CjejFLkzPtOPlWbwS7PY/pub?gid=1505581905&single=true&output=csv")
permit1<-filter(permit1,Permit..%in% gsub(".txt","",list.files("Documents/qt-local/denmet/Denver/permit_links"))==F) 
permit1<-filter(permit1,Valuation %>% gsub("\\$|\\,","",.) %>% as.numeric()>5000)
remDr <- remoteDriver(remoteServerAddr = "localhost",port = 4444L,browserName = "firefox")
remDr$open()
remDr$navigate("https://aca-prod.accela.com/DENVER/Default.aspx")
Sys.sleep(3)

downloadpermit<-function(permitnumber){
  remDr$navigate("https://aca-prod.accela.com/DENVER/Cap/CapHome.aspx?module=Development&TabName=Home")
  Sys.sleep(5)
  el1<-remDr$findElement(using="xpath",value='//*[(@id = "ctl00_PlaceHolderMain_generalSearchForm_txtGSPermitNumber")]')
  el1$clearElement()
  el1$clickElement()
  permcode<-stringr::str_split_fixed(permitnumber,"-",n=3)
  el1$sendKeysToElement(list(selKeys$delete,permcode[1],selKeys$subtract,permcode[2],selKeys$subtract,permcode[3],selKeys$enter))
  Sys.sleep(10)
  pageURL<-remDr$getCurrentUrl()[[1]]
  writeLines(pageURL,paste0("Documents/qt-local/denmet/Denver/permit_links/",permitnumber,".txt"))
  
  #remDr$findElement(using="xpath",'//*[contains(concat( " ", @class, " " ), concat( " ", "rec-downarrow", " " ))]')$clickElement()
  remDr$findElement(using="xpath",'//*[(@id = "imgMoreDetail")]')$clickElement()
  Sys.sleep(1)
  remDr$findElement(using="xpath",'//*[(@id = "imgASI")]')$clickElement()
  Sys.sleep(1)
  remDr$findElement(using="xpath",'//*[(@id = "imgRc")]')$clickElement()
  Sys.sleep(1)
  remDr$findElement(using="xpath",'//*[(@id = "imgParcel")]')$clickElement()
  Sys.sleep(1)
  htmlout<-remDr$getPageSource()[[1]]
  writeLines(htmlout,paste0("Documents/qt-local/denmet/Denver/permit_pages/",permitnumber,".html"))
  htmlout %>% read_html() %>% html_text() %>% stringr::str_extract(.,"Detail exactly what activities you are performing under this work\\)\\:([\\w\\W]+)Does any work affect or add",group=1) %>% stringr::str_squish() %>% writeLines(paste0("Documents/qt-local/denmet/Denver/permit_sow/",permitnumber,".txt"))
  Sys.sleep(1)
}

for(i in k:length(permit1$Permit..)) {downloadpermit(permit1$Permit..[i])
  cat(i)}

remDr$close()

})
}

head(allparcels2)
