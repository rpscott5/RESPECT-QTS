

# Run this code --> docker run --rm -it -p 4444:4444 -p 5900:5900 -p 7900:7900 --shm-size 3g -v /Users/rpscott/dockerfun/ seleniarm/standalone-firefox

library(tidyverse)
library(RSelenium)
library(httr)
library(rvest)

    system("docker restart bcea2f1f20d46d0f2fcb75a350a65f3f285d81f9b3bbe9a6bc69f2b05664e8db")
    Sys.sleep(60)
    
    permit1<-read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vTl2bx7kxV3dq2eDNy5SHuQARuG5xqKN6MEUYBPIeErPCGV54lamNjTMWRg_Isvfg0ZYMsn0GVU8VSD/pub?gid=177088608&single=true&output=csv")
    permit1<-filter(permit1,Stat.Code%in%c(310,313))
    permit1$year<-permit1$Final.Date %>% mdy() %>% lubridate::year()
    permit1<-filter(permit1, stringr::str_detect(Permit..,"2018|2019"))
    permit1<-filter(permit1,Permit..%in% gsub(".txt","",list.files("Documents/qt-local/denmet/Denver/permit_links_2015"))==F) 
    head(permit1)

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
      writeLines(pageURL,paste0("Documents/qt-local/denmet/Denver/permit_links_2015/",permitnumber,".txt"))
      
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

        for(i in 1:length(permit1$Permit..)) {downloadpermit(permit1$Permit..[i])
      cat(i)}
