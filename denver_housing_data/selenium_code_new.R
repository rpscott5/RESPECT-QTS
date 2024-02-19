library(rvest)

r1<-rvest::read_html("https://www.denvergov.org/AccelaCitizenAccess/Cap/CapHome.aspx?module=Development&TabName=Home")
r1 %>% html_form()
https://www.denvergov.org/AccelaCitizenAccess/Report/ShowReport.aspx?module=Development&reportID=380&reportType=LINK_REPORT_LIST

library(RSelenium)


# Run this code --> docker run --rm -it -p 4444:4444 -p 5900:5900 -p 7900:7900 --shm-size 3g local-seleniarm/standalone-firefox


docker run --rm -it -p 4444:4444 -p 5900:5900 -p 7900:7900 --shm-size 3g -v /Users/rpscott/dockerfun/ seleniarm/standalone-firefox

remDr <- remoteDriver(remoteServerAddr = "localhost",port = 4444L,browserName = "firefox")
remDr$open()
remDr$close()
remDr$navigate("https://www.denvergov.org/AccelaCitizenAccess/Cap/CapHome.aspx?module=Development&TabName=Home")

remDr$findElement(using="id",value="ctl00_PlaceHolderMain_generalSearchForm_ddlGSPermitType")$clickElement()
drd<-remDr$findElement(using="name",value="ctl00$PlaceHolderMain$generalSearchForm$txtGSParcelNo")


$sendKeysToElement(list("0226214024000"))
#ctl00_PlaceHolderMain_generalSearchForm_ddlGSPermitType
parcels<-read.csv("Downloads/parcels.csv")
library(plyr)
library(dplyr)
parcels %>% head()
library(rvest)
hm1<-read_html("https://www.denvergov.org/AccelaCitizenAccess/Cap/CapHome.aspx?module=Development&TabName=Home")
hmform<-hm1 %>% html_form()
hmform<-html_form_set(ctl00$HeaderNavigation$hdnShowReportLink)
?html_form_set

idcode<-stringr::str_split_fixed("22CAP-00000-03FM5",pattern="-",n=3)
paste0("https://www.denvergov.org/AccelaCitizenAccess/Cap/CapDetail.aspx?Module=Development&TabName=Development&capID1=",idcode[1,1],"&capID2=",idcode[1,2],"&capID3=",idcode[1,3],"&agencyCode=DENVER&IsToShowInspection=")

remDr$navigate(paste0("https://www.denvergov.org/AccelaCitizenAccess/Cap/CapDetail.aspx?Module=Development&TabName=Development&capID1=",idcode[1,1],"&capID2=",idcode[1,2],"&capID3=",idcode[1,3],"&agencyCode=DENVER&IsToShowInspection="))
remDr$navigate("https://www.denvergov.org/AccelaCitizenAccess/Report/ReportParameter.aspx?module=Development&reportID=380&reportType=LINK_REPORT_LIST")
file.copy(list.files("dockerfun",full.names = T)[1],paste0("permitdata/",paste0(paste0(idcode[1,],collapse="_"),".pdf")))
file.remove("dockerfun/CrystalViewer.pdf")

#ctl00_HeaderNavigation_lblReports
remDr$getCurrentUrl()
remDr$click()
remDr$sendKeysToActiveElement(list(key = c("command","S")))
selKeys
GET https://www.denvergov.org/AccelaCitizenAccess/v4/records

h1<-session(paste0("https://www.denvergov.org/AccelaCitizenAccess/Cap/CapDetail.aspx?Module=Development&TabName=Development&capID1=",idcode[1,1],"&capID2=",idcode[1,2],"&capID3=",idcode[1,3],"&agencyCode=DENVER&IsToShowInspection="))
h1<-session_follow_link("https://www.denvergov.org/AccelaCitizenAccess/Report/ReportParameter.aspx?module=Development&reportID=380&reportType=LINK_REPORT_LIST") 
install.packages("pdftools")
h1 %>% View()
selKeys
https://www.denvergov.org/AccelaCitizenAccess/Report/ReportParameter.aspx?module=Development&reportID=19716&reportType=LINK_REPORT_LIST



dparcels<-read.csv("https://www.denvergov.org/media/gis/DataCatalog/parcels/csv/parcels.csv")
paste0("0",dparcels[10,]$SCHEDNUM %>% as.character())
dparcels$MAPNUM[5]
dparcels$BLKNUM[5]
dparcels$PARCELNUM[5]
dparcels$APPENDAGE[5]
dparcels$PARCELNUM
r1<-rvest::read_html("https://www.denvergov.org/AccelaCitizenAccess/Cap/CapHome.aspx?module=Development&TabName=Home")
r2<-r1 %>% html_form()
r3<-r2[[1]] %>% rvest::html_form_set('ctl00$PlaceHolderMain$generalSearchForm$txtGSStartDate'="01/01/2000")  %>% rvest::html_form_set('ctl00$PlaceHolderMain$generalSearchForm$txtGSParcelNo'=paste0("0",dparcels[10,]$SCHEDNUM %>% as.character()))
r4<-html_form_submit(r3,submit='Submit')
?html_form_submit
r4 %>% read_html() %>% html_table()
'id'="RecordId"
