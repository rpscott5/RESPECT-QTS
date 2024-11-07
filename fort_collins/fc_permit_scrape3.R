remotes::install_github("ashbythorpe/selenider")
library("selenider")
session <- selenider_session(
  "chromote",
  timeout = 10
)
fcfiles$year<-fcfiles$Date.Submitted %>% lubridate::mdy() %>% year()
fcfiles2<-filter(fcfiles,year>=c(2020))
fcfiles2<-filter(fcfiles2, Record..%in%c(list.files("Documents/qt-local/fortcollins/permit_links/") %>% basename() %>% gsub(".txt","",.))==F)
for (i in 1:nrow(fcfiles2)){
  permitnumber<-fcfiles2$Record..[i]
  Sys.sleep(2)
open_url("https://accela-aca.fcgov.com/CitizenAccess/Cap/CapHome.aspx?module=Building&TabName=HOME")
Sys.sleep(2)

s("#ctl00_PlaceHolderMain_generalSearchForm_txtGSPermitNumber") |>
  elem_scroll_to() |>
  elem_click() %>% 
  elem_clear_value() %>%
  elem_send_keys(permitnumber)
Sys.sleep(2)

s("#ctl00_PlaceHolderMain_btnNewSearch span") %>%
  elem_scroll_to() %>% 
  elem_click()
Sys.sleep(2)

fileurl<-selenider::current_url() 
writeLines(fileurl,paste0("Documents/qt-local/fortcollins/permit_links/",permitnumber,".txt"))


Sys.sleep(2)
try({
s('#imgMoreDetail') %>% elem_click() 
})
Sys.sleep(1)
try({
s('#imgAddtional')%>% elem_click() 
})
Sys.sleep(1)
try({
s('#imgASI')%>% elem_click() 
})
Sys.sleep(1)
#try({
#s('#imgRc')%>% elem_click()  
#})
Sys.sleep(2)
try({
s('#imgParcel')%>% elem_click() 
})
Sys.sleep(3)

pageout<-selenider::get_page_source()
Sys.sleep(3)

pageout %>% xml2::write_html(paste0("Documents/qt-local/fortcollins/permit_pages/",permitnumber,".html"))
cat(i)
Sys.sleep(3)
}

gc()
