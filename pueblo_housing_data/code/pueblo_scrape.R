
library(plyr)
library(dplyr)
library(RSelenium)
library(rvest)
psearch<-read_html("https://www.prbd.com/searches/psearch.php")
stab<-psearch %>% html_element("#streetlist") %>% html_children() %>% html_attr("value") %>% .[-1]

remDr <- remoteDriver(remoteServerAddr = "localhost",port = 4444L,browserName = "firefox")
remDr$open()
saveRDS(stab,"Documents/dbc/pueblo_streets.rds")
for(i in 2524:length(stab)){
  Sys.sleep(runif(1,7,15))
  remDr$navigate("https://www.prbd.com/searches/psearch.php")
  street<-remDr$findElement(using="css selector","#street")
  street$clickElement()
  street$sendKeysToElement(list(stab[i]))
  remDr$screenshot(display = T)
  button1<-remDr$findElement(using="css selector","p:nth-child(9) input")
  button1$clickElement()
  Sys.sleep(7)
  remDr$screenshot(display = T)
  ht1<-remDr$getPageSource()[[1]] %>% read_html()
  ht1 %>% html_table() %>% .[[1]] %>% saveRDS(paste0("Documents/dbc/pueblo_permits/",gsub('/',"_",stab[i]),".rds"))
}