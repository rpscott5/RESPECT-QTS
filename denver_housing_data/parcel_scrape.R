library(tidyverse)
library(rvest)
parc<-read.csv("Documents/qt-local/DenMet/Denver/parcels.csv")
parc<-filter(parc,stringr::str_detect(D_CLASS_CN,"SFR|RESIDENTIAL\\-ROWHOUSE|RESIDENTIAL\\-DUPLEX|RESIDENTIAL-TRIPLEX"))

for(i in 3865:nrow(parc)){
  try({
phtml<-rvest::read_html(paste0("https://www.denvergov.org/property/realproperty/summary/",parc$SCHEDNUM[i]))
Sys.sleep(2)
phtml %>% rvest::html_table() %>% saveRDS(paste0("Documents/qt-local/denmet/Denver/parcel_details/",parc$SCHEDNUM[i],".rds"))
rm(phtml)
})
}
