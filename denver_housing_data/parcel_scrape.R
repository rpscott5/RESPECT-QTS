
library(plyr)
library(tidyverse)
library(rvest)
library(dplyr)

parc<-read.csv("Documents/qt-local/DenMet/Denver/parcels.csv",colClasses="character")
parc<-filter(parc,stringr::str_detect(D_CLASS_CN,"SFR|RESIDENTIAL\\-ROWHOUSE|RESIDENTIAL\\-DUPLEX|RESIDENTIAL-TRIPLEX"))
#pwb<-readRDS("Documents/qt-local/denmet/Denver/scratch/denverpermit_working_batch.rds")
pwb<-parc %>% as.data.frame() %>% select(SCHEDNUM) %>% mutate(SCHEDNUMCHAR=as.character(SCHEDNUM)) %>% unique()
parfiles<-list.files("Documents/qt-local/denmet/Denver/parcel_details/",full.names=F) %>% gsub(".rds","",.)
pwb<-filter(pwb, SCHEDNUMCHAR%in%parfiles==F)


for(i in 1:nrow(pwb)){
  try({
phtml<-rvest::read_html(paste0("https://www.denvergov.org/property/realproperty/summary/",pwb$SCHEDNUMCHAR[i]))
Sys.sleep(2)
phtml %>% rvest::html_table() %>% saveRDS(paste0("Documents/qt-local/denmet/Denver/parcel_details/",pwb$SCHEDNUMCHAR[i],".rds"))
rm(phtml)
})
}

parfiles<-list.files("Documents/qt-local/denmet/Denver/parcel_details/",full.names=T)
pardets<-parfiles %>% lapply(function(D) {
  d1<-readRDS(D) 
  if(length(d1)>1){
  d1<-d1 %>% .[[2]] 
  data.frame("style"=d1$X2[1],"year_effective"=d1$X2[3],"rooms"=d1$X2[2],"baths"=d1$X4[2])} else data.frame("style"=NA,"year_effective"=NA,"rooms"=NA,"baths"=NA)}) %>% bind_rows()
pardets$SCHEDNUMCHAR<-basename(parfiles) %>% gsub(".rds","",.)
head(pardets)
