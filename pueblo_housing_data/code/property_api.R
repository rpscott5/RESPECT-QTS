library(plyr)
library(dplyr)
pueb_parcels<-readRDS('Documents/dbc/qts_data/pueblo_parcels.rds') %>% as.data.frame()
parlim<-pueb_parcels %>% select(PAR_NUM,PAR_TXT,ImprovementsActualValue) %>% filter(ImprovementsActualValue>0) 
parlim<-parlim[which(parlim$PAR_NUM%in%as.numeric(list.files("Documents/qt-local/property_api_calls/") %>% gsub("\\.rds","",.))==F),]
for(i in nrow(parlim):1){try({
g1<-jsonlite::read_json(paste0("https://property.spatialest.com/co/pueblo/api/v1/recordcard/",parlim$PAR_NUM[i]))
g1 %>% saveRDS(.,paste0("Documents/qt-local/property_api_calls/",parlim$PAR_NUM[i],".rds"))})
Sys.sleep(runif(1,.75,1.25))
gc()
}
