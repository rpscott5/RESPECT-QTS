library(plyr)
library(dplyr)
pueb_parcels<-readRDS('Documents/dbc/qts_data/pueblo_parcels.rds') %>% as.data.frame()
parlim<-pueb_parcels %>% select(PAR_NUM,PAR_TXT,ImprovementsActualValue) %>% filter(ImprovementsActualValue>0) 
rm(pueb_parcels)
for(i in 1:nrow(parlim)){try({
  g1<-jsonlite::read_json(paste0("https://api.spatialest.com/v1/co/pueblo/buildings/",parlim$PAR_NUM[i]))
  g1 %>% saveRDS(.,paste0("Documents/qt-local/building_api_calls/",parlim$PAR_NUM[i],".rds"))})
  Sys.sleep(runif(1,.5,1))
}