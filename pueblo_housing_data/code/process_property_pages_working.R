do1<-list.files("Documents/qt-local/property_pages/",full.names = T)

ht1e<-lapply(do1,function(X) {try({read_html(X) %>% html_table })})
names(ht1e)<-basename(do1) %>% gsub(".html","",.)

improvements<-sapply(ht1e,function(demo){try({
colnames(demo[[1]])[1]<-"var"
"IMP"%in%demo[[1]]$var})
})
ht1e<-ht1e[names(which(improvements=="TRUE"))]
ht1e<-ht1e[sapply(ht1e, length)>=7]

ht1e[[4]][[3]]
ht1b<-lapply(ht1e, function(X) {X[[2]] %>% pivot_wider(names_from=`Detail Type`,values_from=c(Detail,Count),values_fn = ~paste(.x,sep=";",collapse=";"))}) 
ht1b<-ht1b %>% bind_rows(.id="PAR_NUM")

demo1<-lapply(do1,function(X) {try({
  demo<-read_html(X) 
  yb<-demo %>% html_nodes("#improvements .col-12") %>% html_text() %>% stringr::str_extract(.,"Year Built[0-9]+") %>% stringr::str_extract(.,"[0-9]+") %>% unique() %>% na.omit()  %>% as.numeric()
  yr<-demo %>% html_nodes("#improvements .col-12") %>% html_text() %>% stringr::str_extract(.,"Year Remodeled[0-9]+") %>% stringr::str_extract(.,"[0-9]+") %>% unique() %>% na.omit()  %>% as.numeric()
  st1<-demo %>% html_nodes("#improvements .col-12") %>% html_text() %>% stringr::str_extract(.,"Style[\\w ]+") %>% gsub("Style","",.) %>% unique() %>% na.omit()  %>% as.character()
  br1<-demo %>% html_nodes("#improvements .col-12") %>% html_text() %>% stringr::str_extract(.,"Bedrooms[0-9]+") %>% stringr::str_extract(.,"[0-9]+") %>% unique() %>% na.omit()  %>% as.numeric()
  br2<-demo %>% html_nodes("#improvements .col-12") %>% html_text() %>% stringr::str_extract(.,"Full Baths[0-9]+") %>% stringr::str_extract(.,"[0-9]+") %>% unique() %>% na.omit()  %>% as.numeric()
  br3<-demo %>% html_nodes("#improvements .col-12") %>% html_text() %>% stringr::str_extract(.,"Half Baths[0-9]+") %>% stringr::str_extract(.,"[0-9]+") %>% unique() %>% na.omit()  %>% as.numeric()
cbind("year_built"=yb,"year_remodelled"=yr,"style"=st1,"bedrooms"=br1,"baths"=br2,"hbaths"=br3)
})})
names(demo1)<-basename(do1) %>% gsub(".html","",.)
demo1<-lapply(demo1,as.data.frame)
demo2<-bind_rows(demo1,.id="PAR_NUM")

  

#improvements .col-12


ht1b$`Detail_Heating/Cooling` %>% stringr::str_split(";") %>% unlist() %>% table()

ht1e %>% html_node(".img-fluid") %>% html_attr("src")
