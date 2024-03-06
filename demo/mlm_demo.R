

library(tidyverse)
#using INLA
install.packages("INLA",repos=c(getOption("repos"),INLA="https://inla.r-inla-download.org/R/stable"), dep=TRUE)

library(INLA)

#read in data
sowsheat<-read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vTckJZ0y8YQdOAmORQsMP7Wn8cRIXep_KMnQPqWmwTOqn22mAf1ssSfSI5xIu6xKgBuEqzqFze4H4QX/pub?gid=1680009029&single=true&output=csv")

lmversion<-lm(noncondfurnace~1+
                #fixed portion of model (or pooled estimates)
                scale(as.numeric(RES_ABOVE_GRADE_AREA))+
                scale(log(APPRAISE_1+1))+
                scale(MED_HH_INCOME)+
                scale(perc.less.5)+
                scale(perc.hisp)+
                scale(prob_hispanic)+
                scale(perc.over.65)+
                scale(perc.bach)+
                scale(heatanomoly)+
                shortermrental+
                ac,data=sowsheat)

#this model looks pretty bad!
plot(lmversion)

glmversion<-glm(noncondfurnace~1+
                             #fixed portion of model (or pooled estimates)
                             scale(as.numeric(RES_ABOVE_GRADE_AREA))+
                             scale(log(APPRAISE_1+1))+
                             scale(MED_HH_INCOME)+
                             scale(perc.less.5)+
                             scale(perc.hisp)+
                             scale(prob_hispanic)+
                             scale(perc.over.65)+
                             scale(perc.bach)+
                             scale(heatanomoly)+
                             shortermrental+
                             ac, family=binomial(link="logit"),data=sowsheat)
plot(glmversion)
sowsheat<-filter(sowsheat, is.na(blockgroup)==F)


#some priors for our model
pc.prec1 = list(prec = list(prior = "pc.prec", param = c(.5, 0.01)))

#this code will fit the model (note, DV must be numeric)
modelfit<-inla(noncondfurnace~1+
              #fixed portion of model (or pooled estimates)
              scale(as.numeric(RES_ABOVE_GRADE_AREA))+
              scale(log(APPRAISE_1+1))+
              scale(MED_HH_INCOME)+
              scale(perc.less.5)+
              scale(perc.hisp)+
              scale(prob_hispanic)+
              scale(perc.over.65)+
              scale(perc.bach)+
              scale(heatanomoly)+
              shortermrental+
              ac+
              #Now, we sppecify the levels we want. Each level has a corresponding model that tells us about the structure of the random effect. 
              f(week, model="rw2", cyclic=T,hyper=pc.prec1)+ 
              #random walk order 2, estimate is informed by nearest 2 preceding neighbors.
              f(D_CLASS_CN, model="iid", hyper=pc.prec1)+
              #this is the most common type of random effect, it says all levels are independent draws from a distribution of observed and unobserved effects. Because there is no structure, levels with few observations will borrow more from levels with more observations.
              f(yb.decade, model="rw1", hyper=pc.prec1)+
              #this is a shorter version of the previous random walk
              f(blockgroup, model="iid", hyper=pc.prec1)+
              #this is a model for block groups, it probably should be a bym or car model but that adds complexity. For now, we use IID and assume each block group is independent.
              f(Contractor.s.Name, model="iid")+
              #this is a model for contractors
              f(SCHEDNUMCHAR, model="iid",hyper=pc.prec1)
            #this is a model for each parcel: note because we can observe one parcel have two permits, we add an effect to keep these parcels from driving our results.
            ,
            #modelling choices
            family="binomial",
            data=as.data.frame(sowsheat), 
            control.predictor = list(link = 1, compute = TRUE),
            control.compute=list(config = TRUE))

#we can now look at results 
#note i didnt really put much effort into priors here, these are pretty much default
summary(modelfit)
#note, the results has two sections, a fixed portion and a random portion.
#interpretation of fixed effects is straightforward, 

modelfit$summary.fixed %>% mutate(ID=rownames(.)) %>% filter(stringr::str_detect(ID,"Intercept")==F) %>% ggplot()+geom_pointrange(aes(x=ID,y=mean,ymin=`0.025quant`,ymax=`0.975quant`))+coord_flip()+geom_hline(yintercept=0,lty=2)+theme_minimal()+ylab("95% C.I.")+xlab("variable")


#compared to our glm model, not huge changes 
modelfit$summary.fixed %>% mutate(ID=rownames(.)) %>% ggplot()+geom_pointrange(aes(x=ID,y=mean,ymin=`0.025quant`,ymax=`0.975quant`))+coord_flip()+geom_hline(yintercept=0,lty=2)+theme_minimal()+ylab("95% C.I.")+xlab("variable")+geom_linerange(data=confint(glmversion) %>% as.data.frame() %>% mutate(ID=rownames(.)),aes(x=ID,ymin=`2.5 %`,ymax=`97.5 %`),colour="red")

#the random effect results are precisions, they tell us (generally) how much variation is explained by that random effect. 

#In this model, we can learn important things from the values of random effects themselves (ie, they are not just about properly accounting for the structure of data behind the pooled effects.)
#Is this driven by companies? maybe some of them
modelfit$summary.random$Contractor.s.Name %>% 
  filter(`0.025quant`>0|`0.975quant`<0) %>% 
  ggplot()+geom_pointrange(aes(x=ID,y=mean,ymin=`0.025quant`,ymax=`0.975quant`))+
  coord_flip()+
  theme_minimal()+
  geom_boxplot(data=modelfit$summary.random$Contractor.s.Name %>% filter(`0.025quant`<0 & `0.975quant`>0),aes(x="OTHER",y=mean))+
  geom_hline(yintercept=0,lty=2)

#notice, pretty temporal trends but smaller substantive impact on odds
modelfit$summary.random$week %>% 
  ggplot()+
  geom_pointrange(aes(x=ID,y=mean,ymin=`0.025quant`,ymax=`0.975quant`))+
  xlab("week")+
  theme_minimal()+
  geom_hline(yintercept=0,lty=2)

#we can also look at block group estimates
modelfit$summary.random$blockgroup %>% 
  ggplot()+
  geom_pointrange(aes(x=ID,y=mean,ymin=`0.025quant`,ymax=`0.975quant`))+
  geom_hline(yintercept=0,lty=2)

bgmap<-tigris::block_groups(state="CO",county="Denver")

bgmap %>% 
  left_join(.,modelfit$summary.random$blockgroup %>% mutate(GEOID=paste0("0",ID))) %>% 
  ggplot()+
  geom_sf(aes(fill=mean))+
  theme_minimal()+
  scale_fill_viridis_c()

#this model below is the probably better version of this model, that includes a spatially smoothed random effect for block groups

tf <- tempfile(fileext = ".adj")
download.file("https://raw.githubusercontent.com/rpscott5/RESPECT-QTS/main/denver_housing_data/scratch/denverbg.adj",tf)
inlaids<-read.csv("https://raw.githubusercontent.com/rpscott5/RESPECT-QTS/main/denver_housing_data/scratch/denverbgids.csv") %>% mutate(blockgroup=as.character(blockgroup)) %>% select(-X)
sowsheat$blockgroup<-as.character(sowsheat$blockgroup)
sowsheat<-sowsheat %>% left_join(.,inlaids)

pc.prec1 = list(prec = list(prior = "pc.prec", param = c(.5, 0.01)))

spacefit<-inla(noncondfurnace~1+
                 #fixed portion of model (or pooled estimates)
                 scale(as.numeric(RES_ABOVE_GRADE_AREA))+
                 scale(log(APPRAISE_1+1))+
                 scale(MED_HH_INCOME)+
                 scale(perc.less.5)+
                 scale(perc.hisp)+
                 scale(prob_hispanic)+
                 scale(perc.over.65)+
                 scale(perc.bach)+
                 scale(heatanomoly)+
                 shortermrental+
                 ac+
                 #Now, we sppecify the levels we want. Each level has a corresponding model that tells us about the structure of the random effect. 
                 f(week, model="rw2", cyclic=T,hyper=pc.prec1)+ 
                 #random walk order 2, estimate is informed by nearest 2 preceding neighbors.
                 f(D_CLASS_CN, model="iid", hyper=pc.prec1)+
                 #this is the most common type of random effect, it says all levels are independent draws from a distribution of observed and unobserved effects. Because there is no structure, levels with few observations will borrow more from levels with more observations.
                 f(yb.decade, model="rw1", hyper=pc.prec1)+
                 #this is a shorter version of the previous random walk
                 f(inlaid, model="bym2", graph=tf,hyper=pc.prec1)+
                 #this is a model for block groups, it probably should be a bym or car model but that adds complexity. For now, we use IID and assume each block group is independent.
                 f(Contractor.s.Name, model="iid")+
                 #this is a model for contractors
                 f(SCHEDNUMCHAR, model="iid",hyper=pc.prec1)
               #this is a model for each parcel: note because we can observe one parcel have two permits, we add an effect to keep these parcels from driving our results.
               ,
               #modelling choices
               family="binomial",
               data=as.data.frame(sowsheat), 
               control.predictor = list(link = 1, compute = TRUE),
               control.compute=list(config = TRUE))

bgmap<-tigris::block_groups(state="CO",county="Denver")

# the random effect result is twice as long as the number of block groups, with the second half of the observations containin the rho parameter from the smoothing. We just map mean estimates.

bgmap %>% 
  left_join(.,spacefit$summary.random$inlaid[1:571,] %>% 
              left_join(.,inlaids %>% rename(ID=inlaid) %>% mutate(GEOID=paste0("0",blockgroup)))) %>% 
  ggplot()+
  geom_sf(aes(fill=mean))+
  theme_minimal()+
  scale_fill_viridis_c()
spacefit$summary.random$inlaid[1:571,] %>% 
  ggplot()+
  geom_pointrange(aes(x=ID,y=mean,ymin=`0.025quant`,ymax=`0.975quant`),size=.1)+
  theme_minimal()




sowsheat$yb.decade.copy<-sowsheat$yb.decade

#One thing i wondered looking at results is if there might be a relationship between income and house age, so for example, in older homes, income has a stronger effect than in newer homes. (this is reaching, but i felt like we needed one random slope model).

modelfit.ranslope<-inla(noncondfurnace~1+
                 #fixed portion of model (or pooled estimates)
                 scale(as.numeric(RES_ABOVE_GRADE_AREA))+
                 scale(log(APPRAISE_1+1))+
                 scale(MED_HH_INCOME)+
                 scale(perc.less.5)+
                 scale(perc.hisp)+
                 scale(prob_hispanic)+
                 scale(perc.over.65)+
                 scale(perc.bach)+
                 shortermrental+
                  scale(heatanomoly)+
                 ac+
                 #Now, we specify the levels we want. Each level has a corresponding model that tells us about the structure of the random effect. 
                 f(yb.decade.copy,scale(MED_HH_INCOME), model="iid", hyper=pc.prec1)+ 
                 f(week, model="rw2", cyclic=T,hyper=pc.prec1)+ 
                 #random walk order 2, estimate is informed by nearest 2 preceding neighbors.
                 f(D_CLASS_CN, model="iid", hyper=pc.prec1)+
                 #this is the most common type of random effect, it says all levels are independent draws from a distribution of observed and unobserved effects. Because there is no structure, levels with few observations will borrow more from levels with more observations.
                 f(yb.decade, model="rw1", hyper=pc.prec1)+
                 #this is a shorter version of the previous random walk
                 f(blockgroup, model="iid", hyper=pc.prec1)+
                 #this is a model for block groups, it probably should be a bym or car model but that adds complexity. For now, we use IID and assume each block group is independent.
                 f(Contractor.s.Name, model="iid")+
                 #this is a model for contractors
                 f(SCHEDNUMCHAR, model="iid",hyper=pc.prec1)
               #this is a model for each parcel: note because we can observe one parcel have two permits, we add an effect to keep these parcels from driving our results.
               ,
               #modelling choices
               family="binomial",
               data=as.data.frame(sowsheat), 
               control.predictor = list(link = 1, compute = TRUE),
               control.compute=list(config = TRUE))

#our summary result is below. This should show slopes of household income at each build decade. Generally speaking, this supports the pooled effect we used previously. 
ggplot(modelfit.ranslope$summary.random$yb.decade.copy)+
  geom_pointrange(aes(x=ID,ymin=`0.025quant`,y=mean,ymax=`0.975quant`))+theme_minimal()
