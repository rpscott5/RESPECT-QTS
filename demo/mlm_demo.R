

library(tidyverse)
#using INLA
install.packages("INLA",repos=c(getOption("repos"),INLA="https://inla.r-inla-download.org/R/stable"), dep=TRUE)

library(INLA)

#read in data
sowsheat<-read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vTckJZ0y8YQdOAmORQsMP7Wn8cRIXep_KMnQPqWmwTOqn22mAf1ssSfSI5xIu6xKgBuEqzqFze4H4QX/pub?gid=1680009029&single=true&output=csv")

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
#inteperetation of fixed effects is straightforward, 

modelfit$summary.fixed %>% mutate(ID=rownames(.)) %>% filter(stringr::str_detect(ID,"Intercept")==F) %>% ggplot()+geom_pointrange(aes(x=ID,y=mean,ymin=`0.025quant`,ymax=`0.975quant`))+coord_flip()+geom_hline(yintercept=0,lty=2)+theme_minimal()+ylab("95% C.I.")+xlab("variable")



modelfit$summary.fixed %>% mutate(ID=rownames(.)) %>% ggplot()+geom_pointrange(aes(x=ID,y=mean,ymin=`0.025quant`,ymax=`0.975quant`))+coord_flip()+geom_hline(yintercept=0,lty=2)+theme_minimal()+ylab("95% C.I.")+xlab("variable")+geom_linerange(data=confint(glmversion) %>% as.data.frame() %>% mutate(ID=rownames(.)),aes(x=ID,ymin=`2.5 %`,ymax=`97.5 %`),colour="red")


#Is this driven by companies? maybe some of them
modelfit$summary.random$Contractor.s.Name %>% filter(`0.025quant`>0|`0.975quant`<0) %>% ggplot()+geom_pointrange(aes(x=ID,y=mean,ymin=`0.025quant`,ymax=`0.975quant`))+coord_flip()+theme_minimal()+geom_boxplot(data=test3$summary.random$Contractor.s.Name %>% filter(`0.025quant`<0 & `0.975quant`>0),aes(x="OTHER",y=mean))+geom_hline(yintercept=0,lty=2)

#notice, pretty temporal trends but smaller substantive impact on odds
modelfit$summary.random$week %>% ggplot()+geom_pointrange(aes(x=ID,y=mean,ymin=`0.025quant`,ymax=`0.975quant`))+xlab("week")+theme_minimal()+geom_hline(yintercept=0,lty=2)

modelfit$summary.random$blockgroup %>% ggplot()+geom_pointrange(aes(x=ID,y=mean,ymin=`0.025quant`,ymax=`0.975quant`))+geom_hline(yintercept=0,lty=2)



modelfit$summary.fitted.values$mean





test3$summary.fixed %>% mutate(ID=rownames(.)) %>% filter(stringr::str_detect(ID,"Intercept")==F) %>% ggplot()+geom_pointrange(aes(x=ID,y=mean,ymin=`0.025quant`,ymax=`0.975quant`))+coord_flip()+geom_hline(yintercept=0,lty=2)+theme_minimal()

