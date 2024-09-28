library(tidyverse)
library(sf)

dat<-read_csv("./Data/TimeseriesData.csv")


library(brms)

###########


(prior<-get_prior(Slaughter ~rollsumSTD:DepSTD+rollsumSTD+DepSTD+NDPSTD+
                    DcitSTD+Month+(1|Province)+
                    offset(log(CowPop)),
                  data=dat,family="poisson"))

prior$prior[1]<-"cauchy(0,1)" #coefficient default
prior$prior[18]<-"cauchy(0,2.5)" #random intercept default
prior$prior[19]<-"exponential(1)" #ran intercept deviation default


fitInt4<-brm(Slaughter ~rollsumSTD:DepSTD+
               rollsumSTD+DepSTD+NDPSTD+DcitSTD+Month+(1|Province)+
               offset(log(CowPop)),data=dat,family="poisson",
             cores = 4,iter=10000,init=0,warmup=5000,prior = prior,
             control = list(max_treedepth = 15))


saveRDS(fitInt4, "AllParsMonth_NoPred.rds")




