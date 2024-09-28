library(tidyverse)
library(sf)
Slaughtering<-read_sf("./Data/A - Slaughtering data_10.06.2024.csv")
Slaughtering<-Slaughtering%>%mutate_at(c(4:6),as.integer)
Slaughtering$Date<-gsub(" ","",Slaughtering$Date)
Slaughtering$Date<-zoo::as.yearmon(Slaughtering$Date,format = '%b_%y' )
Slaughtering<-Slaughtering%>%pivot_longer(cols = 4:6,values_to ="Slaughter",names_to = "Species")
SA<-read_sf("../General/CountryShapes/SA/gadm41_ZAF_1.shp")
SA$Province<-SA$NAME_1

#Fedele paper data
x<-read.csv("./Data/SA Provincial data_10.06.2024.csv",sep=";")
x<-x%>%select( NDP_.livelihood,Names)
x$Province<-x$Names
SA<-merge(SA,x,by="Province",all.x=T)

##


Month_Rain<-read_csv("./Data/rainfall_Province_Year.csv")

Month_Rain$Date<-zoo::as.yearmon(Month_Rain$Date)

Month_Rain<-Month_Rain %>% 
  arrange(Province, Date) %>%
  group_by(Province) %>%
  mutate(rollsum = RcppRoll::roll_sum(totalRainfall, 13, align = "right", fill = NA)-totalRainfall)%>%na.omit()
  


Month_Rain<-Month_Rain %>% 
  arrange(Province, Date) %>%
  group_by(Province) %>%
  mutate(rollsumPred = RcppRoll::roll_sum(totalRainfall, 13, align = "left", fill = NA)-totalRainfall)%>%na.omit()


#Cattle data
Cows<-raster::raster("../General/Cattle2015/5_Ct_2015_Da.tif")
Cows<-terra::crop(Cows,SA) #clip the SAing box
Cows<-terra::mask(Cows,SA) #NA everything outside .shp SAary
Cows[]<-ceiling(Cows[])
CowPopBound<-terra::extract(x=Cows, y=SA,fun=sum,na.rm=T) #Make its own object
SA$CowPop<-as.vector(CowPopBound) #Add the vector to the shapefile


#Deprevation data
Dep <- raster::raster("../General/Deprivation/povmap-grdi-v1.tif")
Dep<-terra::crop(Dep,SA)
Dep<-raster::mask(Dep,SA) 
Dep<-terra::extract(x=Dep, y=SA,fun=median,na.rm=T) #Make its own object
SA$Dep<-as.vector(Dep) 



#distance to cities
Dcit<-terra::rast("../General/dist2Cities.tif")
Dcit<-terra::crop(Dcit,SA)
Dcit<-raster::mask(Dcit,SA) 
Dcit<-terra::extract(x=Dcit, y=SA,fun=median,na.rm=T) #Make its own object
SA$dist2Cities<-as.data.frame(Dcit[2])
DcitDf<-as.data.frame(SA)%>%
  dplyr::select(Province,CowPop,Dep,dist2Cities,NDP_.livelihood)

DcitDf$dist2Cities<-as.vector(DcitDf$dist2Cities[,1])


Month_Rain<-Month_Rain%>%filter(Date>="Nov 2014")
Slaughtering<-Slaughtering%>%filter(Date>="Nov 2014")%>%
  filter(Species=="Cattle")
dat<-merge(Slaughtering,Month_Rain,by=c("Province","Date"))%>%na.omit()
dat<-merge(dat,DcitDf,all.x=T)
dat<-as_tibble(dat)

dat$Month<-substr(dat$Date,1,3)
stdize<-function(x){
  (x-mean(x,na.rm=T))/(2*sd(x,na.rm=T))}


dat$rollsumSTD<-stdize(dat$rollsum)
dat$rollsumPredSTD<-stdize(dat$rollsumPred)
dat$DepSTD<-stdize(dat$Dep)
dat$NDPSTD<-stdize(dat$NDP_.livelihood)
dat$DcitSTD<-stdize(dat$dist2Cities)

write.csv(dat,file = "./Data/TimeseriesData.csv")
