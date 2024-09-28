library(tidyverse)
library(sf)
Slaughtering<-read_sf("./A - Slaughtering data_10.06.2024.csv")
Slaughtering<-Slaughtering%>%mutate_at(c(4:6),as.integer)
Slaughtering$Date<-gsub(" ","",Slaughtering$Date)
Slaughtering$Date<-zoo::as.yearmon(Slaughtering$Date,format = '%b_%y' )

SA<-read_sf("./CountryShapes/SA/gadm41_ZAF_1.shp")
SA$Province<-SA$NAME_1

dat<-merge(SA,Slaughtering, by="Province",all.x=T)%>%
  dplyr::select(Province,geometry,Date)

library(chirps)


subDat<-dat[1,] #clip to only the month and province 
START<-as_date(subDat$Date) #First of the month
END<-as_date(subDat$Date+0.1) #first of NEXT month

SAVect<-terra::vect(subDat)

dt <- get_chirps(SAVect, c(START,END),server="CHC",resolution=0.25) #"2008-11-01","2024-04-01"


dt<-terra::crop(dt,subDat) #clip the SAing box
dt<-terra::mask(dt,subDat) #NA everything outside .shp SAary

dt[]<-ifelse(dt[]==-9999,NA,dt[])

#Get some indexes for rainfall. 
subDat$totalRainfall<-sum(dt[],na.rm=T)
subDat$averageRainfall<-mean(dt[],na.rm=T)
subDat$stddevRainfall<-sd(dt[],na.rm=T)

b<-brick(dt)
maxes<-sapply(raster::unstack(b), function(r){max(values(r),na.rm=T)})
subDat$wetDays<-length(which(maxes>=1))
subDat$SDII<-subDat$totalRainfall/subDat$wetDays

ChirpsContainer<-subDat

for(i in 2:nrow(dat)){
  subDat<-dat[i,] #clip to only the month and province 
  START<-as_date(subDat$Date) #First of the month
  END<-as_date(subDat$Date+0.1) #first of NEXT month
  
  SAVect<-terra::vect(subDat)
  
  dt <- get_chirps(SAVect, c(START,END),server="CHC",resolution=0.25) #"2008-11-01","2024-04-01"
  
  
  dt<-terra::crop(dt,subDat) #clip the SAing box
  dt<-terra::mask(dt,subDat) #NA everything outside .shp SAary
  
  dt[]<-ifelse(dt[]==-9999,NA,dt[])
  
  #Get some indexes for rainfall. 
  subDat$totalRainfall<-sum(dt[],na.rm=T)
  subDat$averageRainfall<-mean(dt[],na.rm=T)
  subDat$stddevRainfall<-sd(dt[],na.rm=T)
  
  b<-brick(dt)
  maxes<-sapply(raster::unstack(b), function(r){max(values(r),na.rm=T)})
  subDat$wetDays<-length(which(maxes>=1))
  subDat$SDII<-subDat$totalRainfall/subDat$wetDays
  
  ChirpsContainer<-rbind(ChirpsContainer,subDat)
  
  print(i/nrow(dat)*100)
}

x<-as.data.frame(ChirpsContainer)%>%dplyr::select(!geometry)

write.csv(x,file="rainfall_Province_Year.csv")






