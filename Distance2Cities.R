

library(sf)
library(tidyverse)
library(raster)



SA<- read_sf("../General/CountryShapes/SA/gadm41_ZAF_1.shp")
Cities<-read_sf("../General/SA_Cities/hotosm_zaf_populated_places_points_shp.shp")
Cities<-Cities%>%filter(place %in% c("city")) #Maybe do towns and cities??
st_crs(Cities)<-st_crs(SA)

Cities<-st_crop(Cities,SA)

ggplot(SA)+geom_sf()+geom_sf(data=Cities)


#Cattle data
Cows<-raster("../General/Cattle2015/5_Ct_2015_Da.tif")
Cows<-terra::crop(Cows,SA) 
Cows<-terra::mask(Cows,SA)
#Cows<-aggregate(Cows, fact=2,fun=sum)
#terra::plot(Cows, colNA="#252525" )

dist2Cities<-Cows

Cities<-Cities%>%filter(place %in% c("town","city"))

dist2Cities<-distanceFromPoints(dist2Cities, Cities)
dist2Cities<-terra::mask(dist2Cities,SA)
terra::plot(dist2Cities, colNA="#252525" )


raster::writeRaster(dist2Cities,"dist2Cities.tif")

?distanceFromPoints

