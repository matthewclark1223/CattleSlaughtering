library(tidyverse)
library(sf)
library(ggspatial)
#General plotting things
MapTheme<-  theme(#panel.grid = element_blank(),
    axis.text = element_text(angle = 0,size=14,color="black"))+
  theme(legend.text=element_text(size=12),
        #legend.position = c(.1,.75),
        title = element_text(color="black",size=14,face="bold"),
        legend.title=element_text(size=14),
        panel.grid = element_blank(),
        legend.key.height = unit(1.5, 'cm'),
        legend.key.width = unit(0.33,"cm"))

#Slaughtering map
library(tidyverse)
library(sf)
dat<-read_csv("./Data/TimeseriesData.csv")
dat<-dat%>%filter(Date == "Jan 2015" )
SA<-read_sf("../General/CountryShapes/SA/gadm41_ZAF_1.shp")
SA$Province<-SA$NAME_1
dat<-merge(dat,SA,by=c("Province"))
dat<-st_as_sf(dat,crs=st_crs(SA))

p1<-ggplot(dat)+geom_sf(aes(fill=Slaughter))+
  scale_fill_viridis_c(name="",labels=scales::comma)+
  ggtitle("Cattle slaughter (Dec 2020)")+
  scale_y_continuous(breaks=c(-24,-28,-32))+
annotation_scale(location = "br", width_hint = 0.5,text_cex = 1)+
  #scale_fill_manual(values =c("white","green"))+
  theme_bw()+MapTheme

ggsave("./Figures/DataFigPanels/Slaughter2020.pdf",p1,dpi=350,units = "in",width = 6, height = 4)

#Nature dependant people 
p6<-ggplot(dat)+geom_sf(aes(fill=NDP_.livelihood))+
  scale_fill_distiller(name="",labels=scales::comma,palette ="Greens",direction=1)+
  #scale_fill_viridis_c(name="",labels=scales::comma,option="cividis")+
  ggtitle("Nature dependency (2015-2020)")+
  scale_y_continuous(breaks=c(-24,-28,-32))+
  #annotation_scale(location = "br", width_hint = 0.5,text_cex = 1.2)+
  #scale_fill_manual(values =c("white","green"))+
  theme_bw()+MapTheme

ggsave("./Figures/DataFigPanels/NDP2015_2020.pdf",p6,dpi=350,units = "in",width = 6, height = 4)



###
#Cattle pop 2020
library(raster)
Cows<-raster("../General/Cattle2020.tif")
CowsRescale<-raster("../General/Area10km.tif")
Cows[]<-Cows[]*CowsRescale[]

Cows<-crop(Cows,SA)
Cows<-mask(Cows,SA)

Cowsdf<-raster::as.data.frame(Cows,xy=T)
names(Cowsdf)[3]<-"Cattle"
Cowsdf<-na.omit(Cowsdf)
p4<-ggplot(SA)+geom_tile(data=Cowsdf,aes(x=x,y=y,fill=Cattle) )+
  scale_fill_viridis_c(option="plasma", direction = -1,
                       name="",labels=scales::comma)+
  geom_sf(fill=alpha("white",alpha=0.01),color="black")+
  ggtitle("Cattle population (2020)")+
  scale_y_continuous(breaks=c(-24,-28,-32))+
  xlab("")+ylab("")+
  #annotation_scale(location = "br", width_hint = 0.5,text_cex = 1.2)+
  #scale_fill_manual(values =c("white","green"))+
  theme_bw()+MapTheme

ggsave("./Figures/DataFigPanels/Cattle2020.pdf",p4,dpi=350,units = "in",width = 6, height = 4)


#Deprivation
Dep<-raster("../General/Deprivation/povmap-grdi-v1.tif")
Dep<-crop(Dep,SA)
Dep<-mask(Dep,SA)
Dep<-terra::resample(Dep,Cows,method="bilinear" ) 

Depdf<-raster::as.data.frame(Dep,xy=T)
names(Depdf)[3]<-"Deprivation"
Depdf<-na.omit(Depdf)
p3<-ggplot(SA)+geom_tile(data=Depdf,aes(x=x,y=y,fill=Deprivation) )+
  #scale_fill_viridis_c(option="plasma", direction = -1,
   #                    name="")+
  scale_fill_distiller(name="",palette="RdBu")+
  geom_sf(fill=alpha("white",alpha=0.01),color="black")+
  ggtitle("Deprivation (2010-2021)")+
  scale_y_continuous(breaks=c(-24,-28,-32))+
  xlab("")+ylab("")+
  #annotation_scale(location = "br", width_hint = 0.5,text_cex = 1.2)+
  #scale_fill_manual(values =c("white","green"))+
  theme_bw()+MapTheme

ggsave("./Figures/DataFigPanels/Dep.pdf",p3,dpi=350,units = "in",width = 6, height = 4)

###
#Dist to cities
Dcit <- raster("../General/dist2Cities.tif")
Dcit<-terra::crop(Dcit,SA)
#Dep[]<-ifelse(is.na(Dep[]),85,Dep[])
Dcit<-raster::mask(Dcit,SA) 
Dcit<-terra::resample(Dcit,Cows,method="bilinear" )
Dcit[]<-Dcit[]/1000

Dcitdf<-raster::as.data.frame(Dcit,xy=T)
names(Dcitdf)[3]<-"Distance"
Dcitdf<-na.omit(Dcitdf)
p5<-ggplot(SA)+geom_tile(data=Dcitdf,aes(x=x,y=y,fill=Distance) )+
  #scale_fill_viridis_c(option="plasma", direction = -1,
  #                    name="")+
  scale_fill_distiller(name="",palette="Purples",direction=1)+
  geom_sf(fill=alpha("white",alpha=0.01),color="black")+
  ggtitle("Distance to city (km)")+
  scale_y_continuous(breaks=c(-24,-28,-32))+
  xlab("")+ylab("")+
  #annotation_scale(location = "br", width_hint = 0.5,text_cex = 1.2)+
  #scale_fill_manual(values =c("white","green"))+
  theme_bw()+MapTheme

ggsave("./Figures/DataFigPanels/DCit.pdf",p5,dpi=350,units = "in",width = 6, height = 4)


#Rain
Rain_Dec_2020 <- raster("./Data/RainfallDec2020.tif")
Rain<-terra::crop(Rain_Dec_2020,SA) # crop the Rain raster to the extent defined by Bound
Rain<-raster::mask(Rain,SA) #NA everything outside .shp boundary
Rain[]<-ifelse(Rain[]<0,NA,Rain[]) 
#         values <0 are replaced with NA
#         ifelse(condition, yes, no)
Rain<-terra::resample(Rain,Cows,method="bilinear" )

Raindf<-raster::as.data.frame(Rain,xy=T)
names(Raindf)[3]<-"Rain"
Raindf<-na.omit(Raindf)
p2<-ggplot(SA)+geom_tile(data=Raindf,aes(x=x,y=y,fill=Rain) )+
  #scale_fill_viridis_c(option="plasma", direction = -1,
  #                    name="")+
  scale_fill_distiller(name="",palette="Blues",direction=1)+
  geom_sf(fill=alpha("white",alpha=0.01),color="black")+
  ggtitle("Total precipitation (mm; Dec 2020)")+
  scale_y_continuous(breaks=c(-24,-28,-32))+
  xlab("")+ylab("")+
  #annotation_scale(location = "br", width_hint = 0.5,text_cex = 1.2)+
  #scale_fill_manual(values =c("white","green"))+
  theme_bw()+MapTheme

ggsave("./Figures/DataFigPanels/RainDec2020.pdf",p2,dpi=350,units = "in",width = 6, height = 4)

#Combine

pc<-cowplot::plot_grid(p1,p2,p3,p4,p5,p6,nrow=2,align="hv")

ggsave("./Figures/AllDat.png",pc,dpi=350,units = "in",width = 15, height = 7.5, bg="white")

