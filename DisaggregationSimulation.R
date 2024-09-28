
library(sf)
library(tidyverse)
SA<-read_sf("../General/CountryShapes/SA/gadm41_ZAF_2.shp")
BWA<-read_sf("../General/CountryShapes/BWA/gadm41_BWA_1.shp")

Bound<-rbind(SA,BWA)
ggplot(Bound)+geom_sf()

#Bound <- sf::st_buffer(Bound, dist = 0)
Bound<-Bound%>%filter(as.numeric(st_area(Bound))>5000000000)%>% #Remove small polys
  nngeo::st_remove_holes()


  ggplot(Bound)+geom_sf(aes(fill=NAME_1),show.legend = F)


  


#Make a simulated covariate Y
y <- raster::raster(Bound,crs=terra::crs(Bound),resolution=0.2 )
y[]<-rnorm(length(y[]),12,2)
y<-terra::mask(y,Bound)
terra::plot(y, colNA="#252525")

#rain covariate
Rain <- raster::raster("./TrialRain.tiff")
Rain<-terra::crop(Rain,Bound) #clip the bounding box
Rain<-raster::mask(Rain,Bound) #NA everything outside .shp boundary
Rain[]<-ifelse(Rain[]<0,NA,Rain[])
Rain<-terra::resample(Rain,y,method="bilinear" ) #Resample to target scale
terra::plot(Rain, colNA="#252525" )

#Deprivation covariate
Dep <- raster::raster("../General/Deprivation/povmap-grdi-v1.tif")
Dep<-terra::crop(Dep,Bound) #clip the bounding box
Dep[]<-ifelse(is.na(Dep[]),85,Dep[]) ###############Don't do this! Just removing NA's for testing!!
Dep<-raster::mask(Dep,Bound) #NA everything outside .shp boundary
Dep<-terra::resample(Dep,y,method="bilinear" ) #Resample to target scale
terra::plot(Dep, colNA="#252525" )


#Cattle population data as a population variable.
Cows<-raster::raster("../General/Cattle2015/5_Ct_2015_Da.tif")
Cows<-terra::crop(Cows,Bound) #clip the bounding box
Cows<-terra::mask(Cows,Bound) #NA everything outside .shp boundary
Cows<-terra::resample(Cows,y,method="bilinear" ) #Resample to target scale
Cows[]<-ceiling(Cows[])
terra::plot(Cows, colNA="#252525" )

#Load population
#Pop<-terra::rast("./LandScan/landscan-global-2022.tif")
#Pop<-terra::crop(Pop,Bound) #clip the bounding box
#Pop<-terra::mask(Pop,Bound) #NA everything outside .shp boundary

#PopAgg<-terra::resample(Pop,x,method="sum" ) #Resample to target scale

#If extracting by shapefile boundary
#PopBound<-terra::extract(x=Pop, y=Bound,fun=sum,na.rm=T)[2] #Make its own object
#Bound$Pop<-PopBound$`landscan-global-2022` #Add the vector to the shapefile
#ggplot(Bound)+geom_sf(aes(fill=Pop),show.legend = T) #Plot it.





#Now make some fake cattle sales data

mu<- 0 + 0.01*Dep[] -0.1*Rain[] #effects
mu<-plogis(mu) #get on probability scale
mu<-floor(mu*Cows[]) #Get on cow scale


Sales<-raster::raster(y) #container raster
Sales[]<-rpois(n=length(mu),mu)
plot(Sales[]~Dep[])
plot(Sales[]~Rain[])
terra::plot(Sales)

#Now aggregate to the shapefile

SalesBound<-terra::extract(x=Sales, y=Bound,fun=sum,na.rm=T) #Make its own object
Bound$Sales<-SalesBound #Add the vector to the shapefile

CowPopBound<-terra::extract(x=Cows, y=Bound,fun=sum,na.rm=T) #Make its own object
Bound$CowPop<-CowPopBound #Add the vector to the shapefile
Bound<-Bound%>%mutate(propSales=Sales/CowPop)
ggplot(Bound)+geom_sf(aes(fill=propSales)) #Plot it.


###
#Now try the disaggregation
library(disaggregation)

#Make terra objects for all.
Dep2<-terra::rast(Dep)
Rain2<-terra::rast(Rain)
Cows2<-terra::rast(Cows)

cov_stack<-terra::rast(list(Dep2,Rain2))
cov_stack<-raster::stack(list(Dep,Rain))

Bound2<-as(Bound, 'Spatial')

data_for_model <- disaggregation::prepare_data(polygon_shapefile = Bound2,
                               covariate_rasters = cov_stack,
                               aggregation_raster = Cows,
                               response_var = 'Sales', #From shapefile
                               id_var = 'NAME_1',
                               na.action = TRUE)

plot(data_for_model)

#model_result <- fit_model(data_for_model,
#                            iterations = 1000,
 #                           family = 'poisson',
 #                           link = 'log',
  #                          priors = list(priormean_intercept = 0,
    #                                      priorsd_intercept = 2,
      #                                    priormean_slope = 0.0,
     #                                     priorsd_slope = 0.4,
      #                                    prior_rho_min = 3,
       #                                   prior_rho_prob = 0.01,
           #                               prior_sigma_max = 1,
        #  #                                prior_sigma_prob = 0.01,
            #                              prior_iideffect_sd_max = 0.05,
           #                               prior_iideffect_sd_prob = 0.01))



model_result <- disag_model(data_for_model,
                            iterations = 100,
                            family = 'poisson',
                            link = 'log')
plot(model_result)

preds <- predict(model_result, 
                 N = 100, 
                 CI = 0.95)

plot(preds)



MeanPred<-preds[1]$mean_prediction$prediction

prop<-terra::rast(Sales/Cows)

validation<-terra::rast(list(MeanPred,prop))
terra::plot(validation,colNA="#252525" )

plot(MeanPred[]~Sales[])

     