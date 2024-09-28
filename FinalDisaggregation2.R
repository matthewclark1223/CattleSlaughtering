library(sf)
library(tidyverse)
library(sp)


library(INLA)
library(raster)



# 1. Data loading and cleaning --------------------------------------------

SA<- read_sf("../General/CountryShapes/SA/gadm41_ZAF_1.shp")




Bound <- SA
#Bound<-Bound%>%filter(NAME_1!="Gauteng")

Bound<-Bound%>%filter(as.numeric(st_area(Bound))>5000000000)%>% #Remove small polys
  nngeo::st_remove_holes()

ggplot(Bound)+geom_sf(aes(fill=NAME_1),show.legend = F)



#Cattle data 2020
Cows<-raster("../General/Cattle2020.tif") #Load density
CowsRescale<-raster("../General/Area10km.tif") #Multiply by area to get total number 
Cows[]<-Cows[]*CowsRescale[]
Cows<-terra::crop(Cows,Bound) 
Cows<-terra::mask(Cows,Bound)
Cows[]<-ceiling(Cows[]) # "ceiling()" rounds each element of the raster up to the nearest integer >= to that element 
Cows<-aggregate(Cows, fact=2.5,fun=sum)
terra::plot(Cows, colNA="#252525" )


# 4. Rain Covariates  -----------------------------------------------------
Rain_2019 <- raster("./Data/averaged_rainfall_2019.tif")


## Rain_2019
Rain_2019 <-terra::crop(Rain_2019,Bound) # crop the Rain raster to the extent defined by Bound
Rain_2019 <-raster::mask(Rain_2019,Bound) #NA everything outside .shp boundary
Rain_2019[]<-ifelse(Rain_2019[]<0,NA,Rain_2019[]) 
#         values <0 are replaced with NA
#         ifelse(condition, yes, no)
Rain_2019<-terra::resample(Rain_2019,Cows,method="bilinear" )
#         resamples Rain to match the spatial resolution and alignment of y using bilinear interpolation
#         "resample" adjusts the resolution and alignment of a raster to match another raster
#         bilinear interpolation: computes the new pixel value by taking a weighted average of the surrounding 4 pixels in the input raster
#                                 provides smooth transition between pixel values - commonly used for continuous data
terra::plot(Rain_2019, colNA="#252525" )



# 5. Deprivation Index covariate ------------------------------------------

#Removing NAs....JUST FOR TESTING
Dep <- raster("../General/Deprivation/povmap-grdi-v1.tif")
Dep<-terra::crop(Dep,Bound)
#Dep[]<-ifelse(is.na(Dep[]),85,Dep[])
Dep<-raster::mask(Dep,Bound) 
Dep<-terra::resample(Dep,Cows,method="bilinear" ) 
terra::plot(Dep, colNA="#252525" )

#Distance to cities
Dcit <- raster("../General/dist2Cities.tif")
Dcit<-terra::crop(Dcit,Bound)
#Dep[]<-ifelse(is.na(Dep[]),85,Dep[])
Dcit<-raster::mask(Dcit,Bound) 
Dcit<-terra::resample(Dcit,Cows,method="bilinear" ) 
terra::plot(Dcit, colNA="#252525" )

#Make interaction term
Dep_Rain_Int<-Dep
Dep_Rain_Int[]<-Dep[]*Rain_2019[]
names(Dep_Rain_Int)<-"Dep_Rain_Int"

# 7. Cattle Slaughtering Data ---------------------------------------------
CattleSlaugh <- read_delim("./Data/A - Slaughtering data_10.06.2024.csv", delim = ";")

CattleSlaugh_2020 <- CattleSlaugh %>%
  filter(grepl("_20$", Date))

# spatial join to add spatial information to CSV dataset

# match name of Province Column 
Bound$Province <- Bound$NAME_1

colnames(CattleSlaugh_2020)[1]<-"Province"

# Grouped by province (pivot for the merge) & sum all months 
CattleSL_2020 <- CattleSlaugh_2020 %>% 
  group_by(Province) %>%
  summarise(Cattle = sum(Cattle),
            Sheep = sum(Sheep),
            Pigs = sum(Pigs))

SL <- merge(Bound, CattleSL_2020, by = "Province", all.x = TRUE)


###Remove Gauteng bc >100% slaughter. Maybe rethink this!

#SL<-SL%>%filter(Province!="Gauteng")
####

ggplot(SL)+
  geom_sf(aes(fill=Cattle),show.legend = T)

# Add Cattle pop numbers per province to the SL dataset
CowPopBound<-terra::extract(x=Cows, y=SL, fun=sum,na.rm=T) #Make its own object
SL$CowPop<-CowPopBound #Add the vector to the shapefile

# Calculate Cattle Slaughtering/Pop proportion
CattleSlaugh_2020$Cattle<-as.numeric(CattleSlaugh_2020$Cattle)
#CattleSlaugh_2015$Sheep<-as.numeric(CattleSlaugh_2020$Sheep)
#CattleSlaugh_2015$Pigs<-as.numeric(CattleSlaugh_2015$Pigs)

SL <- SL %>%
  mutate(propSales = Cattle/CowPop) 

ggplot(SL)+
  geom_sf(aes(fill=propSales))+
  scale_fill_viridis_c()


# 8. Disaggregation -------------------------------------------------------

library(disaggregation)

#Make terra objects for all.
stdize<-function(x){
  (x-mean(x,na.rm=T))/(2*sd(x,na.rm=T))}

Dep[] <- stdize(Dep[])
Rain_2019[] <- stdize(Rain_2019[])
Dcit[] <- stdize(Dcit[])
Dep_Rain_Int[]<-stdize(Dep_Rain_Int[])


cov_stack<-raster::stack(list(Dep, Rain_2019,Dcit,Dep_Rain_Int )) #raster stack of all the covariates (same scale and boundaries)

Dep2 <- terra::rast(Dep)
Rain2_2019 <- terra::rast(Rain_2019)
Cows2 <- terra::rast(Cows)
Dcit2 <- terra::rast(Dcit)
Dep_Rain_Int2<-terra::rast(Dep_Rain_Int)

cov_stack2<-terra::rast(list(Dep2, Rain2_2019, Dcit2,Dep_Rain_Int2))

terra::plot(cov_stack2)
#################


##Some versions want different raster/shapefile formats. Add/remove 2's from data as necessary

SL2<-as(SL, 'Spatial') # ?? what's this for?

data_for_model <- disaggregation::prepare_data(polygon_shapefile = SL2, 
                                               covariate_rasters = cov_stack,
                                               aggregation_raster = Cows,
                                               response_var = 'Cattle', #From shapefile
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

?disag_model

model_result <- disag_model(data_for_model,
                            iterations = 1000,
                            family = 'poisson',
                           priors=list(priormean_slope = -.5, #weakly infomative priors from timeseries model
                            priorsd_slope = 0.5),
                            link = 'log')
plot(model_result)

View(model_result)

preds <- predict(model_result, 
                 N = 100, 
                 CI = 0.95)

plot(preds)

MeanPred<-preds[1]$mean_prediction$prediction 
########
#let's transform the data into a dataframe
Preds_df <- as.data.frame(MeanPred, xy = TRUE)


# If you're familiar with ggplot, we can plot this dataframe version with
#those packages
ggplot()+
  geom_raster(data = Preds_df , aes(x = x, y = y, fill = sum)) +
  scale_fill_viridis_c()
##########################

MeanPred<-preds[1]$mean_prediction$prediction 
str(preds)


