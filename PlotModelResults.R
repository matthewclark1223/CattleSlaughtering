#Fit <- readRDS("./ModelObjects/AllParsMonth.rds")
library(brms)
library(tidyverse)

Fit<-readRDS("./ModelObjects/AllParsMonth_NoPred.rds")

dat<-read_csv("./Data/TimeseriesData.csv")



bayesplot::color_scheme_set("black")
p<-mcmc_plot(Fit, variable = c("STD"), regex = TRUE)+
  ggthemes::theme_clean()+
  geom_vline(xintercept=0,linetype=1,color="white")+
  geom_vline(xintercept=0,linetype=2,color="black")+
  scale_y_discrete(labels=c("Percipitation (12mo)","Deprivation","Nature\ndependence",
                            "Distance to\n urban center", "Interaction\n(Precipitation*deprivation)"))+xlab("Standardized coefficient estimate")+
  theme(axis.text.y  = element_text(face= "plain"),
        axis.title = element_text(color="black",size=20),
        axis.text=element_text(color="black",size=15))+ylab("Predictor")

ggsave("./Figures/Coefficients.pdf",p,dpi=350,units = "in",width = 8, height = 5, bg="white")


range(dat$DepSTD)

summary(Fit)


#p<-conditional_effects(Fit, effects = "rollsumSTD:DepSTD",
 #                   int_conditions = list(DepSTD = c(-0.5,0,0.5)),
  #                  spaghetti = T, 
   #                 ndraws = 200) #%>% 


library(tidybayes)

###If adding together all months. Not doing this bc it adds a ton of unnecessary uncertainty
#p<-Fit%>%
 # epred_draws(newdata = expand_grid(rollsumSTD = seq(min(dat$rollsumSTD),max(dat$rollsumSTD),by=0.05),
  #                                  DepSTD = c(-0.5,0,0.5),   #range(dat$DepSTD) = -1.1963354  0.5210309
  #                                  NDPSTD=0, #avg
   #                                 CowPop=mean(dat$CowPop),#avg
   #                                 Month = sample(unique(dat$Month),6),
    #                                Province = unique(dat$Province),#Marginalize across all
     #                               DcitSTD =0),
     #         ndraws=1000) #Avg



#p%>%group_by(rollsumSTD,DepSTD )%>%
# summarise(meddy=median(.epred),lower=quantile(.epred,0.05),upper=quantile(.epred,0.95))%>%ungroup()%>%
#  ggplot(.,aes(x=rollsumSTD, y=meddy))+
# geom_ribbon(aes(ymin=lower,ymax=upper,
#                 fill=as.character(DepSTD)),alpha=0.4)+
#  geom_line(aes(color=as.character(DepSTD)),linewidth=2)+
#  scale_fill_manual(values=c("#76B7B2","#4E79A7","#B07AA1"),
#                   labels=c("-0.5 (better off)","0",
#                           "0.5 (worse off)"),
#                  name="Deprivation")+
# scale_color_manual(values=c("#76B7B2","#4E79A7","#B07AA1"),
#                    labels=c("-0.5 (better off)","0",
#                            "0.5 (worse off)"),
#                  name="Deprivation")+
#scale_color_viridis_d(name="Deprivation",option="I")+
#scale_fill_viridis_d(alpha=0.4,name="Deprivation",option="I")+
# scale_y_continuous(labels = scales::comma)+
# scale_x_continuous(breaks=xbreaks,labels = scales::comma(xlabs))+
# xlab("Total rainfall past 12 months (mm)")+
# ylab("Monthly cattle slaughter")+
#ggthemes::theme_clean()+mytheme


#dat%>%group_by(Month)%>%summarise(sl=mean(Slaughter))%>%arrange(sl) #Get month closest to mean


############### #This works well but only gives 1 month. Going with closest to avg
p<-conditional_effects(Fit, effects = "rollsumSTD:DepSTD",conditions = list(Month="Oct"), 
                       int_conditions = list(DepSTD = c(-0.5,0,0.5)),#spaghetti = T,ndraws=200,
                       prob=0.9,robust=TRUE )


rainscale<-lm(rollsumSTD~rollsum,data=dat)
xlabs<-c(50000,100000,150000,200000)
xbreaks<-predict(rainscale,
                 newdata = data.frame(rollsum=xlabs))


mytheme<-theme(axis.text=element_text(color="black",size=18),
               axis.title=element_text(color="black",size=22),
               legend.text = element_text(size=16),
               legend.title = element_text(size=18),
               legend.key.size = unit(1, 'cm'),
               legend.background  = element_blank(),
               legend.box.spacing = margin(0.5),
               panel.border = element_blank())






p1<-p[[1]]%>%
  ggplot(.,aes(x=rollsumSTD, y=estimate__))+
  #geom_point(data=dat,aes(x=rollsumSTD,y=Slaughter),
   #          color="black",alpha=0.75)+
  geom_ribbon(aes(ymin=lower__,ymax=upper__,
                  fill=as.character(DepSTD)),alpha=0.4)+
  geom_line(aes(color=as.character(DepSTD)),linewidth=2)+
  scale_fill_manual(values=c("#76B7B2","#4E79A7","#B07AA1"),
                    labels=c("-1 (better off)","0",
                             "1 (worse off)"),
                    name="Deprivation (sd)")+
  scale_color_manual(values=c("#76B7B2","#4E79A7","#B07AA1"),
                     labels=c("-1 (better off)","0",
                              "1 (worse off)"),
                     name="Deprivation (sd)")+
  #scale_color_viridis_d(name="Deprivation",option="I")+
  #scale_fill_viridis_d(alpha=0.4,name="Deprivation",option="I")+
  scale_y_continuous(labels = scales::comma)+
  scale_x_continuous(breaks=xbreaks,labels = scales::comma(xlabs))+
  xlab("Total rainfall past 12 months (mm)")+
  ylab("Monthly cattle slaughter")+
  ggthemes::theme_clean()+mytheme


#change in slope fig
library(marginaleffects)

#pp<-avg_slopes(Fit, variables = "rollsumSTD",
 #          condition="rollsumSTD",
  #         conf_level = 0.9,
   #          newdata = datagrid("DepSTD"=seq(-0.5,0.5,0.01),
    #                            "Month"=unique(dat$M)),
     #      by="DepSTD")

#ggplot(pp) +
 # geom_ribbon(aes(DepSTD, ymin = conf.low, ymax = conf.high), alpha = .2) +
#  #geom_line(aes(DepSTD, estimate))+
 # xlab("deprivation")+
#  geom_hline(yintercept=0,color="#252525")+ggthemes::theme_clean()+
 # mytheme()



#Fit2 <- readRDS("./ModelObjects/AllPars.rds")

p2<-marginaleffects::plot_slopes(Fit, variables = "rollsumSTD",
condition = list("DepSTD"=seq(-0.5,0.5,0.05)),
                             conf_level = 0.9)+
  scale_y_continuous(labels = scales::comma)+
  scale_x_continuous(breaks=c(-0.5,0,0.5),limits = c(-0.55,.55),
                     labels=c("-1 sd\n(better off)", "Average", "+1 sd\n(worse off)"))+
  annotate(
    'curve',
    xend = -0.4, # Play around with the coordinates until you're satisfied
    yend = 10000,
    y = 8000,
    x = -0.1,
    linewidth = 1,
    curvature = 0.5,
    arrow = arrow(length = unit(0.5, 'cm'))
  )+
  annotate(
    'text',
    x = -0.1,
    y = 7500,
    label = 'Rain has a positive effect',
    fontface = 'bold', 
    size = 4.5
  ) +
  annotate(
    'curve',
    x = -0.4, # Play around with the coordinates until you're satisfied
    y = -6000,
    yend = -5000,
    xend = -0.1,
    linewidth = 1,
    curvature = 0.5,
    arrow = arrow(length = unit(0.5, 'cm'))
  )+
  annotate(
    'text',
    x = -0.4,
    y = -5000,
    label = 'Rain has a negetive effect',
    fontface = 'bold', 
    size = 4.5
  ) +
  xlab("Deprivation")+
  ylab("Slope estimate\n(slaughter/past rain)")+
  geom_hline(yintercept=0,color="#252525")+
  ggthemes::theme_clean()+mytheme



pc<-cowplot::plot_grid(p1,p2,nrow=1,align="hv",labels = c('A', 'B'),label_size = 22,label_fontface = "bold")

ggsave("./Figures/ConditionalEffects.png",pc,dpi=350,units = "in",width = 20, height = 5, bg="white")

pc<-cowplot::plot_grid(p1,p2,nrow=2,align="hv",labels = c('A', 'B'),label_size = 22,label_fontface = "bold")

ggsave("./Figures/ConditionalEffectsVert.png",pc,dpi=350,units = "in",width = 10, height = 10, bg="white")




##Get conditional expectations 

#This gives slope estimates. Switch Dep level
x<-marginaleffects::slopes(Fit, variables = "rollsumSTD",
                           newdata = datagrid(
                             DepSTD = 0.5,NDPSTD=0,
                               DcitSTD=0),
                           conf_level = 0.9)

x



#Gets expected slaughter values
p[[1]]%>%filter(rollsumSTD %in% c(min(rollsumSTD ),max(rollsumSTD )))







