library(ggspatial)

ggplot(SL)+
  geom_sf(aes(fill=propSales))+
  scale_fill_viridis_c(name="Proportion\nslaughtered")+
  #scale_size(range = c(10,35)) +
  annotation_scale(location = "br", width_hint = 0.5,text_cex = 1.2)+
  #scale_fill_manual(values =c("white","green"))+
  theme_bw()+theme(#panel.grid = element_blank(),
    axis.text = element_text(angle = 0,size=16,color="black"))+
  theme(legend.text=element_text(size=12),
        #legend.position = c(.1,.75),
        legend.title=element_text(size=16),
        legend.key.size = unit(1, 'cm'))






Preds_df <- as.data.frame(MeanPred, xy = TRUE)
Preds_df<-na.omit(Preds_df)

ggplot()+
  geom_raster(data = Preds_df , aes(x = x, y = y, fill = sum)) +
  scale_fill_viridis_c(name="Proportion\nslaughtered")+
  xlab(label="")+ylab(label="")+
  #scale_fill_manual(values =c("white","green"))+
  theme_bw()+theme(#panel.grid = element_blank(),
    axis.text = element_text(angle = 0,size=16,color="black"))+
  theme(legend.text=element_text(size=12),
        legend.title=element_text(size=16),
        legend.key.size = unit(1, 'cm'))



####
library(ggspatial)
p1<-ggplot(SL)+
  geom_sf(aes(fill=Cattle))+
  scale_fill_viridis_c(name="Cows\nslaughtered")+
  #scale_size(range = c(10,35)) +
  annotation_scale(location = "br", width_hint = 0.5,text_cex = 1)+
  #scale_fill_manual(values =c("white","green"))+
  theme_bw()+theme(#panel.grid = element_blank(),
    axis.text = element_text(angle = 0,size=16,color="black"))+
  theme(legend.text=element_text(size=12),
        #legend.position = c(.1,.75),
        legend.title=element_text(size=16),
        legend.key.size = unit(1, 'cm'))



PredsTotal<-MeanPred
PredsTotal[]<-PredsTotal[]*Cows[]

Preds_df2 <- as.data.frame(PredsTotal, xy = TRUE)
Preds_df2<-na.omit(Preds_df2)

p2<-ggplot()+
  geom_sf(data=SL,color=alpha("white",0.001),fill=alpha("white",0.001))+
  geom_raster(data = Preds_df2 , aes(x = x, y = y, fill = layer)) +
  scale_fill_viridis_c(name="Cows\nslaughtered",labels=scales::comma)+
  xlab(label="")+ylab(label="")+
  #scale_fill_manual(values =c("white","green"))+
  theme_bw()+theme(#panel.grid = element_blank(),
    axis.text = element_text(angle = 0,size=16,color="black"))+
  theme(legend.text=element_text(size=12),
        legend.title=element_text(size=16),
        legend.key.size = unit(1, 'cm'))

dp<-cowplot::plot_grid(p1,p2,nrow=1,align="hv",labels=c("A","B"),label_size = 18)


ggsave("./Figures/DisaggPanels.pdf",dp,dpi=350,units = "in",width = 16, height = 6 ,bg="white")
ggsave("./Figures/DisaggPanels.png",dp,dpi=350,units = "in",width = 16, height = 6 ,bg="white")

dpv<-cowplot::plot_grid(p1,p2,nrow=2,align="hv",labels=c("A","B"),label_size = 18)
ggsave("./Figures/DisaggPanelsVert.pdf",dpv,dpi=350,units = "in",width = 8, height = 12 ,bg="white")
ggsave("./Figures/DisaggPanelsVert.png",dpv,dpi=350,units = "in",width = 8, height = 12 ,bg="white")


#####
#sup figs#

sp<-plot(model_result)
sp[[2]]
ggsave("./Figures/DisAggPostPred.pdf",sp[[2]],dpi=350,units = "in",width = 6, height = 5 ,bg="white")
ggsave("./Figures/DisAggPostPred.png",sp[[2]],dpi=350,units = "in",width = 6, height = 5 ,bg="white")


####
#slope coefficient
Ests<-summary(model_result)$model_params[2:5]

Vars<-summary(model_result)$model_params[10:13]

slpz<-data.frame(Ests,Vars,Par=c("Deprivation","Rain\n(2019 total)","Dist. to\nurban","Interaction\nDep*Rain"))

pds<-ggplot(slpz, aes(x = Par, y = Ests)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = Ests - Vars , ymax = Ests +Vars), 
                width = 0.2) +
  coord_flip() +
  theme_minimal() +
  geom_hline(yintercept=0,color="red",linetype="dashed",linewidth=1)+
  labs(y = "Estimate", x = "Parameter") +theme_bw()+
  theme(axis.text = element_text( color="black",size = 18),axis.title=element_text( color="black",size = 22))

ggsave("./Figures/DisAggSlopes.pdf",pds,dpi=350,units = "in",width = 6, height = 5 ,bg="white")
ggsave("./Figures/DisAggSlopes.png",pds,dpi=350,units = "in",width = 6, height = 5 ,bg="white")
