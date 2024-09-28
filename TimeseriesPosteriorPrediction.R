
ps<-plot(
  Fit,variable = c("STD"), regex = TRUE)

ggsave("./Figures/Traceplots.png",ps[[1]],dpi=350,units = "in",width = 8, height = 5, bg="white")


x<-density(posterior_predict(Fit,newdata = dat))
y<-density(dat$Slaughter)

hist(y,add=T,col="red")



dens <- density(mtcars$mpg)
# plot density
plot(x, frame = F, col = "steelblue", 
     main = "Within sample predictive capacity",
xlab="Provincial cattle slaughter")


lines(y,col="red")

legend(40000, 6e-05, legend=c( "Observed","Predicted"),
       col=c("red", "steelblue"), lty=1, cex=2,
        text.font=4)
