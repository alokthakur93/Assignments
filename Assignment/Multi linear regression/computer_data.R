Computer_data <- read.csv(file.choose())
View(Computer_data)
class(Computer_data)


attach(Computer_data)
sum(is.na(Computer_data))#no na values are present
library(psych)
describe(Computer_data)

library(plyr)
Computer_data1 <- Computer_data
Computer_data1$cd <- as.numeric(revalue(Computer_data1$cd,c("yes"=1, "no"=0)))
Computer_data1$multi <- as.numeric(revalue(Computer_data1$multi,c("yes"=1, "no"=0)))
Computer_data1$premium <- as.numeric(revalue(Computer_data1$premium,c("yes"=1, "no"=0)))
View(Computer_data1)
class(Computer_data1)
attach(Computer_data1)

summary(Computer_data1)

pairs(Computer_data1)

cor(Computer_data1)

library(corpcor)
cor2pcor(cor(Computer_data1))

#The linear model of intrest with all variables
Model.Computer <- lm(price~speed+hd+ram+screen+cd+multi+premium+ads+trend)
summary(Model.Computer)  # Adjusted R2 Value - 0.7752

### Scatter plot matrix along with Correlation Coefficients
panel.cor<-function(x,y,digits=2,prefix="",cex.cor)
{
  usr<- par("usr"); on.exit(par(usr))
  par(usr=c(0,1,0,1))
  r=(cor(x,y))
  txt<- format(c(r,0.123456789),digits=digits)[1]
  txt<- paste(prefix,txt,sep="")
  if(missing(cex.cor)) cex<-0.4/strwidth(txt)
  text(0.5,0.5,txt,cex=cex)
}
pairs(Computer_data1,upper.panel = panel.cor,main="Scatter plot matrix with Correlation coefficients")

library(mvinfluence)
library(car)
# Applying VIF function on model built on all inputs
## Variance Inflation factor to check collinearity b/n variables
vif(Model.Computer)
## vif>10 then there exists collinearity among all the variables 
## Added Variable plot to check correlation b/n variables and o/p variable
avPlots(Model.Computer,id.n=2,id.cex=0.7)

# It is Better to delete influential observations rather than deleting entire column which is 
# costliest process
# Deletion Diagnostics for identifying influential observations

influence.measures(Model.Computer)

## plotting Influential measures 
influenceIndexPlot(Model.Computer, id.n=3)# index plots for infuence measures
influencePlot(Model.Computer, id.n=3) # A user friendly representation of the above

#Regression model after deleting the 1441th and 1701th observation which are influential observations.
Model.Computer1<-lm(price~speed+hd+ram+screen+cd+multi+premium+ads+trend,
                         data=Computer_data1[-c(1441,1701),])
summary(Model.Computer1) #R-squared value = 0.7774

#Applying transformation for getting better R-squared value

# Logarthimic Transformation 
Model.Computer_dataLog <- lm(price~log(speed)+log(hd)+log(ram)+log(screen)+
                               log(cd)+log(multi)+log(premium)+log(ads)+log(trend)
                             ,data=Computer_data1[-c(1441,1701),])
summary(Model.Computer_dataLog)      ## Adjusted R2 Value - 0.7441


# Exponential Transformation :
Model.Computer_exp<-lm(log(price)~speed+hd+ram+screen+cd+multi+premium+ads+trend,
                       data=Computer_data1[-c(1441,1701),])
summary(Model.Computer_exp)  #Adjusted R2 Value is 0.7833


# Quad Model
Model.Computer_Quad <- lm(price~speed+I(speed^2)+hd+I(hd^2)+ram+I(ram^2)+screen+I(screen^2)+
                            +cd+I(cd^2)+multi+I(multi^2)+premium+I(premium^2)
                          +ads+I(ads^2)+trend+I(trend^2),data=Computer_data1[-c(1441,1701),])
summary(Model.Computer_Quad)  #Adjusted R2 value is 0.8049



# Poly Model
Model.Computer_Poly <- lm(price~speed+I(speed^2)+I(speed^3)+
                            hd+I(hd^2)+I(hd^3)+
                            ram+I(ram^2)+I(ram^3)+
                            screen+I(screen^2)+I(screen^3)+
                            cd+I(cd^2)+I(cd^3)+
                            multi+I(multi^2)+I(multi^3)+
                            premium+I(premium^2)+I(premium^3)+
                            ads+I(ads^2)+I(ads^3)+
                            trend+I(trend^2)+I(trend^3),data=Computer_data1[-c(1441,1701),])
summary(Model.Computer_Poly) #Adjusted R Square Value is 0.813

#Final Model

finalmodel <- Model.Computer_Poly
summary(finalmodel)
predict(finalmodel,interval = "predict")

plot(finalmodel)

qqPlot((finalmodel),id.n=5) #Normal QQ-plot helps in identifying outliers

hist(residuals(finalmodel)) #close to normal distribution

