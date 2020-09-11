startups <- read.csv(file.choose())
View(startups)

#Exploratory Data Analysis
attach(startups)
sum(is.na(startups))#no na values are present
#Measures of Central Tendency
mean(R.D.Spend)
mean(Administration)
mean(Marketing.Spend)
mean(Profit)

median(R.D.Spend)
median(Administration)
median(Marketing.Spend)
median(Profit)

library(NCmisc)
Mode(R.D.Spend)
Mode(Administration)
Mode(Marketing.Spend)
Mode(Profit)

#Measures of Dispersion

var(R.D.Spend)
var(Administration)
var(Marketing.Spend)
var(Profit)

sd(R.D.Spend)
sd(Administration)
sd(Marketing.Spend)
sd(Profit)

range(R.D.Spend)
range(Administration)
range(Marketing.Spend)
range(Profit)

#Third buisness moments
library(moments)
skewness(R.D.Spend)
skewness(Administration)
skewness(Marketing.Spend)
skewness(Profit)

#Fourth buisness moment

kurtosis(R.D.Spend)
kurtosis(Administration)
kurtosis(Marketing.Spend)
kurtosis(Profit)

#Graphical representation

boxplot(R.D.Spend,horizontal = TRUE)
boxplot(Administration,horizontal = TRUE)
boxplot(Marketing.Spend,horizontal = TRUE)
boxplot(Profit,horizontal = TRUE)

hist(R.D.Spend)
hist(Administration)
hist(Marketing.Spend)
hist(Profit)

str(startups)

library(psych)
describe(startups)

#Probability distributions of variables

qqnorm(R.D.Spend)
qqline(R.D.Spend)
qqnorm(Administration)
qqline(Administration)
qqnorm(Marketing.Spend)
qqline(Marketing.Spend)
qqnorm(Profit)
qqline(Profit)

#Creating Dummy variables

library(plyr)

startups$State <- revalue(startups$State,
                          c("New York"="0", "California"="1", "Florida"="2"))
attach(startups)
Startups <- cbind(RD_Spend=R.D.Spend,Administration,Marketing_Spend=Marketing.Spend,State,Profit)


Startups1 <- as.data.frame(Startups)

attach(Startups1)
summary(Startups1)
pairs(Startups1)
cor(Startups1)

library(corpcor)
cor2pcor(cor(Startups1))
# The Linear Model of interest with all the columns
Model.Startups <- lm(Profit~RD_Spend+Administration+Marketing_Spend+State)
summary(Model.Startups)

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
pairs(Startups1,upper.panel = panel.cor,main="Scatter plot matrix with Correlation coefficients")

library(mvinfluence)
library(car)  
# Applying VIF function on model built on all inputs
## Variance Inflation factor to check collinearity b/n variables
vif(Model.Startups)
## vif>10 then there exists collinearity among all the variables 
## Added Variable plot to check correlation b/n variables and o/p variable
avPlots(Model.Startups,id.n=2,id.cex=0.7)
# It is Better to delete influential observations rather than deleting entire column which is 
# costliest process
# Deletion Diagnostics for identifying influential observations
influence.measures(Model.Startups)
## plotting Influential measures 
influenceIndexPlot(Model.Startups, id.n=3)# index plots for infuence measures
influencePlot(Model.Startups, id.n=3) # A user friendly representation of the above

# Regression after deleting the 49th and 50th observation, which is influential observation
Model.Startups_Fin1<-lm(Profit~RD_Spend+Administration+Marketing_Spend+State,data=Startups1[-c(49,50),])
summary(Model.Startups_Fin1) #adjusted r-squared value = 0.9593


#logarithmic transformations

Model.Startups_Log<-lm(Profit~RD_Spend+log(Administration)+Marketing_Spend+log(State),data=Startups1[-c(49,50),]) 

summary(Model.Startups_Log) #Adjusted R2 Value = 0.9591  

confint(Model.Startups_Log,level=0.95)

predict(Model.Startups_Log,interval="predict")
# Exponential Transformation :
Model.Startups_exp<-lm(log(Profit)~RD_Spend+Administration+Marketing_Spend+State,data=Startups1[-c(49,50),])
summary(Model.Startups_exp)  #Adjusted R2 Value is 0.9182


# Quad Model
Model.Startups_Quad <- lm(Profit~RD_Spend+I(RD_Spend^2)+Administration+I(Administration^2)
                          +Marketing_Spend+I(Marketing_Spend^2)+State+I(State^2),data=Startups1[-c(49,50),])
summary(Model.Startups_Quad)  #Adjusted R2 value is 0.9567

### Variance Inflation Factors is a formal way to check for collinearity
vif(Model.Startups_Log)  # VIF is > 10 => collinearity
avPlots(Model.Startups_Log, id.n=2, id.cex=0.7)# Added Variable Plots

# Final Model
FinalModel<-Model.Startups_Log

summary(FinalModel) #Adjusted R2 Value = 0.9591 
predict(FinalModel,interval="predict")
plot(FinalModel)
#R-squared value table
rsquared_value <-  matrix(c(0.9464,0.9593,0.9591,0.9182,0.9567,0.9591),ncol=1,byrow =TRUE)
colnames(rsquared_value) <- c("R-squared values")
rownames(rsquared_value) <- c("original model","model without influential values","log model","exponential model","quad model","Final model")
rsquared_value <- as.table(rsquared_value)
rsquared_value
