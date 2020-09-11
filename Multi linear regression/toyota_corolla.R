Corolla <- read.csv(file.choose())
View(Corolla)
sum(is.na(Corolla)) #no na values present
attach(Corolla) 
Corolla_Pred <- cbind(Price,Age_08_04,KM,HP,cc,Doors,Gears,Quarterly_Tax,Weight)
Corolla_Pred1 <- as.data.frame(Corolla_Pred)
class(Corolla_Pred1)

View(Corolla_Pred1)
attach(Corolla_Pred1)

library(psych)
describe(Corolla_Pred1)

summary(Corolla_Pred1)

pairs(Corolla_Pred1)

cor(Corolla_Pred1)

library(corpcor)
cor2pcor(cor(Corolla_Pred1))

#The linear model with all variables
corolla.price <- lm(Price~Age_08_04+KM+HP+cc+Doors+Gears+Quarterly_Tax+Weight)
summary(corolla.price) 

corolla.price2 <-lm(Price~Age_08_04+KM+HP+Gears+Quarterly_Tax+Weight)
summary(corolla.price2) 


### Scatter plot matrix with Correlations inserted in graph
panel.cor <- function(x, y, digits=2, prefix="", cex.cor)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r = (cor(x, y))
  txt <- format(c(r, 0.123456789), digits=digits)[1]
  txt <- paste(prefix, txt, sep="")
  
  if(missing(cex.cor)) cex <- 0.4/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex)
}
pairs(Corolla_Pred1, upper.panel=panel.cor,main="Scatter Plot Matrix with Correlation Coefficients")

library(mvinfluence)
library(car)
# Applying VIF function on model built on all inputs
## Variance Inflation factor to check collinearity b/n variables
vif(corolla.price)
## vif>10 then there exists collinearity among all the variables 
## Added Variable plot to check correlation b/n variables and o/p variable
avPlots(corolla.price,id.n=2,id.cex=0.7)

# It is Better to delete influential observations rather than deleting entire column which is 
# costliest process
# Deletion Diagnostics for identifying influential observations

influence.measures(corolla.price)

## plotting Influential measures 
influenceIndexPlot(corolla.price, id.n=3)# index plots for infuence measures
influencePlot(corolla.price, id.n=3) # A user friendly representation of the above

## Regression after deleting the 81st observation, which is influential observation
corolla.price1 <- lm(Price~Age_08_04+KM+HP+cc+Doors+Gears+Quarterly_Tax+Weight,data=Corolla_Pred1[-81,])
summary(corolla.price1)
#By deleting influential there is no change in r-squared value so previous model is better
#than deleting values

finalmodel <- corolla.price
summary(finalmodel)

predict(finalmodel,interval="predict")

pred_final <- predict(corolla.price)
Final <- cbind(Price,pred_final,Age_08_04,KM,HP,cc,Doors,Gears,Quarterly_Tax,Weight)
View(Final)

plot(finalmodel)
qqPlot(finalmodel, id.n=5) # QQ plots helps identify outliers








