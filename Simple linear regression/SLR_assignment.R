########## CALORIES CONSUMED ##########
# X = input variable is calories consume which is continous
# Y = output variable is weight gaine(grams) which is continous , so we proceed with SLR

cal_wt <- read.csv(file.choose())
View(cal_wt)

summary(cal_wt)

attach(cal_wt)

plot(cal_wt)

cor(Weight.gained..grams.,Calories.Consumed) #correlation factor

reg <- lm(Weight.gained..grams.~Calories.Consumed) #simple linear regression model

summary(reg)

pred <- predict(reg)
reg$residuals
mean(reg$residuals)
sum(reg$residuals)
sqrt(mean(reg$residuals^2)) #RMSE

confint(reg,level=0.95)
predict(reg,interval="predict")

#ggplot for adding regression line for data


ggplot(data = cal_wt,aes(x=Calories.Consumed,y=Weight.gained..grams.))+
  geom_point(color = "BLUE")+
  geom_line(color ="red",data = cal_wt, aes(x=Calories.Consumed,y=pred))

#logarithmic model
# X = log(calories.consumed)   Y = weight gained
plot(log(Calories.Consumed),Weight.gained..grams.)
cor(log(Calories.Consumed),Weight.gained..grams.)

reg_log <- lm(Weight.gained..grams.~log(Calories.Consumed))

summary(reg_log)
predict(reg_log)

reg_log$residuals
sqrt(sum(reg_log$residuals^2)/nrow(cal_wt))
confint(reg_log,level=0.95)
predict(reg_log,interval="confidence")

#Exponential model
# X = calories consumed Y = log(weight gained)
plot(Calories.Consumed,log(Weight.gained..grams.))
cor(Calories.Consumed,log(Weight.gained..grams.))

reg_exp <- lm(log(Weight.gained..grams.)~Calories.Consumed)
summary(reg_exp)

reg_exp$residuals

logwt <- predict(reg_exp)
wt <- exp(logwt)

error = cal_wt$Weight.gained..grams.-wt
error

sqrt(sum(error^2)/nrow(cal_wt))  #RMSE

confint(reg_exp,level = 0.95)
predict(reg_exp,interval = "confidence")


#Square root model
# X= sqrt(calories consumed), Y = weight gained

plot(sqrt(Calories.Consumed), Weight.gained..grams.)

cor(sqrt(Calories.Consumed),Weight.gained..grams.)

reg_sqrt <- lm(Weight.gained..grams.~sqrt(Calories.Consumed))
summary(reg_sqrt)

confint(reg_sqrt,level = 0.95)
predict(reg_sqrt,interval = "confidence")

####From above three model the first model "reg" have highest R squared value so it is best fit model

#-------------------------------------------------------------------------------#

#######DELIVERY TIME########


dl.tm <- read.csv(file.choose())
dt.st <- dl.tm
View(dt.st)

attach(dt.st)

summary(dt.st)

plot(Sorting.Time,Delivery.Time)

cor(Sorting.Time,Delivery.Time)
# If |r| is greater than  0.85 then Co-relation is Strong(Correlation Co-efficient = 0.8259973). 
# This has a moderate Correlation

#simple linear regression model without any transformations

reg <- lm(Delivery.Time~Sorting.Time)
summary(reg)

# The multiple-R-Squared Value is 0.6823 which is lesser than 0.8(In General)
# Adjusted R-Squared Value is 0.6655 
confint(reg,level = 0.95)
predict(reg,interval="predict")

# Adjusted R-squared value for the above model is 0.6655 

# we may have to do transformation of variables for better R-squared value
# Applying transformations

#logarithmic transformation

reg_log<-lm(Delivery.Time~log(Sorting.Time))  # Regression using logarthmic transformation
summary(reg_log)

confint(reg_log,level=0.95)

predict(reg_log,interval="confidence")

# Multiple R-squared value for the above model is 0.6954
# Adjusted R-squared:  0.6794 

# we may have to do different transformation for a better R-squared value
# Applying different transformations

#Exponential model

reg_exp <- lm(log(Delivery.Time)~Sorting.Time)
summary(reg_exp)

confint(reg_exp,level = 0.95)

exp(predict(reg_exp,interval="confidence"))

#R-squared value - 0.7109
# Adjusted R SQuare Value - 0.6957 
# Higher the R-sqaured value - Better chances of getting good model 
# for Delivery Time and Sorting Time

#Quadratic model
quad_mod <- lm(Delivery.Time~Sorting.Time+I(Sorting.Time^2),data=dt.st)
summary(quad_mod)

confint(quad_mod,level = 0.95)
predict(quad_mod,interval="predict")

# Adjusted R-Squared = 0.6594
#Multiple R -Squared Value = 0.6934

#Cubic model

poly_mod <- lm(Delivery.Time~Sorting.Time+I(Sorting.Time^2)+I(Sorting.Time^3),data=dt.st)
summary(poly_mod)

confint(poly_mod,level = 0.95)

predict(poly_mod,interval = "predict")


# Adjusted R-Squared = 0.6511
#Multiple R -Squared Value = 0.7034
# Exponential  model gives the highest Adjusted R-Squared value so it is the best fit model
predicted_Value <- exp(predict(reg_exp))
predicted_Value

ggplot(data = dt.st,aes(x=Sorting.Time,y=Delivery.Time))+
  geom_point(color = "BLUE")+
  geom_line(color ="red",data = dt.st, aes(x=Sorting.Time,y=predicted_Value))

##-----------------------------------------------------------------------------##

#####EMP DATA -> BUild a prediction model for churn out rate######

sh_cr <- read.csv(file.choose())
View(sh_cr)

attach(sh_cr)

plot(Salary_hike,Churn_out_rate)

summary(sh_cr)

cor(Salary_hike,Churn_out_rate)

# If |r| is greater than  0.85 then Co-relation is Strong(Correlation Co-efficient = -0.9117216). 
# This has a strong negative Correlation 

# Simple model without using any transformation
reg<-lm(Churn_out_rate~Salary_hike)
summary(reg)

# The multiple-R-Squared Value is 0.8312 which is greater than 0.8(In General)
# Adjusted R-Squared Value is 0.8101

confint(reg,level = 0.95)

predict(reg,interval = "predict")
pred <- predict(reg)
library(ggplot2)
ggplot(data = sh_cr,aes(x=Salary_hike,y=Churn_out_rate))+
  geom_point(color = "BLUE")+
  geom_line(color ="red",data = sh_cr, aes(x=Salary_hike,y=pred))

# Adjusted R-squared value for the above model is 0.8101 

# we may have to do transformation of variables for better R-squared value
# Applying transformations

# Logarthmic transformation
reg_log<-lm(Churn_out_rate~log(Salary_hike))  # Regression using logarthmic transformation
summary(reg_log)

confint(reg_log,level = 0.95)

predict(reg_log,interval = "predict")

pred_log <- predict(reg_log)

ggplot(data = sh_cr,aes(x=Salary_hike,y=Churn_out_rate))+
  geom_point(color = "BLUE")+
  geom_line(color ="red",data = sh_cr, aes(x=Salary_hike,y=pred_log))

# Multiple R-squared value for the above model is 0.8486
# Adjusted R-squared:  0.8297 

# we may have to do different transformation for a better R-squared value
# Applying different transformations

# Exponential model 
reg_exp<-lm(log(Churn_out_rate)~Salary_hike) # regression using Exponential model
summary(reg_exp)

confint(reg_exp,level = 0.95)
predict(reg_exp,interval = "predict")
 
pred_exp <- predict(reg_exp)

ggplot(data = sh_cr,aes(x=Salary_hike,y=Churn_out_rate))+
  geom_point(color = "BLUE")+
  geom_line(color ="red",data = sh_cr, aes(x=Salary_hike,y=pred_exp))

# Multiple R-squared value - 0.8735
# Adjusted R SQuare Value - 0.8577 
# Higher the R-sqaured value - Better chances of getting good model 
# for Delivery Time and Sorting Time

# Quadratic model
quad_mod <- lm(Churn_out_rate~Salary_hike+I(Salary_hike^2),data=sh_cr)
summary(quad_mod)

confint(quad_mod,level = 0.95)

predict(quad_mod,interval = "predict")

pred_quad <- predict(quad_mod)

ggplot(data = sh_cr,aes(x=Salary_hike,y=Churn_out_rate))+
  geom_point(color = "BLUE")+
  geom_line(color ="red",data = sh_cr, aes(x=Salary_hike,y=pred_quad))

# Adjusted R-Squared = 0.9662 
#Multiple R -Squared Value = 0.9737

# Cubic model
poly_mod <- lm(Churn_out_rate~Salary_hike+I(Salary_hike^2)+I(Salary_hike^3),data=sh_cr)
summary(poly_mod)

confint(poly_mod,level = 0.95)

predict(poly_mod,interval = "predict")

pred_poly <- predict(poly_mod)

ggplot(data = sh_cr,aes(x=Salary_hike,y=Churn_out_rate))+
  geom_point(color = "BLUE")+
  geom_line(color ="red",data = sh_cr, aes(x=Salary_hike,y=pred_poly))

# Adjusted R-Squared = 0.984
#Multiple R -Squared Value = 0.9893
# Cubic  model gives the best Adjusted R-Squared value


plot(poly_mod)

hist(residuals(poly_mod)) # it is close to normal distribution

#####-----------------------------------------------------------#####


###Salary_hike -> Build a prediction model for Salary_hike

ye_sh <- read.csv(file.choose())
View(ye_sh)
attach(ye_sh)

plot(YearsExperience,Salary)

boxplot(ye_sh)

summary(ye_sh)

cor(YearsExperience,Salary)

# If |r| is greater than  0.85 then Co-relation is Strong(Correlation Co-efficient = 0.9782416). 
# This has a strong Positive Correlation 

reg1 <- lm(Salary~YearsExperience)
summary(reg1)

# The multiple-R-Squared Value is 0.957 which is greater than 0.8(In General)
# Adjusted R-Squared Value is 0.9554

confint(reg1,level = 0.95)

predict(reg1,interval = "predict")

pred1 <- predict(reg1)

ggplot(data = ye_sh,aes(x=YearsExperience,y=Salary))+
  geom_point(color = "BLUE")+
  geom_line(color ="red",data = ye_sh, aes(x=YearsExperience,y=pred1))

# Adjusted R-squared value for the above model is 0.9554 

# we may have to do transformation of variables for better R-squared value
# Applying transformations

# Logarthmic transformation
reg_log<-lm(Salary~log(YearsExperience))  # Regression using logarthmic transformation
summary(reg_log)

confint(reg_log,level = 0.95)

predict(reg_log,interval = "predict")

pred_log1 <- predict(reg_log)

ggplot(data = ye_sh,aes(x=YearsExperience,y=Salary))+
  geom_point(color = "BLUE")+
  geom_line(color ="red",data = ye_sh, aes(x=YearsExperience,y=pred_log1))

# Multiple R-squared value for the above model is 0.8539
# Adjusted R-squared:  0.8487 

# we may have to do different transformation for a better R-squared value
# Applying different transformations

# Exponential model 
reg_exp<-lm(log(Salary)~YearsExperience) # regression using Exponential model
summary(reg_exp)

confint(reg_exp,level = 0.95)

predict(reg_exp,interval = "predict")
pred_exp <- predict(reg_exp)

ggplot(data = ye_sh,aes(x=YearsExperience,y=Salary))+
  geom_point(color = "BLUE")+
  geom_line(color ="red",data = ye_sh, aes(x=YearsExperience,y=pred_exp))
# Multiple R-squared value - 0.932
# Adjusted R SQuare Value - 0.9295 
# Higher the R-sqaured value - Better chances of getting good model 
# for Salary hike and Years of Experience

# Quadratic model
quad_mod <- lm(Salary~YearsExperience+I(YearsExperience^2),data=ye_sh)
summary(quad_mod)

confint(quad_mod,level = 0.95)

predict(quad_mod,interval = "predict")

pred_quad <- predict(quad_mod)

pred_quad

ggplot(data = ye_sh,aes(x=YearsExperience,y=Salary))+
  geom_point(color = "BLUE")+
  geom_line(color ="red",data = ye_sh, aes(x=YearsExperience,y=pred_quad))

# Adjusted R-Squared = 0.9538 
#Multiple R -Squared Value = 0.957

# Cubic model
poly_mod <- lm(Salary~YearsExperience+I(YearsExperience^2)+I(YearsExperience^3),data=ye_sh)
summary(poly_mod) # 0.9636

confint(poly_mod,level = 0.95)

predict(poly_mod,interval = "predict")

pred_poly <- predict(poly_mod)

pred_poly

ggplot(data = ye_sh,aes(x=YearsExperience,y=Salary))+
  geom_point(color = "BLUE")+
  geom_line(color ="red",data = ye_sh, aes(x=YearsExperience,y=pred_poly))
# Cubic  model gives the best Adjusted R-Squared value
pred_poly


rmse <- sqrt(mean((pred_poly-ye_sh$Salary)^2))
rmse
plot(poly_mod)


