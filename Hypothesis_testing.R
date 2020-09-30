############Cutlets dataset##############

cutlets <- read.csv(file.choose())
attach(cutlets)
#
#############Normality test###############

shapiro.test(Unit.A)
# p-value = 0.32 >0.05 so p high null fly => It follows normal distribution

shapiro.test(Unit.B)
# p-value = 0.5225 >0.05 so p high null fly => It follows normal distribution

#############Variance test###############

var.test(Unit.A,Unit.B)#variance test
# p-value = 0.3136 > 0.05 so p high null fly => Equal variances

############2 sample T Test ##################

t.test(Unit.A,Unit.B,alternative = "greater",var.equal = T)

# Null Hypothesis -> mean diameter of unit A = mean diameter of unit B
# Alternative Hypothesis -> mean diameter of unit A not equal to mean diameter of unit B
# p-value = 0.2361> 0.05 => p high null fly => accept null hypothesis
# There is no significance difference between the diameter of the cutlet between two units.

#----------------------LABTAT DATASET----------------------#

LabTAT <- read.csv(file.choose())
View(LabTAT)
#Input = descrete with more than 2 variable
#Output = continous
#That implies we must go for ANOVA test
Stacked <- stack(LabTAT)
attach(Stacked)
View(Stacked)

#############Normality test###############

shapiro.test(LabTAT$Laboratory.1)
shapiro.test(LabTAT$Laboratory.2)
shapiro.test(LabTAT$Laboratory.3)
shapiro.test(LabTAT$Laboratory.4)
#In all above cases p>0.05 accept null hypothesis that is data is normal

library(car)
leveneTest(values~ ind, data = Stacked)
#p=0.05161 > 0.05 accept null hypothesis that is variences are equal
Anova_results <- aov(values~ind,data = Stacked)
summary(Anova_results)

#NULL Hypothesis -> all mean are equal 
#Alternate Hypothesis -> atleast one is different
#here p<0.05, p low null go , accept alternate hypothesis
# yes there is difference in average TAT among the different laboratories.

#----------------------------BUYER'S RATIO DATASET--------------------###########
buyers <- read.csv(file.choose())
View(buyers)
stack <- stack(buyers)
attach(stack)
table(values,ind)
t2 <- prop.table(table(values))
t1 <- table(ind)

chisq.test(table(values,ind))
#p = 0.2931 > 0.05 accept null hypothesis i.e; all means are equal
#That implies proportion of male & female buyers is same across all the region

#--------------------------Customerorderform dataset-----------------###########
customer <- read.csv(file.choose(),stringsAsFactors=FALSE)
View(customer)
stacked_customer <- stack(customer)
View(stacked_customer)
attach(stacked_customer)
chisq.test(values,ind)
#here p = 0.2771 > 0.05 accept null hypothesis i.e; all means are equal
#The defective percentage doesnot varies by the centres

##---------------------------FANTALOONS dataset-----------------########
fantaloons <- read.csv(file.choose())
View(fantaloons)
attach(fantaloons)
table<- table(Weekdays,Weekend)
table
prop.test(x=c(66,47),n=c(233,167),conf.level = 0.95,correct = FALSE,alternative = "two.sided")
# p-value = 0.9681 < 0.05 accept null hypothesis i.e.
# equal proportions 







