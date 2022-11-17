#IQR
data = c(1,7,30,60,72,98,145,212,240,399,400,450,3000,3333,5000)
summary(data)

#Cook's Distance
mtcars
dim(mtcars)
head(mtcars)
str(mtcars)
model1 = lm(mpg ~ cyl + wt, data = mtcars)
model1

help("cooks.distance")
plot(model1, pch = 18, col = 'red', which = c(4))
#plot(model1, pch = 18, col = 'red', which = c(3))

cooks.distance(model1)
CooksDistance = cooks.distance(model1)

#Make data easier to read
round(CooksDistance, 5) #round values to 5 dp
sort(round(CooksDistance, 5))

#Outlier Detection using "Cooks Distance"
#Multivariate Regression using Cook's Distance
#Cook's Distance is an estimate of the influence if a data point, 
#Cook's Distance is a summary of how much a regression model changes when the ith observation is removed from the data
library(ISLR)
head(Hitters) #baseball hitters dataset
dim(Hitters)
is.na(Hitters)
HittersData = na.omit(Hitters)
dim(HittersData) #check dimensions of data set after removing NA values
glimpse(HittersData)
head(HittersData)

#Multivariate regression model using all features of the dataset to predict salary of the Baseball player
SalaryPredictModel1 = lm(Salary ~., data = HittersData)
summary(SalaryPredictModel1)
#Multiple R-squared:  0.5461,	Adjusted R-squared:  0.5106 

#Cook's Distance
cooksD = cooks.distance(SalaryPredictModel1)
influential = cooksD[(cooksD > (3*mean(cooksD, na.rm = TRUE)))]
influential

#18 players have a Cook's Distance greater than 3*mean
#Exclude players and rerun model to check for better fit in regression model
names_of_influential = names(influential)
names_of_influential
outliers = HittersData[names_of_influential,]
Hitters_Without_Outliers = HittersData %>% anti_join(outliers)

#Model 2: Without Outliers
SalaryPredictModel2 = lm(Salary ~., data = Hitters_Without_Outliers)
summary(SalaryPredictModel2)
#Multiple R-squared:  0.6721,	Adjusted R-squared:  0.6445 
#Improved Adjusted R-Squared from 0.5106 to 0.6445 with removal of 18 observations
