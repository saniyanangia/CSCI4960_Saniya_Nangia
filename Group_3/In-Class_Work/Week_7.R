library(ISLR)
library(MASS)
library(boot)
set.seed(1)

??cv.glm #generalized linear model --> acts like lm function; has built-in function for cross-validation
help("sample")
train = sample(392, 196)

#Use subset option in lm() function to fit a linear regression using only observations in training set
lm.fit = lm(mpg~horsepower, data = Auto, subset = train)

#Now use predict() to estimate response for all observations
#Use mean() to calculate MSE of 196 observations in validation set. 
#[-train] selects only observations that are not in training set
attach(Auto)
head(Auto)
tail(Auto)
summary(Auto)
str(Auto)

mean((mpg-predict(lm.fit,Auto))[-train]^2)

#Quadratic regression line
lm.fit2 = lm(mpg~poly(horsepower,2), data = Auto, subset = train)
mean((mpg-predict(lm.fit2,Auto))[-train]^2)

#Cubic regression line
lm.fit3 = lm(mpg~poly(horsepower,3), data = Auto, subset = train)
mean((mpg-predict(lm.fit3,Auto))[-train]^2)

#Quartic regression line
lm.fit4 = lm(mpg~poly(horsepower,4), data = Auto, subset = train)
mean((mpg-predict(lm.fit4,Auto))[-train]^2)

#Quintic regression line
lm.fit5 = lm(mpg~poly(horsepower,5), data = Auto, subset = train)
mean((mpg-predict(lm.fit5,Auto))[-train]^2)

#Lower error = better!
#Error Rates: 23.26601, 18.71646, 18.79401, 19.16017, 19.40812

set.seed(2)
train = sample(392, 196)

#Regression
lm.fit = lm(mpg~horsepower, data = Auto, subset = train)
mean((mpg-predict(lm.fit, Auto))[-train]^2)

#Quadratic
lm.fit2 = lm(mpg~poly(horsepower,2), data = Auto, subset = train)
mean((mpg-predict(lm.fit2,Auto))[-train]^2)

#Cubic
lm.fit3 = lm(mpg~poly(horsepower,3), data = Auto, subset = train)
mean((mpg-predict(lm.fit3,Auto))[-train]^2)

#Quartic
lm.fit4 = lm(mpg~poly(horsepower,4), data = Auto, subset = train)
mean((mpg-predict(lm.fit4,Auto))[-train]^2)

#Quintic
lm.fit5 = lm(mpg~poly(horsepower,5), data = Auto, subset = train)
mean((mpg-predict(lm.fit5,Auto))[-train]^2)

#Error Rates: 25.72651, 20.43036, 20.38533, 20.30902, 19.79689

set.seed(3)
train = sample(392, 196)

#Regression
lm.fit = lm(mpg~horsepower, data = Auto, subset = train)
mean((mpg-predict(lm.fit, Auto))[-train]^2)

#Quadratic
lm.fit2 = lm(mpg~poly(horsepower,2), data = Auto, subset = train)
mean((mpg-predict(lm.fit2,Auto))[-train]^2)

#Cubic
lm.fit3 = lm(mpg~poly(horsepower,3), data = Auto, subset = train)
mean((mpg-predict(lm.fit3,Auto))[-train]^2)

#Quartic
lm.fit4 = lm(mpg~poly(horsepower,4), data = Auto, subset = train)
mean((mpg-predict(lm.fit4,Auto))[-train]^2)

#Quintic
lm.fit5 = lm(mpg~poly(horsepower,5), data = Auto, subset = train)
mean((mpg-predict(lm.fit5,Auto))[-train]^2)

#Error Rates: 21.99239, 17.70254, 17.71032, 17.62777, 17.09783

#Based on the validation set error rates (after splitting observations into training and validation sets), the model that predicts mpg using a quintic function of horsepower performs better
#There is little evidence in favor of using a cubic or quartic function of horsepower 


#Two types of cross-validation: LOOCV and K-fold

#K-fold Cross-Validation
#Use cv.glm()
#Set random seed
#Store CV errors corresponding to polynomial fits of orders 1 to 10; k = 10

??cv.glm()
set.seed(17)
help("rep")
cv.error.10 = rep(0,10)
for(i in 1:10){    #powers of 1 to 10 --> each time, 10 folds are evaluated
  glm.fit = glm(mpg ~ poly(horsepower, i), data = Auto)
  cv.error.10[i] = cv.glm(Auto, glm.fit, K = 10) $delta[1]
}
cv.error.10

#Computation time is much shorter than LOOCV
#There is little evidence that using cubic or higher-order polynomial terms leads to a lower test error/ better performance than using a quadratic fit


#Random Forest
install.packages("randomForest")
library(randomForest)
data1 = read.csv("/Users/saniyanangia/Desktop/CSCI_4960_Data_Analytics/DA_Files/car.csv", header = FALSE, stringsAsFactors=T)
head(data1)
colnames(data1) = c('BuyingPrice', 'Maintenance', 'NumDoors', 'NumPersons', 'BootSpace', 'Safety', 'Condition')
head(data1)
str(data1)

#Condition column has 4 levels - acc, good, unacc, vgood
levels(data1$Condition)
summary(data1)

#Create training and validation datasets
#Randomly choose 70% (0.7) of the data points for training and 30% (0.3) for validation
#Set the seed
set.seed(100)
train = sample(nrow(data1), 0.7*nrow(data1), replace = FALSE)
TrainSet = data1[train,]
ValidSet = data1[-train,]
summary(TrainSet)
summary(ValidSet)

#Random Forest Model with default parameters
help(randomForest)
model1 = randomForest(Condition ~ ., data = TrainSet, importance = TRUE)
model1
#OOB estimate of  error rate: 3.64%
#By default, number of trees is 500 and number of variables tried at each split is 2 in this case

#Finetuning parameters of RandomForest model
#Increase mtry from 2 to 6
#mtry - number of the variables ramndomly sampled as candidates at each split
#Note that default values are different for classification (sqrt(p) where p is number of variables in x) and regression (p/3)
model2 = randomForest(Condition ~ ., data = TrainSet, ntree = 500, mtry = 6, importance = TRUE)
model2
#OOB estimate of  error rate: 2.32%

#First conduct prediction using training set, after that do prediction using validation set
#We are doing this to oberve the differences in results
#Predicting on the training dataset
predTrain = predict(model2, TrainSet, type = 'class')
#Use table() to check classification accuracy
table(predTrain, TrainSet$Condition)

#Predicting on validation dataset
predValid = predict(model2, ValidSet, type = 'class')
table(predValid, ValidSet$Condition)

#We can also use importance() function to check important variables
#The below functions show the drop in mean accuracy for each of the variables
#Check important variables
importance(model2)
varImpPlot(model2)

#Now, use 'for' loop and check for different values of mtry
#Using a 'for' loop to identify the right mtry for the model
a = c()
i = 5
for(i in 3:8) {
  model3 = randomForest(Condition ~ ., data = TrainSet, ntree = 500, mtry = i, importance = TRUE)
  predValid = predict(model3, ValidSet, type = 'class')
  a[i-2] = mean(predValid == ValidSet$Condition)
}
a
plot(3:8, a)

#Compare performance of model with decision tree 
install.packages("caret")
library(rpart)
library(caret)
library(e1071)

model_dt = train(Condition ~ ., data = TrainSet, method = 'rpart')
model_dt_1 = predict(model_dt, data = TrainSet)
table(model_dt_1, TrainSet$Condition)
mean(model_dt_1 == TrainSet$Condition)
#On the training dataset, the accuracy is around 81.3% (0.8130687) and there is a lot of misclassification

#Validation set
model_dt_vs = predict(model_dt, newdata = ValidSet)
table(model_dt_vs, ValidSet$Condition)
mean(model_dt_vs == ValidSet$Condition)
