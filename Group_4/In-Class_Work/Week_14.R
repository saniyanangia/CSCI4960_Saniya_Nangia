#LOESS, LOWESS, Logistic Regression, LDA

#Economics Dataset
data(economics, package = "ggplot2") #load data
View(economics)
economics$index = 1:nrow(economics) #create index variable
economics = economics[1:80, ] #80 rows for better graphical understanding
loessMod10 = loess(uempmed ~ index, data = economics, span = 0.10) #10% smoothing span
loessMod25 = loess(uempmed ~ index, data = economics, span = 0.25) #25% smoothing span
loessMod50 = loess(uempmed ~ index, data = economics, span = 0.50) #50% smoothing span

#predict LOESS
smoothed10 = predict(loessMod10)
smoothed25 = predict(loessMod25)
smoothed50 = predict(loessMod50)

#as span increases, smoothing of the curve also increases
plot(economics$uempmed, x = economics$date, type = "l", main = "LOESS Smoothing and Prediction", xlab = "Date", ylab = "Unemployment (Median)")
lines(smoothed10, x = economics$date, col = "red")
lines(smoothed25, x = economics$date, col = "green")
lines(smoothed50, x = economics$date, col = "blue")
legend("bottomright", legend = c("0.10 span", "0.25 span", "0.50 span"), col = c("red", "green", "blue"), lty = 1, cex = 0.8)


#Cars Dataset
#fitting a curve to the data 
#local regression/ local polynomial regression = moving average
#generalization of moving average and polynomial regression
#one of the most common methods initially developed for scatterplot smoothing
#2 types - LOESS and LOWESS
#using LOWESS for the Cars dataset here

data("cars")
str(cars)
#50 observations of 2 variables

plot(speed ~ dist, data = cars)
#positive relationship between both variables

help("lowess")
lowess(cars$speed ~ cars$dist)
#use lowess() with line() function to draw the lines
lines(lowess(cars$speed ~ cars$dist, f = 2/3), col = "blue")
#f value is the smoother span. f = 2/3 = 0.666
#default value for smoother span is 0.666 in RStudio

#f gives proportion of points in the plot that influence the smoothing at each value
#larger values = more smoothing
#changing the f value and observing the shape of the line
lines(lowess(cars$speed ~ cars$dist, f = 0.75), col = "gray") #f = 0.75
lines(lowess(cars$speed ~ cars$dist, f = 0.8), col = "red") #f = 0.8
lines(lowess(cars$speed ~ cars$dist, f = 0.9), col = "green") #f = 0.9
lines(lowess(cars$speed ~ cars$dist, f = 0.1), col = 5) #f = 0.1
lines(lowess(cars$speed ~ cars$dist, f = 0.01), col = 6) #f = 0.01
#Very low values for f ==> overfitting


#Logistic Regression
library(ISLR)
data("Smarket")
head(Smarket)
#percentage returns for S&P500 stock index over 1250 days (2001 - 2005)
names(Smarket)
#for each date, record percentage returns for 5 previous trading days (Lag1 to Lag5)
#Volume = no. of shares traded on previous day (in billions)
#Today = percentage return on the date
#Direction = whether market was up or down on this date

dim(Smarket)
summary(Smarket)
cor(Smarket[, -9]) #omit "Direction" (last column)
#cor() function produces a matrix containing all the pairwise correlations among the predictors in a dataset.
#cor(Smarket) gives an error message since "Direction" variable is qualitative
#so omit "Direction" (last column)

#correlation between lag variables and today's returns are close to 0
#little correlation between today's returns and previous days' returns
#substantial correlation between Year and Volume only

attach(Smarket)
plot(Volume)

#logistic Regression to predict "Direction" using 'Lag1' to 'Lag5' and 'Volume'
help("glm")
#glm() is similar to lm(), except we pass arg family = binomial for logistic regression
#instead of some other type of generalized linear model
glm.fit.model1 = glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume, data = Smarket, family = binomial)
summary(glm.fit.model1)

#smallest p-value is associated with Lag1
#negative coefficient for this predictor suggests and if market has positive return yesterday, it is less likely to go up
#but at value of 0.145, p-value is still relatively large
#therefore, there is no clear evidence of a real association between "Lag1" and "Direction"

#predict() to find probability that market will go up given the predictors
#type = "response" --> output probabilities of the form P(Y = 1|X)
#instead of to other information like the logit
#if no dataset is supplied to predict(), then probabilities are computed for training data used to fit logistic regression model
#for the 10 values printed, values correspond to probability of the market going up (not down)
#because contrasts() indicates that R has created a dummy variable with 1 for Up
glm.probs = predict(glm.fit.model1, type = "response")
glm.probs[1:10]
contrasts(Direction)

#to predict whether market will go up or down on a particular day, convert the predicted probabilities into "Up" or "Down" class labels
#create a vector of class predictions based on whether the predicted probability of a market increase is greater than or less than 0.5 
help("rep") #"rep" replicates elements of vectors
#create a vector of 1,250 "Down" elements
glm.pred = rep("Down", 1250)
#transform all of the elements for which the predicted probability of a market increase exceeds 0.5 to "Up" 
glm.pred[glm.probs > 0.5] = "Up" 

#table() produces a confusion matrix in order to determine how many observations were correctly or incorrectly classified
table(glm.pred, Direction)

#model correctly predicted that the market would go up on 507 days 
#model correctly predicted that the market would go down on 145 days
#total correct predictions = 507 + 145 = 652 
#percentage correct = (507+145) / 1250 = 0.5216

#mean() to compute fraction of days for which prediction was correct
mean(glm.pred == Direction)
#0.5216
#logistic regression correctly predicted the movement of the market 52.2% of the time

#result is misleading because model was trained and tested on the same 1250 observations
#training error rate = 100 − 52.2 = 47.8%
#overly optimistic and underestimates the test error rate

#instead, create a vector corresponding to the observations from 2001 through 2004
#create a held out dataset of observations from 2005
train = (Year < 2005)
Smarket.2005 = Smarket[!train,] 
dim(Smarket.2005) #252 observations
Direction.2005 = Direction[!train]
#train = boolean vector of 1250 elements
#elements of vector corresponding to observations before 2005 = TRUE
#elements of vector corresponding to observations in 2005 = FALSE

#eg to pick out submatrix of stock market data set (corresponding only to dates before 2005)
#Smarket[train,]

#!train is a vector similar to train
#except elements that are TRUE in train get swapped to FALSE in !train
#and elements that are FALSE in train get swapped to TRUE in !train
#Smarket[!train,]

#fit a logistic regression model using only subset of observations corresponding to dates before 2005
#obtain predicted probabilities of stock market going up for each day in test set
#ie days in 2005
glm.fit.model2 = glm(Direction ~ Lag1+Lag2+Lag3+Lag4+Volume, data = Smarket, family = binomial, subset = train)
glm.prob2 = predict(glm.fit.model2, Smarket.2005, type = "response")

#compute predictions for 2005 and compare them to actual movements of market over that time period
glm.pred2 = rep("Down", 252)
glm.pred2[glm.prob2 > 0.5] = "Up"
table(glm.pred2, Direction.2005)
mean(glm.pred2 == Direction.2005) #accuracy rate
mean(glm.pred2 != Direction.2005) #error rate
#test set error rate = 0.52 (worse than random guessing)

#logistic regression model had very underwhelming p-values associated with all predictors
#the smallest p-value, though not very small, corresponded to Lag1
#remove variables that are not helpful in predicting Direction
#as these predictors increase variance without a corresponding decrease in bias

#refit  logistic regression using just Lag1 and Lag2,
#which have the highest predictive power in the original model
glm.fit.model3 = glm(Direction ~ Lag1+Lag2, data = Smarket, family = binomial, subset = train)
glm.probs3 = predict(glm.fit.model3, Smarket.2005, type = "response") 
glm.pred3 = rep("Down", 252)
glm.pred3[glm.probs3 > 0.5] = "Up"
table(glm.pred3, Direction.2005)
mean(glm.pred3 == Direction.2005) 
# 0.5595238 ==> approx. 0.56 accuracy rate


#Linear Discriminant Analysis
#multiclass classification
library(MASS)
names(iris)
dim(iris) #150 rows and 5 columns
head(iris)

#create training dataset using random sampling ==> sample()
#allocate half the dataset to train the model
set.seed(555)
Train = sample(1:nrow(iris), nrow(iris)/2) 
iris_Train = iris[Train,] #training dataset 
irist_Test = iris[-Train,] #testing dataset

help(lda)
#fit model
fit1 = lda(Species ~ Sepal.Length+Sepal.Width+Petal.Length+Petal.Width, data = iris_Train)
#conduct prediction
predict1 = predict(fit1, iris_Train) 
predict1_class = predict1$class
#generate confusion matrix
table1 = table(predict1_class, iris_Train$Species)
table1
#accuracy of prediction
sum(diag(table1))/sum(table1)
#accuracy obtained = 1.0 (no incorrect predictions)
