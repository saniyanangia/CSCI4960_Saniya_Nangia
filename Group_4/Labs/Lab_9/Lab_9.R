library(e1071)
set.seed(1)
#use svm() function  to fit support vector classifier for given value of cost parameter
#2D example to plot resulting decision boundary
x = matrix(rnorm(20*2), ncol = 2)
y = c(rep(-1, 10), rep(1, 10))
x[y == 1,] = x[y == 1,] + 1
x
y

#check whether classes are linearly separable
plot(x, col = (3-y))
#they are not
#next, fit support vector classifier
#for svm() to perform classification, we must encode the response variable as a factor variable
#create a data frame with response coded as a factor
dat = data.frame(x = x, y = as.factor(y))
svmfit = svm(y~., data = dat, kernel = "linear", cost = 10, scale = FALSE)
#the argument scale = FALSE tells svm() not to scale each feature to have mean 0 or sd 1
#can use scale = TRUE depending on the application

#plot support vector classifier obtained
plot(svmfit, dat)
#arguments in plot() are output of the call to svm(), and data used in the call to svm()
#2 sections: feature space assigned to -1 and 1
#decision boundary is linear since kernel = "linear" (might look somewhat jagged)
#only 1 observation is misclassified
#unlike the usual plot() function in R, the first feature is plotted on the x-axis and the second feature is plotted on the y-axis
#support vectors are plotted as crosses and the remaining observation are plotted as circles
#there are 7 support vectors in this plot

#determine identities of support vectors
svmfit$index
#1  2  5  7 14 16 17

#obtain basic information about support vector classifier
summary(svmfit)

#linear kernel used with cost = 10
#7 support vectors --> 4 in one class and 3 in the other

#use a smaller value of the cost parameter
#cost = 0.1
svmfit = svm(y~., data = dat, kernel = "linear", cost = 0.1, scale = FALSE)
plot(svmfit, dat)
svmfit$index
#1  2  3  4  5  7  9 10 12 13 14 15 16 17 18 20

#smaller value of cost --> obtain larger number of support vectors since margin is wider
#note that svm() function does not explicitly output the coefficients of the linear decision boundary obtained when support vector classifier is fit, 
#nor does it output the width of the margin

#e1071 library includes a built-in function, tune(), to perform cross-validation
#by default, tune() performs 10-fold cross-validation on a set of models of interest
#pass in relevant information about set of models under consideration
#we compare SVMs with a linear kernel using a range of values of the cost parameter

set.seed(1)
tune.out = tune(svm, y~., data = dat, kernel = "linear", ranges = list(cost = c(0.001, 0.01, 0.1, 1, 5, 10, 100)))
#we can access cross-validation errors for each of these models using the summary() command
summary(tune.out)
#cost = 0.1 in the lowest cross-validation error rate
#tune() function stores the best model obtained
bestmod = tune.out$best.model
summary(bestmod)

#predict() function can be used to predict the class label on a set of test observations at any given value of the cost parameter
#generate test data set
xtest = matrix(rnorm(20*2), ncol = 2)
ytest = sample(c(-1,1), 20, rep = TRUE)
xtest[ytest == 1,] = xtest[ytest == 1,] + 1
testdat = data.frame(x = xtest, y = as.factor(ytest))

#predict class labels of test observation
#use best model obtained through cross-validation in order to make predictions
ypred = predict(bestmod, testdat)
table(predict = ypred, truth = testdat$y)

#with this value of cost, 17 of the test observations are correctly classified
#cost = 0.01
svmfit = svm(y~., data = dat, kernel = "linear", cost = 0.01, scale = FALSE)
ypred = predict(svmfit, testdat)
table(predict = ypred, truth = testdat$y)

#in this case, one additional observation is misclassified
#consider a situation in which 2 classes are linearly separable
#find a separating hyperplane using svm()
#first, further separate the 2 classes in our simulated data so that they are linearly separable
x[y == 1,] = x[y == 1,]+0.5
plot(x, col = (y+5)/2, pch = 19)

#now the observations are just barely linearly separable
#fit the support vector classifier and plot the resulting hyperplane
#use a very large value of cost so that no observations are misclassified
dat = data.frame(x = x, y = as.factor(y))
svmfit = svm(y~., data = dat, kernel = "linear", cost = 1e5)
summary(svmfit)
plot(svmfit, dat)

#no training errors and only 3 support vectors used
#however, margin is very narrow since observations that are circles (ie not support vectors) are very close to the decision boundary
#seems likely that model will perform poorly on test data
#try a smaller value of cost

svmfit = svm(y~., data = dat, kernel = "linear", cost = 1)
summary(svmfit)
plot(svmfit, dat)

#using cost = 1, we misclassify a training observation
#but we also obtain a much wider margin and make use of 7 support vectors
#seems likely that this model will perform better on test data than the model with cost = 1e5


#Khan data set 
#consists of a number of tissue samples corresponding to four distinct types of small round blue cell tumors
#for each tissue sample, gene expression measurements are available
#training data = xtrain and ytrain
#testing data = xtest and ytest
library(e1071)
library(ISLR)
names(Khan)

#dataset consists on expression measurement for 2308 genes
#training and testing sets consist of 63 and 20 observations respectively
dim(Khan$xtrain)
dim(Khan$xtest)

length(Khan$ytrain)
length(Khan$ytest)
table(Khan$ytrain)
table(Khan$ytest)

#use a support vector approach to predict cancer subtype using gene expression measurements
#dataset has a very large number of features relative to number of observations
#this suggests that we should use a linear kernel, because the additional flexibility that will result from using a polynomial or radial kernel is unnecessary
dat = data.frame(x = Khan$xtrain, y = as.factor(Khan$ytrain))
out = svm(y~., data = dat, kernel = "linear", cost = 10)
summary(out)

#there are no training errors
#large number of variables relative to the number of observations implies that it is easy to find hyperplanes that fully separate the classes
#we are interested in the support vector classifier's performance on the test observations, rather than the training observations

dat.te = data.frame(x = Khan$xtest, y = as.factor(Khan$ytest))
pred.te = predict(out, newdata = dat.te)
table(pred.te, dat.te$y)
#using cost = 10 yields 2 test set errors on this data


#Note:
#glm.cv(..., family = 'binomial') --> acts like lm() + does cross-validation if you add '.cv'
