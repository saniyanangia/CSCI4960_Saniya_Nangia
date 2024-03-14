install.packages("tree")
library(MASS)
library(tree)
library(rpart)
set.seed(1)
head(Boston)
tail(Boston)
summary(Boston)
str(Boston)

train = sample(1:nrow(Boston), nrow(Boston)/2)  #indices ==> select half the rows in Boston
tree.boston = tree(medv ~., Boston, subset = train)
summary(tree.boston)
#summary() indicates only 3 of the variables have been used to construct the tree
#"rm", "lstat", "crim" 
#for regression tree, the deviance is the sum of squared errors for the tree

#regression tree
tree(formula = medv ~., data = Boston, subset = train)
plot(tree.boston)
text(tree.boston, pretty = 0)
#lstat measures the percentage of individuals with a lower socioeconomic status
#tree indicates that lower values of lstat correspond to a more expensive house

#use cv.tree() to see whether pruning the tree improves performance
cv.boston = cv.tree(tree.boston)
plot(cv.boston$size, cv.boston$dev, typ = "b")

#in this case, the most complex tree is selected by cross-validation
#if we wish to prune the tree, we could use prune.tree()
prune.boston = prune.tree(tree.boston, best = 5)
plot(prune.boston)
text(prune.boston, pretty = 0)

#in keeping with cross-validation results, use the unpruned tree to make predictions on the test set
yhat = predict(tree.boston, newdata = Boston[-train, ])
boston.test = Boston[-train, "medv"]
plot(yhat, boston.test)
abline(0, 1)
mean((yhat-boston.test)^2)

#test set MSE associated with the regression tree = 28.08
#square root of MSE is ~5.298
#indicates that model leads to test predictions that are within around $5,298 of the true median home value for the suburb

#Bagging and Random Forest Example
library(randomForest)
set.seed(1)
bag.boston = randomForest(medv ~., data = Boston, subset = train, mtry = 13, importance = TRUE)
bag.boston
#the argument mtry = 13 indicates that all 13 predictors should be considered for each split of the tree
#in other words, the bagging should be done

#check how well bagged model performs on the test set
yhat.bag = predict(bag.boston, newdata = Boston[-train, ])
plot(yhat.bag, boston.test)
abline(0, 1)
mean((yhat.bag-boston.test)^2)

#test set MSE associated with the bagged regression tree is 11.31 --> less than that obtained using an optimally-pruned single tree
#can change the number of trees grown by RandomForest() using the ntree argument

bag.boston = randomForest(medv ~., data = Boston, subset = train, mtry = 13, ntree = 25)
yhat.bag = predict(bag.boston, newdata = Boston[-train, ])
mean((yhat.bag-boston.test)^2)

set.seed(1)
rf.boston = randomForest(medv ~., data = Boston, subset = train, mtry = 6, importance = TRUE)
yhat.rf = predict(rf.boston, newdata = Boston[-train, ])
mean((yhat.rf-boston.test)^2)
#test set MSE = 10.60
#indicates that random forest yielded an improvement over bagging in this case

#using the importance() function, we can view the importance of each variable
importance(rf.boston)
#2 measures of variable importance are reported

#the first is based on mean decrease in accuracy of predictions on the out-of-bag samples
#when a given variable is excluded from the model

#the second is a measure of the total decrease in node impurity 
#that results from splits over that variable, averaged over all trees

#for regression trees, the node impurity is measured by the training RSS
#for classification trees, the node impurity is measured by the deviance
#produce plots of these measures using varImpPlot() function
varImpPlot(rf.boston)
