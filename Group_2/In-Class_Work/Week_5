#Call NaiveBayes Classifier Package e1071, which auto calls the Class package
library("e1071")
classifier = naiveBayes(iris[,1:4], iris[,5])
table(predict(classifier, iris[,5]), iris[,5], dnn = list('predicted', 'actual'))
classifier$apriori
classifier #tables$Petal.length
plot(function(x) dnorm(x, 1.462, 0.1736640), 0, 8, col = 'red', main = 'Petal length distribution for the 3 different species')
#1.462 is the mean; 0.1736640 is the sd
curve(dnorm(x, 4.260, 0.4699110), add = TRUE, col = 'blue')
curve(dnorm(x, 5.552, 0.5518947), add = TRUE, col = 'green')
      
#Decision Trees
#Classification Tree
library("rpart")
install.packages("rpart.plot")
library("rpart.plot") 

#iris dataset
iris
dim(iris)  

#sample 
s_iris = sample(150,100) #random sample since dataset is ordered
s_iris
length(s_iris) #dim function will not work because this is not a dataframe

#testing and training dataset
iris_train = iris[s_iris,] #100 values
iris_test = iris[-s_iris,] #remaining 50 values
dim(iris_train) #always check the dimensions of train and test dataset!!
dim(iris_test)

#decision tree model
decisionTreeModel = rpart(Species~., iris_train, method = "class") #species is the 5th column; ~. includes all remaining columns instead of typing them out
#decisionTreeModel = rpart(Species~Sepal.Length + Sepal.Width + Petal.Length + Petal.Width, iris_train, method = "class")
decisionTreeModel #only shows decision tree in console, not as a diagram

#plot the decision tree
rpart.plot(decisionTreeModel)
