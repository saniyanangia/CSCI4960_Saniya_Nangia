data("iris")
head(iris)
str(iris)
library(ggplot2)
library(e1071)

#Separate species based on color and plot with Petal.Length VS Petal.Width
#We can clearly see the separation of Setosa, but there is overlaps in Versicolor and Virginica
#Plot using qplot(), X = Petal.Length, Y = Petal.Width using the color separation with respect to species
qplot(Petal.Length, Petal.Width, data = iris, color = Species)

#Use svm() function from e1071 library
#First model = svm_model1
help("svm")
svm_model1 = svm(Species~., data = iris)

#Use summary command to see summary of first model
#Pass svm_model1 to summary() function
summary(svm_model1)

#Use plot() function to plot results from svm_model1
#Axes are Petal.Width VS Petal.Length --> Need to lock Sepal.Width and Sepal.Length values
#You choose the Sepal.Width (median ~= 3) and Sepal.Length (testing results with 4 even though median ~= 5.8) values
#Median values used since they will be less sensitive to outliers
#Just need ballpark values for Sepal.Width and Sepal.Length --> locking exact median values won't result in a large improvement in accuracy
plot(svm_model1, data = iris, Petal.Width~Petal.Length, slice = list(Sepal.Width = 3, Sepal.Length = 4))

#Prediction using the model created
pred1 = predict(svm_model1, iris)

#Create table using predicted and actual values
table1 = table(Predicted = pred1, Actual = iris$Species)
table1
#2 misclassifications in Virginica and Versicolor

#Calculate model1 accuracy (manual calculation in this case)
model1_accuracyRate = sum(diag(table1))/sum(table1)
model1_accuracyRate

#Calculate misclassification rate
model1_misclassificationRate = 1- model1_accuracyRate
model1_misclassificationRate
#Default kernel is radial for this library



#Use kernel = "linear"
svm_model2 = svm(Species~., data = iris, kernel = "linear")

#Summary of second model
summary(svm_model2)

#Plot results of second model
#Axes = Petal.Width VS Petal.Length
plot(svm_model2, data = iris, Petal.Width~Petal.Length, slice  = list(Sepal.Width = 3, Sepal.Length = 4))

#Prediction using second model
pred2 = predict(svm_model2, iris)

#Create table using predicted and actual values
table2 = table(Predicted = pred2, Actual = iris$Species)
table2
#5 misclassifications

#model2 accuracy rate
model2_accuracyRate = sum(diag(table2))/sum(table2)
model2_accuracyRate

#model2 misclassification rate
model2_misclassificationRate = 1 - model2_accuracyRate
model2_misclassificationRate



#Use kernel = "polynomial"
svm_model3 = svm(Species~., data = iris, kernel = "polynomial")

#Summary of third model
summary(svm_model3)

#Plot results of third model
#Axes = Petal.Width VS Petal.Length
plot(svm_model3, data = iris, Petal.Width~Petal.Length, slice  = list(Sepal.Width = 3, Sepal.Length = 4))

#Prediction using third model
pred3 = predict(svm_model3, iris)

#Create table using predicted and actual values
table3 = table(Predicted = pred3, Actual = iris$Species)
table3
#7 misclassifications

#model3 accuracy rate
model3_accuracyRate = sum(diag(table3))/sum(table3)
model3_accuracyRate

#model3 misclassification rate
model3_misclassificationRate = 1 - model3_accuracyRate
model3_misclassificationRate

