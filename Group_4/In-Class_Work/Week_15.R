library(caret)

data(iris)
dataset = iris
head(dataset)

#select 20% of data for validation
#use remaining 80% of data for training and testing models 
train_index = createDataPartition(dataset$Species, p = 0.80, list = FALSE) 
dataset = dataset[train_index,]
validation = dataset[-train_index,]
#dimensions of dataset
dim(dataset)

#list types for each attribute
sapply(dataset, class)
#examine first 6 rows of data
head(dataset)
#list species
levels(dataset$Species)
#summarise class distribution
percentage = prop.table(table(dataset$Species)) * 100 
cbind(freq = table(dataset$Species), percentage = percentage)

#summarise attribute distributions 
summary(dataset)

#Plots
#split input and output
x = dataset[,1:4]
y = dataset[,5]
#boxplot for each attribute on one image 
par(mfrow = c(1,4))
for(i in 1:4){
  boxplot(x[,i], main = names(iris)[i])}

#barplot for class breakdown
plot(y)

#Multivariate Plots
#scatterplot matrix
featurePlot(x = x, y = y, plot = "ellipse")

#box and whisker plots for each attribute 
featurePlot(x = x, y = y, plot = "box")

#density plots for each attribute by class value
scales = list(x = list(relation = "free"), y = list(relation = "free"))
featurePlot(x = x, y = y, plot = "density", scales = scales)

#run algorithms using 10-fold cross validation
control = trainControl(method = "cv", number = 10)
metric = "Accuracy"

#build models
#a)linear algorithms
set.seed(7)
fit.lda = train(Species~., data = dataset, method = "lda", metric = metric, trControl = control)

#b)nonlinear algorithms
#CART
set.seed(7)
fit.cart = train(Species~., data = dataset, method = "rpart", metric = metric, trControl = control)

#KNN
set.seed(7)
fit.knn = train(Species~., data = dataset, method = "knn", metric = metric, trControl = control)

#c)advanced algorithms
#SVM
set.seed(7)
fit.svm = train(Species~., data = dataset, method = "svmRadial", metric = metric, trControl = control) 

#Random Forest
set.seed(7)
fit.rf = train(Species~., data = dataset, method = "rf", metric = metric, trControl = control)

#summarize accuracy of models
results = resamples(list(lda = fit.lda, cart = fit.cart, knn = fit.knn, svm = fit.svm, rf = fit.rf)) 
summary(results)

#compare accuracy of models
dotplot(results)
#most accurate model is LDA

#summarize best model
print(fit.lda)

#run LDA on the validation dataset
predictions = predict(fit.lda, validation)
confusionMatrix(predictions, validation$Species)
