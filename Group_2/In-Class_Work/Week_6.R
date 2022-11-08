#Part 1: KNN

abalone = read.csv("/Users/saniyanangia/Desktop/CSCI_4960_Data_Analytics/DA_Files/abalone.csv", header = FALSE, sep = ',')
#Column names
colnames(abalone) = c('sex', 'length', 'diameter', 'height', 'whole_weight', 'shucked_weight', 'viscera_weight', 'shell_weight', 'rings')
#summary on abalone
summary(abalone)
#structure of abalone data
str(abalone)
#summary of abalone rings column
abalone$rings = as.numeric(abalone$rings)
summary(abalone$rings)
#rings variable has a range between 1 to 29
#this is the variable we want to predict

#break the rings variable into 3 levels
#"young" for abalone<8, "adult" for 8<=abalone<=11, "old" for abalone>11
abalone$rings = cut(abalone$rings, br=c(-1,8,11,15), labels = c("young", "adult", "old")) #starting with -1 because of histogram widths - make sure 0 does not go out of bounds
abalone$rings = as.factor(abalone$rings)
summary(abalone$rings)
#remove "sex" variable in abalone since KNN requires all numeric variables of prediction
aba = abalone
aba$sex = NULL

#normalise the data using min max normalisation
aba$length = as.numeric(aba$length)
aba$diameter = as.numeric(aba$diameter)
aba$height = as.numeric(aba$height)
aba$whole_weight = as.numeric(aba$whole_weight)
aba$shucked_weight = as.numeric(aba$shucked_weight)
aba$viscera_weight = as.numeric(aba$viscera_weight)
aba$shell_weight = as.numeric(aba$shell_weight)
aba = na.omit(aba)

normalise = function(x) {return ((x - min(x)) / (max(x) - min(x)))}
aba[1:7] = as.data.frame(lapply(aba[1:7], normalise))
summary(aba$shucked_weight)
#After normalisation, each variable has a min of 0 and a max of 1 (range from 0 to 1)

#Split data into training and testing sets
ind = sample(2, nrow(aba), replace = TRUE, prob = c(0.7, 0.3))
KNNtrain = aba[ind == 1,]
KNNtest = aba[ind == 2,]
nrow(KNN)
sqrt(2950)
#make k equal to the square root of 2950, the number of observations in the training set
#sqrt(2950) ~= 54.3139 ==> round to k = 55
#We usually take an odd number for k value

#knn model
#knn() is in the 'class' library
library(class)
help("knn")
KNNpred = knn(train = KNNtrain[1:7], test = KNNtest[1:7], cl = KNNtrain$rings, k = 55)
KNNpred
table(KNNpred)


#Part 2: KMeans

library(ggplot2)
head(iris) #first 6 rows
str(iris) #structure of iris dataset
#dataset has 150 observations equally distributed between three species: Setosa, Versicolor and Verginica
summary(iris) #summary statistics for all 4 variables
help("sapply")
sapply(iris[,-5],var)
summary(iris)

#plot Sepal.Length VS Sepal.Width
ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width, col = Species)) + geom_point()
#plot Petal.lLength VS Sepal.Width using ggpolt
ggplot(iris,aes(x = Petal.Length, y = Petal.Width, col = Species)) + geom_point()

#KMeans clustering
set.seed(300)
k.max = 12
#tot.withinss = total within-cluster sum of squares
#iter.max = maximum number of iterations allowed
#nstart = if centers is a number, how many random sets should be chosen
wss = sapply(1:k.max, function(k) {kmeans(iris[,3:4], k, nstart = 20, iter.max = 1000)$tot.withinss})
wss # within sum of squares
plot(1:k.max, wss, type="b", xlab="Number of clusters(k)", ylab="Within cluster sum of squares") 
icluster = kmeans(iris[,3:4], 3, nstart = 20)
table(icluster$cluster, iris$Species)

#In the table, we can see that most of the observations have been clustered correctly
#However, 2 of the versicolor have been put in cluster 1 which has mostly virginica
#4 of the virginica have been put in cluster 2 which has mostly versicolor
