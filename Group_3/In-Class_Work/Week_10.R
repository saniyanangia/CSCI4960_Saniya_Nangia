#PCA on USArrests dataset

data("USArrests")
help("USArrests")

#rows contain the 50 states in alphabetical order
states = row.names(USArrests)
states

#columns contain the 4 variables
names(USArrests)

#variables have vastly different means
#apply() function allows us to apply a function - the mean() function to each row/column in this case
#second input denotes whether we wish to compute the mean of the rows (1) or columns (2)
apply(USArrests, 2, mean)

#variances of four variables
apply(USArrests, 2, var)
#variables also have vastly different variances

#UrbanPop variable measures percentage of population in each state living in an urban area
#this is not comparable to the number of crimes in each state per 10,000 variables
#need to scale the variables before performing PCA
#else most of the principal components observed would be driven by Assault variable
#assault variable has the largest mean and variance
#standardise the variables to have mean 0 and sd 1 before performing PCA

#perform principal components analysis using prcomp() function
#by default, prcomp() centers the variables to have mean 0
#use scale = TRUE to scale the variables to have sd 1

pr.out = prcomp(USArrests, scale = TRUE)
names(pr.out)

#means and standard deviations of variables that were used for scaling prior to implementing PCA
pr.out$center 
pr.out$scale

#rotation matrix provides principal component loadings
#each column of pr.out$rotation contains the corresponding principal component loading vector
#there are 4 distinct principal components
pr.out$rotation

#using the prcomp() function, 
#we do not need to explicitly multiply that data by the principal component loading vectors
#in order to obtain the principal component score vectors

#rather the 50x4 matrix x has as its columns the principal component score vectors
#kth column is the kth principal component score vector
dim(pr.out$x)

#plot the first 2 principal components
biplot(pr.out, scale = 0)
#scale e= 0 ensures that arrows are scaled to represent the loadings
#other value for scale give slighgtly different bipolts with different representtaions

#prcomp() also output the sd of each principal component
pr.out$sdev

#variance for each principal component
pr.var = pr.out$sdev^2
pr.var

#to find proportion of variance explained by each principal component, 
#divide the variance explained by each principal component by the total variance by all 4 principal components
pve = pr.var/sum(pr.var)
pve
#first principal component explains 62.0% of the variance in the data
#second principal component explains 24.74% of the variance in the data
#third principal component explains 8.91% of the variance in the data
#fourth principal component explains 4.34% of the variance in the data



#PCA on Iris dataset

data("iris")
head("iris")

#create another dataset from iris dataset that contains columns 1-4
irisdata1 = iris[1:4]
irisdata1
head(irisdata1)

help("princomp")
principal_components = princomp(irisdata1, cor = TRUE, score = TRUE)
#cor = logical value indicating whether calculation should use the correlation matrix or covariance matrix
#correlation matrix can only be used if there are no constant variables
#score = logical value indicating whether score on each principal component should be calculated
summary(principal_components)
#4 principal components since input data has 4 different features
#proportion of Variance: 0.7296245 0.2285076 0.03668922 0.005178709
#component 1 represents 72.9% of the variation of the data
#component 2 represents 22.8% of the variation of the data
#first two components collectively represent 95.7%

#plot the principal components
plot(principal_components)
#plot principal components using a line graph
plot(principal_components, type = "l")
#plot principal components using biplot() function
help("biplot")
biplot(principal_components)



#PCA on Boston dataset

data(Boston, package = "MASS")
help(Boston)

#prcomp() function to compute principal components and turn on scaling
help(prcomp)
pca_out = prcomp(Boston, scale. = T)
#pca_out shows the loadings that are used
pca_out
plot(pca_out)

#plotting using biplot()
help(biplot)
biplot(pca_out, scale = 0)

boston_pc = pca_out$x
boston_pc
#boston_pc has principal components having the same number of rows as the original dataset
head(boston_pc)
summary(boston_pc)
