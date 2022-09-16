#install.packages("MASS") --> but dependencies may not be installed
#Command+Enter to run code

library(MASS)
attach(Boston)

#Exploratory Data Analysis
#Figure out the Anomalies, Trends and thus pick your Model

?Boston #documentation of df
help("Boston") #documentation of df
head(Boston) #first 6 rows of df
dim(Boston) #dimentions of df
names(Boston) #column headers
nrow(Boston) #number of rows
ncol(Boston) #number of columns
str(Boston) #should run this every time you get a new df --> structure of df
summary(Boston) #summary statistics
Boston$crim #prints the entire 'crime' column into the console - BE CAREFUL
summary(Boston$crim) #summary of crime problem

library(ISLR)
data("Auto")
head(Auto)
summary(Auto$mpg)
fivenum(Auto$mpg)
median(Auto$horsepower)
hist(Auto$mpg)
boxplot(Auto$mpg)


help(read.csv) #always read the documentation of the method first
data1 = read.csv(file.choose(), header = TRUE) #csv file read into R will include header
head(data1)
View(data1) #excel-like view of the df
ncol(data1)
nrow(data1)
summary(data1)
fivenum(data1,na.rm = TRUE)
boxplot(data1)
hist(data1)
