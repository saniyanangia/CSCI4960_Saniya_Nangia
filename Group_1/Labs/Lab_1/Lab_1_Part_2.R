install.packages("dplyr")
install.packages("ggplot2")

data_2010EPI = read.csv("/Users/saniyanangia/Desktop/CSCI_4960_Data_Analytics/DA_Files/EPI/2010EPI_data.csv", na.string = "NA", stringsAsFactors=FALSE)
names(data_2010EPI) = as.matrix(data_2010EPI[1, ])
data_2010EPI = data_2010EPI[-1, ]
View(data_2010EPI)
attach(data_2010EPI)
fix(data_2010EPI)

EPI = as.numeric(EPI) #data_2010EPI$EPI
tf = is.na(EPI)
E = EPI[!tf]

DALY = as.numeric(DALY) #data_2010EPI$DALY
tf = is.na(DALY)
E = DALY[!tf]

WATER_H = as.numeric(WATER_H) #data_2010EPI$WATER_H
tf = is.na(WATER_H)
E = WATER_H[!tf]

ENVHEALTH = as.numeric(ENVHEALTH) #data_2010EPI$ENVHEALTH
tf = is.na(ENVHEALTH)
E = ENVHEALTH[!tf]

ECOSYSTEM = as.numeric(ECOSYSTEM) #data_2010EPI$ECOSYSTEM
tf = is.na(ECOSYSTEM)
E = ECOSYSTEM[!tf]

AIR_H = as.numeric(AIR_H) #data_2010EPI$AIR_H
tf = is.na(AIR_H)
E = AIR_H[!tf]

AIR_E = as.numeric(AIR_E) #data_2010EPI$AIR_E
tf = is.na(AIR_E)
E = AIR_E[!tf]

WATER_E = as.numeric(WATER_E) #data_2010EPI$WATER_E
tf = is.na(WATER_E)
E = WATER_E[!tf]

BIODIVERSITY = as.numeric(BIODIVERSITY) #data_2010EPI$BIODIVERSITY
tf = is.na(BIODIVERSITY)
E = BIODIVERSITY[!tf]

# Comparing Distributions
qqplot(EPI, ENVHEALTH)
qqplot(EPI, ECOSYSTEM)
qqplot(EPI, AIR_H)
qqplot(EPI, AIR_E)
qqplot(EPI, WATER_E)
qqplot(EPI, BIODIVERSITY)

qqplot(ENVHEALTH, ECOSYSTEM)
qqplot(ENVHEALTH, AIR_H)
qqplot(ENVHEALTH, AIR_E)
qqplot(ENVHEALTH, WATER_E)
qqplot(ENVHEALTH, BIODIVERSITY)
qqplot(ENVHEALTH, DALY)
qqplot(ENVHEALTH, WATER_H)

qqplot(ECOSYSTEM, AIR_H)
qqplot(ECOSYSTEM, AIR_E)
qqplot(ECOSYSTEM, WATER_E)
qqplot(ECOSYSTEM, BIODIVERSITY)
qqplot(ECOSYSTEM, DALY)
qqplot(ECOSYSTEM, WATER_H)

qqplot(AIR_H, AIR_E)
qqplot(AIR_H, WATER_E)
qqplot(AIR_H, BIODIVERSITY)
qqplot(AIR_H, DALY)
qqplot(AIR_H, WATER_H)

qqplot(AIR_E, WATER_E)
qqplot(AIR_E, BIODIVERSITY)
qqplot(AIR_E, DALY)
qqplot(AIR_E, WATER_H)

qqplot(WATER_E, BIODIVERSITY)
qqplot(WATER_E, DALY)
qqplot(WATER_E, WATER_H)

qqplot(BIODIVERSITY, DALY)
qqplot(BIODIVERSITY, WATER_H)

qqplot(DALY, WATER_H)


boxplot(EPI, ENVHEALTH)
boxplot(EPI, ECOSYSTEM)
boxplot(EPI, AIR_H)
boxplot(EPI, AIR_E)
boxplot(EPI, WATER_E)
boxplot(EPI, BIODIVERSITY)

boxplot(ENVHEALTH, ECOSYSTEM)
boxplot(ENVHEALTH, AIR_H)
boxplot(ENVHEALTH, AIR_E)
boxplot(ENVHEALTH, WATER_E)
boxplot(ENVHEALTH, BIODIVERSITY)
boxplot(ENVHEALTH, DALY)
boxplot(ENVHEALTH, WATER_H)

boxplot(ECOSYSTEM, AIR_H)
boxplot(ECOSYSTEM, AIR_E)
boxplot(ECOSYSTEM, WATER_E)
boxplot(ECOSYSTEM, BIODIVERSITY)
boxplot(ECOSYSTEM, DALY)
boxplot(ECOSYSTEM, WATER_H)

boxplot(AIR_H, AIR_E)
boxplot(AIR_H, WATER_E)
boxplot(AIR_H, BIODIVERSITY)
boxplot(AIR_H, DALY)
boxplot(AIR_H, WATER_H)

boxplot(AIR_E, WATER_E)
boxplot(AIR_E, BIODIVERSITY)
boxplot(AIR_E, DALY)
boxplot(AIR_E, WATER_H)

boxplot(WATER_E, BIODIVERSITY)
boxplot(WATER_E, DALY)
boxplot(WATER_E, WATER_H)

boxplot(BIODIVERSITY, DALY)
boxplot(BIODIVERSITY, WATER_H)

boxplot(DALY, WATER_H)


# Multivariate Regression
multivariate = read.csv("/Users/saniyanangia/Desktop/CSCI_4960_Data_Analytics/DA_Files/multivariate.csv")
head(multivariate)
attach(multivariate)
help(lm)
mm = lm(Homeowners~Immigrant)
mm #R object
summary(mm)$coef #Pr(>|t|) is the significance level

plot(Homeowners~Immigrant)
help(abline)
abline(mm)
abline(mm, col=2, lwd=3) # line color = red, line width = 3

# Predicting Homeowner Values
library(dplyr)
newImmigrantdata = data.frame(Immigrant = c(0,20))
mm %>% predict(newImmigrantdata)

abline(mm)
abline(mm, col=3, lwd=3) # line color = green, line width = 3
attributes(mm)
mm$coefficients

# Creating Plots
plot(mtcars$wt, mtcars$mpg)
library(ggplot2)
qplot(mtcars$wt, mtcars$mpg)
qplot(wt, mpg, data = mtcars)
ggplot(mtcars, aes(x = wt, y = mpg)) + geom_point()
plot(pressure$temperature, pressure$pressure, type = "l")
points(pressure$temperature, pressure$pressure)

lines(pressure$temperature, pressure$pressure/2, col = "red")
points(pressure$temperature, pressure$pressure/2, col = "blue")
library(ggplot2)
qplot(pressure$temperature, pressure$pressure, geom = "line")
qplot(temperature, pressure, data = pressure, geom = "line")
ggplot(pressure, aes(x = temperature, y = pressure)) + geom_line() + geom_point()

# Creating Bar Graphs
barplot(BOD$demand, names.arg = BOD$Time)
table(mtcars$cyl)
barplot(table(mtcars$cyl)) #generate a table of counts
qplot(mtcars$cyl) #cyl is continuous here
qplot(factor(mtcars$cyl)) #treat cyl as discrete

# Bar Graph of Counts
qplot(factor(cyl), data = mtcars)
ggplot(mtcars, aes(x = factor(cyl))) + geom_bar()

# Creating Histograms
# To view distribution of one-dimensional data
hist(mtcars$mpg)
hist(mtcars$mpg, breaks = 10) #approximate number of bins = breaks
hist(mtcars$mpg, breaks = 5)
hist(mtcars$mpg, breaks = 12)
qplot(mpg, data = mtcars, binwidth = 4)
ggplot(mtcars, aes(x = mpg)) + geom_histogram(binwidth = 4)
ggplot(mtcars, aes(x = mpg)) + geom_histogram(binwidth = 5)

# Creating Box Plots
plot(ToothGrowth$supp, ToothGrowth$len) # passes plot() function a factor of x-values and a vector of y-values
boxplot(len ~ supp, data = ToothGrowth) #2 vectors are in same df, so can use formula syntax; combines 2 variables on x-axis
boxplot(len ~ supp + dose, data = ToothGrowth) #put interaction of 2 variables on x-axis

library(ggplot2) #using ggplot2 for box plots
qplot(ToothGrowth$supp, ToothGrowth$len, geom = "boxplot")
qplot(supp, len, data = ToothGrowth, geom = "boxplot") #can use this syntax if both vectors are in same df
ggplot(ToothGrowth, aes(x = supp, y = len)) + geom_boxplot() #same as above, using ggplot2

qplot(interaction(ToothGrowth$supp, ToothGrowth$dose), ToothGrowth$len, geom = "boxplot") #3 separate vectors
qplot(interaction(supp, dose), len, data = ToothGrowth, geom = "boxplot") #same as above + get columns from df
ggplot(ToothGrowth, aes(x = interaction(supp, dose), y = len)) + geom_boxplot() #using ggplot()
