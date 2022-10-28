multivariate = read.csv("/Users/saniyanangia/Desktop/CSCI_4960_Data_Analytics/DA_Files/multivariate.csv")
head(multivariate)
attach(multivariate)
names(multivariate)
multivariate

#Scatterplots
plot(Income, Immigrant, main = "Scatterplot")
plot(Immigrant, Homeowners)

#Fitting Linear Models using 'lm' function
help(lm)
mm = lm(Homeowners~Immigrant)
mm 
plot(Immigrant, Homeowners)
abline(mm)
abline(mm, col = 2, lwd = 3)

summary(mm)
attributes(mm)
mm$coefficients

#Linear Fit
help(abline)
plot(Homeowners~Immigrant)

#Multivariate Regression
HP = Homeowners/Population
PD = Population/area
mm = lm(Immigrant~Income + Population + HP + PD) 
summary(mm)

cm = coef(mm)
cm
