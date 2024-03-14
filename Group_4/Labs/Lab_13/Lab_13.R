require(ggplot2)
head(diamonds)

#bar plot
ggplot(diamonds, aes(clarity, fill = cut)) + geom_bar()

#faceted bar plot
ggplot(diamonds, aes(clarity)) + geom_bar() + facet_wrap(~ cut)

#histogram
ggplot(diamonds) + geom_histogram(aes(x = price)) + geom_vline(xintercept = 12000)

#frequency polygon
ggplot(
  data = diamonds,
  mapping = aes(color = cut_number(carat, 5), x = price)) +
  geom_freqpoly() +
  labs(x = "Price", y = "Count", color = "Carat")

#boxplot
ggplot(diamonds, aes(x = cut_number(price, 10), y = carat)) + 
  geom_boxplot() +
  coord_flip() +
  labs(x = "Price", y = "Carat")

#boxplot
ggplot(diamonds, aes(x = cut_number(carat, 5), y = price, colour = cut)) +
  geom_boxplot() +
  labs(x = "Carat", y = "Price")


#Linear Discriminant Analysis
library(MASS)
library(ISLR)

data("Smarket")
attach(Smarket)
head(Smarket)
names(Smarket)
str(Smarket)
dim(Smarket)

#create a vector corresponding to observations from 2001 to 2004
#test set = observations from 2005
train = (Year < 2005)
Smarket.2005 = Smarket[!train,]
dim(Smarket.2005)
Direction.2005 = Direction[!train]

#fit model using only observations before 2005
lda.fit = lda(Direction ~ Lag1+Lag2, data = Smarket, subset = train)
lda.fit
#LDA output indicates that prob1 (Down) = 0.492 and prob2 (Up) = 0.508
#49.2% of training observations correspond to days during which the market went down

#predict() returns a list with three elements
#first element (class) contains LDA’s predictions about movement of the market
#second element (posterior) is a matrix, 
#whose kth column contains the posterior probability that the corresponding observation belongs to the kth class
#x contains linear discriminants
lda.pred = predict(lda.fit, Smarket.2005)
names(lda.pred)
lda.class = lda.pred$class 
table(lda.class, Direction.2005) 
mean(lda.class == Direction.2005)
#accuracy of predictions on test set = 0.56
