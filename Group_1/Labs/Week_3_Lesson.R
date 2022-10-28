install.packages("ggplot2")
library("ggplot2")
install.packages("dplyr")
library("dplyr")

data(diamonds)
View(diamonds)
head(diamonds)
tail(diamonds)
summary(diamonds)
dim(diamonds)

is.na(diamonds)
na.omit(diamonds)
dim(diamonds)
colnames(diamonds)
attach(diamonds)

#Categorical Variables
ggplot(data = diamonds) + geom_bar(mapping = aes(x = cut))
#Count observations in each bin
help("count")
diamonds %>% count(cut)

#Continuous Variables
ggplot(data = diamonds) + geom_histogram(mapping = aes(x = carat), binwidth = 0.5)
#Count observations in each bin (range = 0.5)
diamonds %>% count(cut_width(carat, 0.5))

#Smaller binwidths for diamonds with size < 3
help("filter")
smaller = diamonds %>% filter(carat < 3) 
ggplot(data = smaller, mapping = aes(x = carat)) + geom_histogram(binwidth = 0.1)
#clusters of smaller values ==> subgroups exist within data

#Use geom_freqpoly() instead of geom_histogram() to overlay multiple histograms in the same plot
help("geom_freqpoly")
ggplot(data = smaller, mapping = aes(x = carat, color = cut)) + geom_freqpoly(binwidth = 0.1)

#Questions to Ask:
#Which values are the most common? Why?
#Which values are rare? Why? Does that match your expectations?
#Can you see any unusual patterns? What might explain them?

#Eg
#Why are there more diamonds at whole carats and common fractions of carats?
#Why are there more diamonds slightly to the right of each peak than there are slightly to the left of each peak?
#Why are there no diamonds bigger than 3 carats?

#For Clusters
#How are the observations within each cluster similar to each other?
#How are the observations in separate clusters different from each other?
#How can you explain or describe the clusters?
#Why might the appearance of clusters be misleading?

#Unusually wide limits on y-axis ==> presence of outliers
ggplot(diamonds) + geom_histogram(mapping = aes(x = y), binwidth = 0.5)

#Zoom into plot
help("coord_cartesian")
ggplot(diamonds) + geom_histogram(mapping = aes(x = y), binwidth = 0.5) + coord_cartesian(ylim = c(0, 50))
#Limits y-axis to specific values
#Use xlim() for limits on x-axis
#If using xlim() and ylim() in ggplot2, values outside limits will be discarded

#Unusual values identified: 0, ~30 and ~60
#Pluck out these values with dplyr
unusual = diamonds %>% filter(y < 3 | y > 20) %>% arrange(y)
unusual
#y is measuring one of the 3 dimensions of diamonds ==> cannot be 0
#32mm and 59mm are also too big (price does not increase much with such a large size)


#In-Class Work

#x-variable
ggplot(data = diamonds) + geom_histogram(mapping = aes(x = x), binwidth = 0.5)
diamonds %>% count(cut_width(x, 0.5))
ggplot(diamonds) + geom_histogram(mapping = aes(x = x), binwidth = 0.5) + coord_cartesian(ylim = c(0, 50))
#Outlier is 0
unusual_x = diamonds %>% filter(x < 3) %>% arrange(x)
unusual_x
#x has a distribution between 3.25 to 10.8

#y-variable
ggplot(data = diamonds) + geom_histogram(mapping = aes(x = y), binwidth = 0.5)
diamonds %>% count(cut_width(y, 0.5))
ggplot(diamonds) + geom_histogram(mapping = aes(x = y), binwidth = 0.5) + coord_cartesian(ylim = c(0, 50))
#Outliers are 0, ~30 and ~60
unusual_y = diamonds %>% filter(y < 3 | y > 20) %>% arrange(y)
unusual_y
#y has a distribution between 3.25 to 10.8

#z-variable
ggplot(data = diamonds) + geom_histogram(mapping = aes(x = z), binwidth = 0.5)
diamonds %>% count(cut_width(z, 0.5))
ggplot(diamonds) + geom_histogram(mapping = aes(x = z), binwidth = 0.5) + coord_cartesian(ylim = c(0, 50))
#Outliers are 0 and ~32
unusual_z = diamonds %>% filter(z < 1 | z > 20) %>% arrange(z)
unusual_z %>% print(width = Inf, n = 30)
#z has a distribution between 0.75 to 8.25

#Since x and y have similar distributions, they are likely to represent the length and width of a diamond
#z is likely to represent the depth of a diamond 

#price variable
#binwidth = 500
ggplot(data = diamonds) + geom_histogram(mapping = aes(x = price), binwidth = 500)
#binwidth = 100
ggplot(data = diamonds) + geom_histogram(mapping = aes(x = price), binwidth = 100)
#binwidth = 50
ggplot(data = diamonds) + geom_histogram(mapping = aes(x = price), binwidth = 50)
#Has a bin with no observations
#Zoom into  plot
ggplot(diamonds) + geom_histogram(mapping = aes(x = price), binwidth = 50) + coord_cartesian(xlim = c(0, 1525))
print(diamonds %>% count(cut_width(price, 50)), n = 25)
#No observations in range[1475, 1525]

#carat variable
carat_0.99 = diamonds[diamonds$carat == 0.99, ]
nrow(carat_0.99)
#23 diamonds are 0.99 carat

carat_1.00 = diamonds[diamonds$carat == 1.00, ]
nrow(carat_1.00)
#1558 diamonds are 1 carat
#Possible reason: 1 carat diamonds are more popular in the market (eg for engagement rings) than 0.99 carat diamonds

#Comparison of xlim() with ylim()
ggplot(diamonds) + geom_histogram(mapping = aes(x = price), binwidth = 50) + coord_cartesian(xlim = c(0, 1000))
#Limits x-axis to given values
ggplot(diamonds) + geom_histogram(mapping = aes(x = price), binwidth = 50) + coord_cartesian(ylim = c(0, 1000))
#Limits y-axis to given values

#Binwidth unset
ggplot(diamonds) + geom_histogram(mapping = aes(x = price)) + coord_cartesian(xlim = c(0, 1525))
#`stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

#Zoom to half a bar
ggplot(diamonds) + geom_histogram(mapping = aes(x = price), binwidth = 50) + coord_cartesian(xlim = c(1525, 1550))
#There are still ~250 observations in this range
#Histogram bin widths have already been computed beforehand
#No change to number of observations in the range


#For unusual values:
#Drop the entire row 
#[not recommended - not all values may be invalid/ you may end up with too little data]
diamonds2 = diamonds %>% filter(between(y, 3, 20))

#Or replace unusual values with NA
diamonds3 = diamonds %>% mutate(y = ifelse(y < 3 | y > 20, NA, y))
ggplot(data = diamonds3, mapping = aes(x = x, y = y)) + geom_point()
#To remove warning, set na.rm = TRUE
ggplot(data = diamonds2, mapping = aes(x = x, y = y)) + geom_point(na.rm = TRUE)

#NYC Flights

install.packages("nycflights13")
library(nycflights13)
View(flights)
head(flights)
tail(flights)
summary(flights)
dim(flights)

is.na(flights)
na.omit(flights)
dim(flights)
colnames(flights)
attach(flights)

nycflights13::flights %>% 
  mutate(cancelled = is.na(dep_time), 
  sched_hour = sched_dep_time %/% 100, 
  sched_min = sched_dep_time %% 100, 
  sched_dep_time = sched_hour + sched_min / 60) %>% ggplot(mapping = aes(sched_dep_time)) + geom_freqpoly(mapping = aes(color = cancelled), binwidth = 1/4)


#Exercises

data("airquality")
View(airquality)
attach(airquality)

#Missing Values in Histogram
ggplot(data = airquality) + geom_histogram(mapping = aes(x = Solar.R), binwidth = 10)
#Warning message: Removed 7 rows containing non-finite values (stat_bin).
#NA values are not plotted in histogram

#Missing Values in Bar Chart
ggplot(data = airquality) + geom_bar(mapping = aes(x = Solar.R))
#Warning message: Removed 7 rows containing non-finite values (stat_count)
#NA values are not plotted in bar chart 

#mean()
mean(Solar.R)
#NA
#na.rm = TRUE in mean()
mean(Solar.R, na.rm = TRUE)
#185.9315

#sum()
sum(Solar.R)
#NA
#na.rm = TRUE in sum()
sum(Solar.R, na.rm = TRUE)
#27146
