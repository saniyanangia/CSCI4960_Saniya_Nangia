#gg-plot line graph example
install.packages("gcookbook")
library(gcookbook)

#biochemical oxygen demand dataset
head(BOD)
summary(BOD)
ggplot(BOD, aes(x = Time, y = demand)) + geom_line()

#make a copy of the dataset
BOD1 = BOD 
BOD1$Time = factor(BOD1$Time)
ggplot(BOD1, aes(x = Time, y = demand, group = 1)) + geom_line()
ggplot(BOD, aes(x = Time, y = demand)) + geom_line() + ylim(0, max(BOD$demand)) 
ggplot(BOD, aes(x = Time, y = demand)) + geom_line() + expand_limits(y = 0)

#adding points to a line graph
ggplot(BOD, aes(x = Time, y = demand)) + geom_line() + geom_point()

#world population dataset
head(worldpop)
summary(worldpop)
ggplot(worldpop, aes(x = Year, y = Population)) + geom_line() + geom_point()

#adding points to log-y axis
ggplot(worldpop, aes(x = Year, y = Population)) + geom_line() + geom_point() + scale_y_log10()


#diamonds dataset plots example
require(ggplot2)
data(diamonds)
head(diamonds)

ggplot(diamonds, aes(clarity, fill = cut)) + geom_bar()
ggplot(diamonds, aes(clarity)) + geom_bar() + facet_wrap(~cut)
ggplot(diamonds) + geom_histogram(aes(x = price)) + geom_vline(xintercept = 12000)

ggplot(data = diamonds, mapping = aes(color = cut_number(carat, 5), x = price)) + 
                        geom_freqpoly() + 
                        labs(x = "Price", y = "Count", color = "Carat")

ggplot(diamonds, aes(x = cut_number(price, 10), y = carat)) + 
                        geom_boxplot() +
                        coord_flip() +
                        xlab("Price")

ggplot(diamonds, aes(x = cut_number(carat, 5), y = price, color = cut)) + geom_boxplot()
                                                                              
