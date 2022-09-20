# Exercise 1

data_2010EPI = read.csv("/Users/saniyanangia/Desktop/CSCI_4960_Data_Analytics/DA_Files/EPI/2010EPI_data.csv", na.string = "NA", stringsAsFactors=FALSE)
#names(data_2010EPI) <- as.matrix(data_2010EPI[1, ])
#data_2010EPI <- data_2010EPI[-1, ]
#data_2010EPI[] <- lapply(data_2010EPI, function(x) type.convert(as.character(x)))
data_2010EPI = data_2010EPI[-1, ]
#data_2010EPI = lapply(data_2010EPI, function(x) replace(x, is.na(x), 0))
View(data_2010EPI)
attach(data_2010EPI)
fix(data_2010EPI)


# EPI Distribution

EPI = as.numeric(EPI) #data_2010EPI$EPI
tf = is.na(EPI)
E = EPI[!tf]
EPI
summary(EPI)
fivenum(EPI, na.rm = TRUE)
stem(EPI)
hist(EPI)
hist(EPI, seq(30., 95., 1.0), prob = TRUE)
lines(density(EPI, na.rm = TRUE, bw=1.)) #or bw = “SJ”
rug(EPI)

plot(ecdf(EPI), do.points=FALSE, verticals=TRUE) #cumulative density function
par(pty="s") #quantile-quantile
qqnorm(EPI)
qqline(EPI)

x = seq(30, 95, 1) #q-q plot
qqplot(qt(ppoints(250), df = 5), x, xlab = "Q-Q plot for t dsn")
qqline(x)


# DALY Distribution

DALY = as.numeric(DALY) #data_2010EPI$DALY
tf = is.na(DALY)
E = DALY[!tf]
DALY
summary(DALY)
fivenum(DALY, na.rm = TRUE)
stem(DALY)
hist(DALY)
hist(DALY, seq(0., 95., 1.0), prob = TRUE)
lines(density(DALY, na.rm = TRUE, bw=1.))
rug(DALY)

plot(ecdf(DALY), do.points=FALSE, verticals=TRUE) #cumulative density function
par(pty="s") #quantile-quantile
qqnorm(DALY)
qqline(DALY)

x = seq(0, 95, 1) #q-q plot
qqplot(qt(ppoints(250), df = 5), x, xlab = "Q-Q plot for t dsn")
qqline(x)


# WATER_H Distribution

WATER_H = as.numeric(WATER_H) #data_2010EPI$WATER_H
tf = is.na(WATER_H)
E = WATER_H[!tf]
WATER_H
summary(WATER_H)
fivenum(WATER_H, na.rm = TRUE)
stem(WATER_H)
hist(WATER_H)
hist(WATER_H, seq(0., 100., 1.0), prob = TRUE)
lines(density(WATER_H, na.rm = TRUE, bw=1.))
rug(WATER_H)

plot(ecdf(WATER_H), do.points=FALSE, verticals=TRUE) #cumulative density function
par(pty="s") #quantile-quantile
qqnorm(WATER_H)
qqline(WATER_H)

x = seq(0, 100, 1) #q-q plot
qqplot(qt(ppoints(250), df = 5), x, xlab = "Q-Q plot for t dsn")
qqline(x)


# Comparing Distributions

boxplot(EPI,DALY)
boxplot(EPI,WATER_H)
boxplot(DALY,WATER_H)

qqplot(EPI,DALY)
qqplot(EPI,WATER_H)
qqplot(DALY,WATER_H)


# Exercise 2


# Landlock Filtering

EPILand = EPI[!Landlock]
Eland = EPILand[!is.na(EPILand)]
hist(ELand)
hist(ELand, seq(30., 95., 1.0), prob=TRUE)

summary(ELand)
fivenum(ELand, na.rm = TRUE)
stem(ELand)
hist(ELand)
hist(ELand, seq(30., 95., 1.0), prob = TRUE)
lines(density(ELand, na.rm = TRUE, bw=1.)) #or bw = “SJ”
rug(ELand)

plot(ecdf(ELand), do.points=FALSE, verticals=TRUE) #cumulative density function
par(pty="s") #quantile-quantile
qqnorm(ELand)
qqline(ELand)

x = seq(30, 95, 1) #q-q plot
qqplot(qt(ppoints(250), df = 5), x, xlab = "Q-Q plot for t dsn")
qqline(x)


# No Surface Water Filtering

EPISWater = EPI[!No_surface_water]
ESWater = EPISWater[!is.na(EPISWater)]
hist(ESWater)
hist(ESWater, seq(30., 95., 1.0), prob=TRUE)

summary(ESWater)
fivenum(ESWater, na.rm = TRUE)
stem(ESWater)
hist(ESWater)
hist(ESWater, seq(30., 95., 1.0), prob = TRUE)
lines(density(ESWater, na.rm = TRUE, bw=1.)) #or bw = “SJ”
rug(ESWater)

plot(ecdf(ESWater), do.points=FALSE, verticals=TRUE) #cumulative density function
par(pty="s") #quantile-quantile
qqnorm(ESWater)
qqline(ESWater)

x = seq(30, 95, 1) #q-q plot
qqplot(qt(ppoints(250), df = 5), x, xlab = "Q-Q plot for t dsn")
qqline(x)


# Desert Filtering

EPIDesert = EPI[!Desert]
EDesert = EPIDesert[!is.na(EPIDesert)]
hist(EDesert)
hist(EDesert, seq(30., 95., 1.0), prob=TRUE)

summary(EDesert)
fivenum(EDesert, na.rm = TRUE)
stem(EDesert)
hist(EDesert)
hist(EDesert, seq(30., 95., 1.0), prob = TRUE)
lines(density(EDesert, na.rm = TRUE, bw=1.)) #or bw = “SJ”
rug(EDesert)

plot(ecdf(EDesert), do.points=FALSE, verticals=TRUE) #cumulative density function
par(pty="s") #quantile-quantile
qqnorm(EDesert)
qqline(EDesert)

x = seq(30, 95, 1) #q-q plot
qqplot(qt(ppoints(250), df = 5), x, xlab = "Q-Q plot for t dsn")
qqline(x)


# High Population Density Filtering

EPIHPD = EPI[!High_Population_Density]
EHPD = EPIHPD[!is.na(EPIHPD)]
hist(EHPD)
hist(EHPD, seq(30., 95., 1.0), prob=TRUE)

summary(EHPD)
fivenum(EHPD, na.rm = TRUE)
stem(EHPD)
hist(EHPD)
hist(EHPD, seq(30., 95., 1.0), prob = TRUE)
lines(density(EHPD, na.rm = TRUE, bw=1.)) #or bw = “SJ”
rug(EHPD)

plot(ecdf(EHPD), do.points=FALSE, verticals=TRUE) #cumulative density function
par(pty="s") #quantile-quantile
qqnorm(EHPD)
qqline(EHPD)

x = seq(30, 95, 1) #q-q plot
qqplot(qt(ppoints(250), df = 5), x, xlab = "Q-Q plot for t dsn")
qqline(x)


# Filtering on EPI Regions

EPI_South_Asia = EPI[EPI_regions == "South Asia"]
EPI_South_Asia


# Filtering on GEO Subregions
EPI_Caribbean = EPI[GEO_subregion == "Caribbean"]
EPI_Caribbean
