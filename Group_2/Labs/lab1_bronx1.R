install.packages("xlsx")
library(gdata)

#faster xls reader but requires perl!
#bronx1<-read.xls(file.choose(),pattern="BOROUGH",stringsAsFactors=FALSE,sheet=1,perl="<SOMEWHERE>/perl/bin/perl.exe") 
#bronx1<-bronx1[which(bronx1$GROSS.SQUARE.FEET!="0" & bronx1$LAND.SQUARE.FEET!="0" & bronx1$SALE.PRICE!="$0"),]

#alternate
#library("xlsx", lib.loc="/Library/Frameworks/R.framework/Versions/3.0/Resources/library")
library("xlsx")
bronx1<-read.xlsx("/Users/saniyanangia/Desktop/CSCI_4960_Data_Analytics/DA_Files/NYhousing/rollingsales_bronx.xls",pattern="BOROUGH",stringsAsFactors=FALSE,sheetIndex=1,startRow=5,header=TRUE)
View(bronx1)

# attach(bronx1) # If you choose to attach, leave out the "data=." in lm regression
SALE.PRICE<-sub("\\$","",SALE.PRICE) 
SALE.PRICE<-as.numeric(gsub(",","", SALE.PRICE)) 
summary(SALE.PRICE)

GROSS.SQUARE.FEET<-as.numeric(gsub(",","", GROSS.SQUARE.FEET)) 
summary(GROSS.SQUARE.FEET)

LAND.SQUARE.FEET<-as.numeric(gsub(",","", LAND.SQUARE.FEET))
summary(LAND.SQUARE.FEET)

log.SALE.PRICE = log(SALE.PRICE)
log.GROSS.SQUARE.FEET = log(GROSS.SQUARE.FEET)
log.SALE.PRICE[is.na(log.SALE.PRICE) | log.SALE.PRICE == "Inf" | log.SALE.PRICE == "-Inf"] = NA
log.GROSS.SQUARE.FEET[is.na(log.GROSS.SQUARE.FEET) | log.GROSS.SQUARE.FEET == "Inf" | log.GROSS.SQUARE.FEET == "-Inf"] = NA
summary(log.SALE.PRICE)
summary(log.GROSS.SQUARE.FEET)

plot(log.GROSS.SQUARE.FEET, log.SALE.PRICE) 
m1<-lm(log.SALE.PRICE~log.GROSS.SQUARE.FEET, data = bronx1)
summary(m1)
abline(m1,col="red",lwd=2)
plot(resid(m1))

# Model 2

log.SALE.PRICE = log(bronx1$SALE.PRICE)
log.SALE.PRICE[is.na(log.SALE.PRICE) | log.SALE.PRICE == "Inf" | log.SALE.PRICE == "-Inf"] = NA

log.GROSS.SQUARE.FEET = log(bronx1$GROSS.SQUARE.FEET)
log.GROSS.SQUARE.FEET[is.na(log.GROSS.SQUARE.FEET) | log.GROSS.SQUARE.FEET == "Inf" | log.GROSS.SQUARE.FEET == "-Inf"] = NA

log.LAND.SQUARE.FEET = log(bronx1$LAND.SQUARE.FEET)
log.LAND.SQUARE.FEET[is.na(log.LAND.SQUARE.FEET) | log.LAND.SQUARE.FEET == "Inf" | log.LAND.SQUARE.FEET == "-Inf"] = NA

m2<-lm(log.SALE.PRICE~log.GROSS.SQUARE.FEET+log.LAND.SQUARE.FEET+factor(bronx1$NEIGHBORHOOD))
summary(m2)
plot(resid(m2))
# Suppress intercept - using "0+ ..."
m2a<-lm(log.SALE.PRICE~0+log.GROSS.SQUARE.FEET+log.LAND.SQUARE.FEET+factor(bronx1$NEIGHBORHOOD))
summary(m2a)
plot(resid(m2a))

# Model 3
m3<-lm(log.SALE.PRICE~0+log.GROSS.SQUARE.FEET+log.LAND.SQUARE.FEET+factor(bronx1$NEIGHBORHOOD)+factor(bronx1$BUILDING.CLASS.CATEGORY))
summary(m3)
plot(resid(m3))

# Model 4
m4<-lm(log.SALE.PRICE~0+log.GROSS.SQUARE.FEET+log.LAND.SQUARE.FEET+factor(bronx1$NEIGHBORHOOD)*factor(bronx1$BUILDING.CLASS.CATEGORY))
summary(m4)
plot(resid(m4))
#
