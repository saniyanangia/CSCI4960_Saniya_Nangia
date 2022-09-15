# Lesson 2

# Part 1

temperature = 84.5
class(temperature)
RPI = "Rensselaer Polytechnic Institute"
class(RPI)
Rpi = 3.14159265359
class(Rpi)
isSnowing = FALSE
class(isSnowing)
R = FALSE
class(R)

num_vec = c(1,3,5,99)
class(num_vec)

cha_vec = c("R", "P", "I")
class(cha_vec)
boolean_vec = c(T, FALSE, F)
class(boolean_vec)

vec_mixed = c("RPI", 1824, 3.14)
vec_mixed
class(vec_mixed) #characters always dominate!! the other 2 are converted to characters
vec_mixed_bool_1 = c(TRUE, "RPI", 1824, 3.14)
vec_mixed_bool_1
class(vec_mixed_bool_1) #numbers are converted to characters

vec_mixed_bool_2 = c(TRUE, 1824, 3.14)
vec_mixed_bool_2
class(vec_mixed_bool_2) #true is converted to 1

temperature = c(80,81.3,83,84.2,82.5)
names_temparture = c("Mon", "Tue", "Wed", "Thu", "Fri")
#can do the same thing by:
Week_Days = c("Mon", "Tue", "Wed", "Thu", "Fri")
names(temperature) = Week_Days
temperature

vec1 = c("R", "P", "I") #indexing in R starts with 1!!
vec2 = c(1, 8, 2, 4)
vec1[1]
vec2[2]

m = c(1:10)
m
matrix(1:12, byrow = FALSE, nrow = 4)
matrix(1:12, byrow = TRUE, nrow = 4)

goog = c(560, 561, 562, 563, 564)
mcft = c(460, 461, 462, 463, 464)
stocks = c(goog, mcft)
stocks
print(stocks)
stocks.matrix = matrix(stocks, byrow = T, nrow = 2)
stocks.matrix #same as stocks_matrix (periods are allowed in variable names)
days = c("Mon", "Tue", "Wed", "Thu", "Fri")
st.names = c("goog", "mcft")
colnames(stocks.matrix) = days
rownames(stocks.matrix) = st.names
print(stocks.matrix)
mat = matrix(1:25, byrow = 1, nrow = 5) #byrow can also = T
mat
mat*2
mat/2
mat^2
1/mat
mat>15
mat[mat > 15]
mat + mat
mat/mat
colSums(stocks.matrix)
rowSums(stocks.matrix)
rowMeans(stocks.matrix)

#Bind the columns
FB = c(223, 224, 225, 226.5, 221)
tech.stocks = rbind(stocks.matrix, FB) #row bind
tech.stocks
avg = rowMeans(tech.stocks)
avg
tech.stocks = cbind(tech.stocks, avg) #column bind
tech.stocks
mat = matrix(1:50, byrow = T, nrow = 5)
mat
mat[1, ]
mat[ ,1]
mat[1:3, ]
mat[1:2, 1:3]
mat[ ,9:10]
mat[2:3, 5:6]

#Factor and categorical variables
animals = c('dog', 'cat', 'dog', 'cat', 'cat')
id = c(1, 2, 3, 4, 5)
temps = c('cold', 'med', 'hot', 'hot', 'hot', 'cold', 'med')
temps
fact.temps = factor(temps, ordered = T, levels = c('cold', 'med', 'hot'))
fact.temps
summary(fact.temps)
summary(temps)

undergrads = c('Freshman', 'Junior', 'Sophomore', 'Junior', 'Senior', 'Sophomore', 'Junior', 'Freshman', 'Senior', 'Junior')
undergrads
factor.undergrads = factor(undergrads, ordered = T, levels = c('Freshman', 'Sophomore', 'Junior', 'Senior'))
factor.undergrads
summary(factor.undergrads)

A = c(1, 2, 3)
B = c(4, 5, 6)
A = rbind(A, B)
A
mat = matrix(1:9, nrow = 3)
mat
is.matrix(mat)
mat2 = matrix(1:25, byrow = T, nrow = 5)
mat2
mat2[2:3, 2:3]
mat2[4:5, 4:5]
sum(mat2)
help("runif")
u = runif(20)
u
runif(matrix(20))
matrix(runif(20), nrow = 4)


#Part 2

days = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")
temp = c(28.0, 30.2, 32, 31.2, 29.3, 27.9, 26.4)
snowed = c("T", "T", "F", "F", "T", "T", "F") #passing this as characters, not bool
help("data.frame")

myDataFrame = data.frame() #empty df
myDataFrame

RPI_Weather_Week = data.frame(days, temp, snowed)

RPI_Weather_Week
head(RPI_Weather_Week) #first 6 rows

str(RPI_Weather_Week) #seeing the structure of the the df using the str() function
summary(RPI_Weather_Week)

RPI_Weather_Week[1, ] #1st row & all columns; do not need to put ':' 
RPI_Weather_Week[ ,1] #1st column & all rows

RPI_Weather_Week[ ,'snowed']
RPI_Weather_Week[ ,'days']
RPI_Weather_Week[ ,'temp']
RPI_Weather_Week[1:5,c('days', 'temp')]
RPI_Weather_Week$temp
summary(RPI_Weather_Week$temp)

subset(RPI_Weather_Week,subset=snowed==TRUE)
sorted.snowed = order(RPI_Weather_Week['snowed'])
sorted.snowed
RPI_Weather_Week[sorted.snowed, ]

dec.snow = order(-RPI_Weather_Week$temp)
dec.snow

#empty dataframe
empty.Dataframe = data.frame()
v1 = 1:10
v1
letters
v2 = letters[1:10]
df = data.frame(col.name.1 = v1, col.name.2 = v2)
df

#importing data and exporting data
#writing to a csv file
write.csv(df, file = 'saved_df1.csv')
df2 = read.csv('saved_df1.csv')
df2
