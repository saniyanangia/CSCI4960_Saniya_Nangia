#creating a matrix with random numbers and plotting the matrix using the image() function
#there is no real pattern in the plot

set.seed(12345) #making sure the random numbers do not change beyond this scope
help(par)

#par is used to set or query graphical parameters
#parameters can be set by specifying them as arguments
#to par in tag=value form, or by passing them as a list of tagged values

par(mar = rep(0.2,4))
data_Matrix = matrix(rnorm(400), nrow = 40)
image(1:10, 1:40, t(data_Matrix)[,nrow(data_Matrix):1])

#run hierarchical cluster analysis on the dataset
#use heatmap() function available in R
help(heatmap) #or ?heatmap
help(rep)

par(mar = rep(0.2, 4))
heatmap(data_Matrix, col = heat.colors(256))

#now add a pattern to the data using a random coin flip
#use rbinom() function with a for-loop
help(rbinom)

set.seed(678910)
for(i in 1:40){
  #flipping the coin and getting the data
  coin_Flip = rbinom(1, size = 1, prob = 0.5)
  #if coin = "heads", add a common pattern to that row
  if(coin_Flip){
    data_Matrix[i, ] = data_Matrix[i,] + rep(c(0,3), each = 5)
  }
}

#now plot the data: RHS columns have more yellow (higher values), while LHS columns have more red (lower values)
#some of the rows have a mean of 3 in RHS, and some of the rows have a mean of 0
#pattern has been introduced

par(mar = rep(0.2, 4))
image(1:10, 1:40, t(data_Matrix)[, nrow(data_Matrix):1])

#run heatmap() function; 2 sets of columns are easily separated (2 clusters with 5 columns each)
#dendrogram on top of matrix
#rows have no clear pattern
par(mar=rep(0.2, 4)) 
heatmap(data_Matrix, col = heat.colors(256))

#find marginal means of rows and columns
#10 different column means and 40 different row means
hh = hclust(dist(data_Matrix))
data_Matrix_Ordered = data_Matrix[hh$order,]
par(mfrow = c(1,3))
image(t(data_Matrix_Ordered)[, nrow(data_Matrix_Ordered):1])
plot(rowMeans(data_Matrix_Ordered), 40:1, , xlab = "The Row Mean", ylab = "Row", pch = 19)
plot(colMeans(data_Matrix_Ordered), xlab = "Column", ylab = "Column Mean", pch = 19)

#Left plot has mean of each row (there are 40 rows and therefore 40 dots representing the mean)
#Right plot has mean of each column (there are 10 columns and therefore 10 dots representing the mean)
