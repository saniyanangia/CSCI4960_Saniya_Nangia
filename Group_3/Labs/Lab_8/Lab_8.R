#PCA on UCI wine dataset
#cvs = cultivars (varieties) of the class of the wine
#need to identify which of the 3 cultivars a wine belongs to

wine_data = read.table("http://archive.ics.uci.edu/ml/machine-learning-databases/wine/wine.data", sep = ",")
#no header --> need to add the variable names
head(wine_data)
#13 variables in dataset
#first variable is the cultivar (cv1, cv2 or cv3)
nrow(wine_data)
#178 rows

#adding variable names
colnames(wine_data) = c("Cvs", "Alcohol", "Malic_Acid", "Ash", "Alkalinity_of_Ash", "Magnesium", "Total_Phenols", "Flavanoids", "NonFlavanoid_Phenols", "Proanthocyanins", "Color_Intensity", "Hue", "OD280/OD315_of_Diluted_Wine", "Proline")
head(wine_data)

#use heatmap() to check correlations between variables
#dark colors = correlated
#light colors = not/less correlated
help("heatmap")
heatmap(cor(wine_data), Rowv= NA, Colv = NA)

#to identify the 3 cultivars, we will declare 3 classes that represent the cultivars (Cv1, Cv2, Cv3) by using the factor() function
help(factor)
cultivar_classes = factor(wine_data$Cvs)
cultivar_classes

#default function in R for PCA is prcomp() function
help(prcomp)

#normalise wine data to a common scale using scale() function
#PCA process will not overweigh variables that happen to have larger values
help(scale)

#we do not normalise the Cvs variable (first column) so we can exclude -1 values
wine_data_PCA = prcomp(scale(wine_data[, -1]))

#use summary() function to see the cumulative proportion that each principal component (PC) contributes
summary(wine_data_PCA)
#PC1 gives 36.2% cumulative distribution ==> PC1 represents 36.2% variance of the data

#We can choose to have 8 variables out of 13 (PC1 to PC8) with only about 8% loss of cumulative contribution value
