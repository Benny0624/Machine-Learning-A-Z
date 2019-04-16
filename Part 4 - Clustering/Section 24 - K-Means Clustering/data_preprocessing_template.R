# Data Preprocessing Template

# Importing the dataset
dataset = read.csv('Mall_Customers.csv')
X <- dataset[4:5]
# Splitting the dataset into the Training set and Test set
# install.packages('caTools')
# library(caTools)
# set.seed(123)
# split = sample.split(dataset$DependentVariable, SplitRatio = 0.8)
# training_set = subset(dataset, split == TRUE)
# test_set = subset(dataset, split == FALSE)

# Feature Scaling
# training_set = scale(training_set)
# test_set = scale(test_set)

#Ussing the Elbow methodto find the optimal number of cluster
set.seed(6)
wcss <- vector()
for(i in 1:10)wcss[i] <- sum(kmeans(X, i)$withinss)
plot(1:10, wcss, type = 'b', main = 'Cluster of clients', 
     xlab = 'Number of Clusters', ylab = 'wcss')

#Apply k-means to the Mall dataset
set.seed(29)
kmeans <- kmeans(X, 5, iter.max = 300, nstart = 10)

#Visualizing the Result
library(cluster)
clusplot(X,
         kmeans$cluster,
         lines = 0,
         shade = T,
         color = T,
         labels = 2,
         plotchar = F,
         span = T,
         main = 'Cluster of client',
         xlab = 'Annual income',
         ylab = 'Spending Score')













