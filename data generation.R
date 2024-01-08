####### Data generation ########
library(mvtnorm)

set.seed(123)

#### bivariate normal distribution with 2 clusters

# parameters for generating the data set

# number of observations
n = 400

# means matrix
mean = c(1, 10)

# covariance matrix
cov = matrix(c(2, 5, 10, 4), nrow = 2)

# generating the multivariate normal distribution

data = as.data.frame(rmvnorm(n = n, mean = mean, sigma = cov))

colnames(data) = c("Variable 1", "Variable 2")

# k-means clustering

num_clusters = 4
kmeans_cluster = kmeans(data, centers = num_clusters)
cluster_assignments <- kmeans_cluster$cluster


plot(data, col = cluster_assignments, pch = 19, main = "Simulated Data with Clusters")
points(kmeans_result$centers, col = 1:num_clusters, pch = 3, cex = 2)
legend("topright", legend = paste("Cluster", 1:num_clusters), col = 1:num_clusters, pch = 19)