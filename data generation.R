####### Data generation ########
library(mvtnorm)
library(cluster)
library(missForest)

set.seed(123)

#### bivariate normal distribution with 2 clusters

# parameters for generating the data set


# number of observations
n = 400

# means matrix
mean = c(1, 10)

# covariance matrix
cov = matrix(c(5, 0, 0, 2), nrow = 2)

# generating the multivariate normal distribution

data = as.data.frame(rmvnorm(n = n, mean = mean, sigma = cov))
colnames(data) = c("Var 1", "Var 2")

# deleting a percentage of the data set to simulate 10%, 20% and 40% of missing data respectively
# the sample takes the amount of missing data of the data frame and will be used to delete the correlating indices of the data frame

# deleteData = function(data, percentage) {
#   unlisted = unlist(data)
#   missing10 = sample(1:length(unlisted), size = percentage * length(unlisted))
#   
#   unlistedSplit = split(unlisted, f = rep(1:2, length.out = length(unlisted)))
#   x = data.frame(Var1 = unlistedSplit$"1", Var2 = unlistedSplit$"2")
#   return(x)
# }

data = deleteData(data = data, percentage = 0.1)


unlisted = unlist(data)
missing10 = sample(1:length(unlisted), size = 0.1 * length(unlisted))
unlist(data)
unlisted[missing10] = NA

# split the unlisted vector to turn it back into a data frame
unlistedSplit = split(unlisted, f = rep(1:2, length.out = length(unlisted)))
dataMissing10 = data.frame(Var1 = unlistedSplit$"1", Var2 = unlistedSplit$"2")


## impute data

# missForest

imputedData = missForest::missForest(data)


# k-means clustering

num_clusters = 2
kmeans_cluster = kmeans(data, centers = num_clusters)
cluster_assignments <- kmeans_cluster$cluster


plot(data, col = cluster_assignments, pch = 19, main = "Simulated Data with Clusters")
points(kmeans_cluster$centers, col = 1:num_clusters, pch = 3, cex = 2)
legend("topright", legend = paste("Cluster", 1:num_clusters), col = 1:num_clusters, pch = 19)
