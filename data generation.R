####### Data generation ########
library(mvtnorm)
library(cluster)
library(missForest)
library(tidyverse)
library(mice)
library(Amelia)
library(ggplot2)
library(VIM)

set.seed(123)

#### bivariate normal distribution with 2 clusters

# parameters for generating the data set


# number of observations
n = 1000

# means matrix
mean = c(1, 10)

# covariance matrix
cov = matrix(c(5, 0, 0, 2), nrow = 2)

# generating the multivariate normal distribution

data = as.data.frame(rmvnorm(n = n, mean = mean, sigma = cov))
colnames(data) = c("Var1", "Var2")

# deleting a percentage of the data set to simulate 10%, 20% and 40% of missing data respectively
# the sample takes the amount of missing data of the data frame and will be used to delete the correlating indices of the data frame

deleteData = function(data, percentage) {
  unlisted = unlist(data)
  missing = sample(1:length(unlisted), size = percentage * length(unlisted))
  unlisted[missing] = NA

  unlistedSplit = split(unlisted, f = rep(1:2, length.out = length(unlisted)))
  x = data.frame(Var1 = unlistedSplit$"1", Var2 = unlistedSplit$"2")
  return(x)
}

dataMissing10 = prodNA(data, noNA = 0.1)
# dataMissing10 = deleteData(data = data, percentage = 0.1)
dataMissing20 = deleteData(data = data, percentage = 0.2)
dataMissing40 = deleteData(data = data, percentage = 0.4)

# unlisted = unlist(data)
# missing10 = sample(1:length(unlisted), size = 0.1 * length(unlisted))
# unlist(data)
# unlisted[missing10] = NA
# 
# # split the unlisted vector to turn it back into a data frame
# unlistedSplit = split(unlisted, f = rep(1:2, length.out = length(unlisted)))
# dataMissing10 = data.frame(Var1 = unlistedSplit$"1", Var2 = unlistedSplit$"2")


## impute data

# mean imputation

imputeMean = function(data) {
  mean = colMeans(data, na.rm = TRUE)
  imputedData = replace_na(data, as.list(mean))
  return(imputedData)
}

# ampute

dataMean = imputeMean(dataMissing10)

# random forest imputation with missForest

# dataForest = missForest(xmis = dataMissingTest, xtrue = data, verbose = TRUE)


# predictive mean matching with mice 

imputationPMM = mice(dataMissing10, method = "pmm", m = 1)
dataPMM = complete(imputationPMM)

# bootstrapping imputation with amelia package

imputationAmelia = amelia(dataMissing10, m = 1, trace = TRUE, max.resample = 1000)
summary(imputationAmelia)
view(imputationAmelia$imputations)
dataAmelia = as.data.frame(do.call(cbind, imputationAmelia$imputations))
imputationAmelia

summary(dataAmelia)
# check if any NAs were missed

any(is.na(dataMean))
# any(is.na(dataForest))
any(is.na(dataPMM))
any(is.na(dataAmelia))


# k-means clustering
numClusters = 2

clusterData = function(data, numClusters = 2) {
  kmeansCluster = kmeans(data, centers = numClusters)
  data$cluster = as.character(kmeansCluster$cluster)
  return(data)
}

# baseline data frame with no missing data
clusterBaseline = clusterData(data)

# clustering for mean imputation
clusterMean = clusterData(dataMean)

# clustering for random forest
# clusterForest = clusterData(dataForest)

# clustering for PMM 
clusterPMM = clusterData(dataPMM)

#clustering for Amelia
clusterAmelia = clusterData(dataAmelia)

ggplot() + geom_point(data = clusterBaseline, mapping = aes(x = Var1, y = Var2, colour = cluster))
ggplot() + geom_point(data = clusterMean, mapping = aes(x = Var1, y = Var2, colour = cluster)) 
ggplot() + geom_point(data = clusterPMM, mapping = aes(x = Var1, y = Var2, colour = cluster)) 
ggplot() + geom_point(data = clusterAmelia, mapping = aes(x = Var1, y = Var2, colour = cluster))


