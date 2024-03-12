####### Data generation ########
library(mvtnorm)
library(cluster)
library(missRanger)
library(tidyverse)
library(mice)
library(Amelia)
library(ggplot2)
library(VIM)
library(missForest)
library(cluster)
library(mclust)
library(fpc)
library(clv)

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

# lists to store the generated data
dataList = list()
dataMissing10 = list()
dataMissing20 = list()
dataMissing40 = list()

dataMean10 = list()
dataMean20 = list()
dataMean40 = list()

dataRanger10 = list()
dataRanger20 = list()
dataRanger40 = list()

dataPMM10 = list()
dataPMM20 = list()
dataPMM40 = list()

dataAmelia10 = list()
dataAmelia20 = list()
dataAmelia40 = list()

clusterBaseline = list()

clusterMean10 = list()
clusterMean20 = list()
clusterMean40 = list()

clusterRanger10 = list()
clusterRanger20 = list()
clusterRanger40 = list()

clusterPMM10 = list()
clusterPMM20 = list()
clusterPMM40 = list()

clusterAmelia10 = list()
clusterAmelia20 = list()
clusterAmelia40 = list()

ajrMean = as.data.frame(0)
ajrMean10 = as.data.frame(0)
ajrMean20 = as.data.frame(0)
ajrMean40 = as.data.frame(0)

ajrRanger = as.data.frame(0)
ajrRanger10 = as.data.frame(0)
ajrRanger20 = as.data.frame(0)
ajrRanger40 = as.data.frame(0)

ajrPMM = as.data.frame(0)
ajrPMM10 = as.data.frame(0)
ajrPMM20 = as.data.frame(0)
ajrPMM40 = as.data.frame(0)

jaccardMean10 = as.data.frame(0)
jaccardMean20 = as.data.frame(0)
jaccardMean40 = as.data.frame(0)

jaccardRanger10 = as.data.frame(0)
jaccardRanger20 = as.data.frame(0)
jaccardRanger40 = as.data.frame(0)

jaccardPMM10 = as.data.frame(0)
jaccardPMM20 = as.data.frame(0)
jaccardPMM40 = as.data.frame(0)

ajrClv = as.data.frame(0)
ajrFossil = as.data.frame(0)
jaccardRanger = list()
jaccardPMM = list()

silValuesMean = list()
silValuesRanger = list()
silValuesPMM = list()


#### Help functions for data imputation and clustering

# imputing function for mean imputation. input: data frame with missing data, output: data frame with all NAs replaced by the column mean
imputeMean = function(data) {
  mean = colMeans(data, na.rm = TRUE)
  imputedData = replace_na(data, as.list(mean))
  return(imputedData)
}

# clustering function 
clusterData = function(data, numClusters = 2) {
  kmeansCluster = kmeans(data, centers = numClusters)
  data$cluster = as.character(kmeansCluster$cluster)
  return(data)
}

# function to calculate the jaccard index between the baseline cluster and a cluster with imputed data
jaccard = function(data, imputedData) {
  intersection = length(intersect(data, imputedData))
  union = length(union(data, imputedData))
  
  jaccard = intersection / union
  return(jaccard)
}

# function to calculate silhouette values of a cluster
sil = function(clusteredData) {
  silValues = silhouette(clusteredData$cluster, dist(clusteredData))
  meanSilValues = mean(silValues[, "sil_width"])
  return(meanSilValues)
}


for (i in 1:50) {
  data = as.data.frame(rmvnorm(n = n, mean = mean, sigma = cov))
  colnames(data) = c("Var1", "Var2")
  
  # Append data to the list
  dataList[[i]] = data
  
  # deleting 10%/20%/40% of the data of each data frame in dataList and saving it, again, in a list
  dataMissing10[[i]] = prodNA(dataList[[i]], noNA = 0.1)
  dataMissing20[[i]] = prodNA(dataList[[i]], noNA = 0.2)
  dataMissing40[[i]] = prodNA(dataList[[i]], noNA = 0.4)
  
  dataMean10[[i]] = imputeMean(dataMissing10[[i]])
  dataMean20[[i]] = imputeMean(dataMissing20[[i]])
  dataMean40[[i]] = imputeMean(dataMissing40[[i]])

  dataRanger10[[i]] = missRanger(dataMissing10[[i]])
  dataRanger20[[i]] = missRanger(dataMissing20[[i]])
  dataRanger40[[i]] = missRanger(dataMissing40[[i]])
  
  dataPMM10[[i]] = complete(mice(dataMissing10[[i]], method = "pmm", m = 1))
  dataPMM20[[i]] = complete(mice(dataMissing20[[i]], method = "pmm", m = 1))
  dataPMM40[[i]] = complete(mice(dataMissing40[[i]], method = "pmm", m = 1))
  
  clusterBaseline[[i]] = clusterData(dataList[[i]])
  
  clusterMean10[[i]] = clusterData(dataMean10[[i]])
  clusterMean20[[i]] = clusterData(dataMean20[[i]])
  clusterMean40[[i]] = clusterData(dataMean40[[i]])
  
  clusterRanger10[[i]] = clusterData(dataRanger10[[i]])
  clusterRanger20[[i]] = clusterData(dataRanger20[[i]])
  clusterRanger40[[i]] = clusterData(dataRanger40[[i]])
  
  clusterPMM10[[i]] = clusterData(dataPMM10[[i]])
  clusterPMM20[[i]] = clusterData(dataPMM20[[i]])
  clusterPMM40[[i]] = clusterData(dataPMM40[[i]])
  
  # clusterAmelia10[[i]] = clusterData(dataAmelia10[[i]])
  # clusterAmelia20[[i]] = clusterData(dataAmelia20[[i]])
  # clusterAmelia40[[i]] = clusterData(dataAmelia40[[i]])
  
  # adjustedRandIndexMean10[[i]] = adjustedRandIndex(clusterBaseline[[i]]$cluster, clusterMean10[[i]]$cluster)
  # adjustedRandIndexMean20[[i]] = adjustedRandIndex(clusterBaseline[[i]]$cluster, clusterMean20[[i]]$cluster)
  # adjustedRandIndexMean40[[i]] = adjustedRandIndex(clusterBaseline[[i]]$cluster, clusterMean40[[i]]$cluster)
  # 
  # adjustedRandIndexRanger10[[i]] = adjustedRandIndex(clusterBaseline[[i]]$cluster, clusterRanger10[[i]]$cluster)
  # adjustedRandIndexRanger20[[i]] = adjustedRandIndex(clusterBaseline[[i]]$cluster, clusterRanger20[[i]]$cluster)
  # adjustedRandIndexRanger40[[i]] = adjustedRandIndex(clusterBaseline[[i]]$cluster, clusterRanger40[[i]]$cluster)
  # 
  # adjustedRandIndexPMM10[[i]] = adjustedRandIndex(clusterBaseline[[i]]$cluster, clusterPMM10[[i]]$cluster)
  # adjustedRandIndexPMM20[[i]] = adjustedRandIndex(clusterBaseline[[i]]$cluster, clusterPMM20[[i]]$cluster)
  # adjustedRandIndexPMM40[[i]] = adjustedRandIndex(clusterBaseline[[i]]$cluster, clusterPMM40[[i]]$cluster)
  
  ajrMean10[i] = adjustedRandIndex(clusterBaseline[[i]]$cluster, clusterMean10[[i]]$cluster)
  ajrMean20[i] = adjustedRandIndex(clusterBaseline[[i]]$cluster, clusterMean20[[i]]$cluster)
  ajrMean40[i] = adjustedRandIndex(clusterBaseline[[i]]$cluster, clusterMean40[[i]]$cluster)
  
  ajrRanger10[i] = adjustedRandIndex(clusterBaseline[[i]]$cluster, clusterRanger10[[i]]$cluster)
  ajrRanger20[i] = adjustedRandIndex(clusterBaseline[[i]]$cluster, clusterRanger20[[i]]$cluster)
  ajrRanger40[i] = adjustedRandIndex(clusterBaseline[[i]]$cluster, clusterRanger40[[i]]$cluster)
  
  ajrPMM10[i] = adjustedRandIndex(clusterBaseline[[i]]$cluster, clusterPMM10[[i]]$cluster)
  ajrPMM20[i] = adjustedRandIndex(clusterBaseline[[i]]$cluster, clusterPMM20[[i]]$cluster)
  ajrPMM40[i] = adjustedRandIndex(clusterBaseline[[i]]$cluster, clusterPMM40[[i]]$cluster)
  
  jaccardMean10[i] = clv.Jaccard(std.ext(clusterBaseline[[i]]$cluster, clusterMean10[[i]]$cluster))
  jaccardMean20[i] = clv.Jaccard(std.ext(clusterBaseline[[i]]$cluster, clusterMean20[[i]]$cluster))
  jaccardMean40[i] = clv.Jaccard(std.ext(clusterBaseline[[i]]$cluster, clusterMean40[[i]]$cluster))
  
  
  # ajrClv[i] = clv.Rand(std.ext(clusterBaseline[[i]]$cluster, clusterMean10[[i]]$cluster))
  # ajrFossil[i] = rand.index(clusterBaseline[[i]], clusterMean10[[i]])
  jaccardRanger10[i] = clv.Jaccard(std.ext(clusterBaseline[[i]]$cluster, clusterRanger10[[i]]$cluster))
  jaccardRanger20[i] = clv.Jaccard(std.ext(clusterBaseline[[i]]$cluster, clusterRanger20[[i]]$cluster))
  jaccardRanger40[i] = clv.Jaccard(std.ext(clusterBaseline[[i]]$cluster, clusterRanger40[[i]]$cluster))
  
  jaccardPMM10[i] = clv.Jaccard(std.ext(clusterBaseline[[i]]$cluster, clusterPMM10[[i]]$cluster))
  jaccardPMM20[i] = clv.Jaccard(std.ext(clusterBaseline[[i]]$cluster, clusterPMM20[[i]]$cluster))
  jaccardPMM40[i] = clv.Jaccard(std.ext(clusterBaseline[[i]]$cluster, clusterPMM40[[i]]$cluster))
  # silValuesMean[[i]] = sil(clusterMean10[[i]])
  
  
  
  # jaccardMean[[i]] = jaccard(clusterBaseline[[i]]$cluster, clusterMean10[[i]]$cluster)
  
  
  # silValues[i] = cluster.stats(clusterMean10[[i]]$cluster)
}

# average adjusted rand indexes

ajrMean10Average = rowMeans(ajrmean10)
ajrMean20Average = rowMeans(ajrmean20)
ajrMean40Average = rowMeans(ajrmean40)

ajrRanger10Average = rowMeans(ajrRanger10)
ajrRanger20Average = rowMeans(ajrRanger20)
ajrRanger40Average = rowMeans(ajrRanger40)

ajrPMM10Average = rowMeans(ajrPMM10)
ajrPMM20Average = rowMeans(ajrPMM20)
ajrPMM40Average = rowMeans(ajrPMM40)


summary(adjustedRandIndexMean10)


## impute data

# mean imputation

imputeMean = function(data) {
  mean = colMeans(data, na.rm = TRUE)
  imputedData = replace_na(data, as.list(mean))
  return(imputedData)
}

dataImputeMean = list()
dataImputeRanger = list()
dataImputePMM = list()
dataImputeAmelia = list()

for (i in 1:50) {
  dataImputeMean[[i]] = imputeMean(dataMissing10[[i]])
}

# ampute

dataMean = imputeMean(dataMissing10)

# random forest imputation with missRanger

dataRanger = missRanger(dataMissing10)


# predictive mean matching with mice 

imputationPMM = mice(dataMissing10, method = "pmm", m = 1)
dataPMM = complete(imputationPMM)

# bootstrapping imputation with amelia package

imputationAmelia = amelia(dataMissing40[[1]], m = 1, trace = TRUE, max.resample = 1000, burnin = 100)
summary(imputationAmelia)
# view(imputationAmelia$imputations)
dataAmelia = na.omit(as.data.frame(do.call(cbind, imputationAmelia$imputations)))
imputationAmelia

summary(dataAmelia)
# check if any NAs were missed

any(is.na(dataMean))
any(is.na(dataRanger))
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
clusterForest = clusterData(dataRanger)

# clustering for PMM 
clusterPMM = clusterData(dataPMM)

#clustering for Amelia
clusterAmelia = clusterData(dataAmelia)

ggplot() + geom_point(data = clusterBaseline, mapping = aes(x = Var1, y = Var2, colour = cluster))
ggplot() + geom_point(data = clusterMean, mapping = aes(x = Var1, y = Var2, colour = cluster)) 
ggplot() + geom_point(data = clusterPMM, mapping = aes(x = Var1, y = Var2, colour = cluster)) 
ggplot() + geom_point(data = clusterAmelia, mapping = aes(x = Var1, y = Var2, colour = cluster))

jaccard <- function(a, b) {
  intersection = length(intersect(a, b))
  union = length(a) + length(b) - intersection
  return (intersection/union)
}

jaccard(clusterBaseline, clusterPMM)

adjustedRandIndex(clusterBaseline$cluster, clusterPMM$cluster)
adjustedRandIndex(clusterBaseline$cluster, clusterMean$cluster)
adjustedRandIndex(clusterBaseline$cluster, clusterForest$cluster)
adjustedRandIndex(clusterBaseline$cluster, clusterAmelia$cluster)
as.set(clusterBaseline$cluster)


n <- 1000

# Means vector
means <- c(0, 5, 10, 15)

# Standard deviations vector
sds <- c(1, 2, 3, 4)

# Generating the multivariate normal distribution
four_dimensional_data <- data.frame(
  Var1 = rnorm(n, mean = means[1], sd = sds[1]),
  Var2 = rnorm(n, mean = means[2], sd = sds[2]),
  Var3 = rnorm(n, mean = means[3], sd = sds[3]),
  Var4 = rnorm(n, mean = means[4], sd = sds[4])
)

fourD = prodNA(four_dimensional_data, 0.1)

imputationAmelia = amelia(fourD, m = 1, trace = TRUE, max.resample = 1000, burnin = 100)
summary(imputationAmelia)
# view(imputationAmelia$imputations)
data4d = as.data.frame(imputationAmelia$imputations)
dataAmelia = na.omit(as.data.frame(do.call(cbind, imputationAmelia$imputations)))
imputationAmelia
summary(data4d)

df1 = clusterBaseline[[1]]
df2 = clusterMean10[[1]]

clv = clv.Rand(std.ext(df1$cluster, df2$cluster))
fossil = rand.index(as.matrix(df1), as.matrix(df2))
as.matrix(df1)
