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

# This r code deals with the case of having 2 dimensional data and clustering it into 2 clusters. Please see the other scripts
# to find the other cases, those being 2 dimensional data with 4 clusters and 4 dimensional data with 2 and 4 clusters respectively


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

dataAmelia10Imputed = list()
dataAmelia10Imputed1 = list()
dataAmelia10Imputed2 = list()
dataAmelia10Imputed3 = list()
dataAmelia10Imputed4 = list()
dataAmelia10Imputed5 = list()
dataAmelia20Imputed = list()
dataAmelia40Imputed = list()

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

rMean = as.data.frame(0)
rMean10 = as.data.frame(0)
rMean20 = as.data.frame(0)
rMean40 = as.data.frame(0)

rRanger = as.data.frame(0)
rRanger10 = as.data.frame(0)
rRanger20 = as.data.frame(0)
rRanger40 = as.data.frame(0)

rPMM = as.data.frame(0)
rPMM10 = as.data.frame(0)
rPMM20 = as.data.frame(0)
rPMM40 = as.data.frame(0)


jaccardMean10 = as.data.frame(0)
jaccardMean20 = as.data.frame(0)
jaccardMean40 = as.data.frame(0)

jaccardRanger = as.data.frame(0)
jaccardRanger10 = as.data.frame(0)
jaccardRanger20 = as.data.frame(0)
jaccardRanger40 = as.data.frame(0)

jaccardPMM = as.data.frame(0)
jaccardPMM10 = as.data.frame(0)
jaccardPMM20 = as.data.frame(0)
jaccardPMM40 = as.data.frame(0)

ajrClv = as.data.frame(0)
ajrFossil = as.data.frame(0)
jaccardRanger = list()
jaccardPMM = list()


#### Help functions for data imputation and clustering

# imputing function for mean imputation. input: data frame with missing data, output: data frame with all NAs replaced by the column mean
imputeMean = function(data) {
  mean = colMeans(data, na.rm = TRUE)
  imputedData = replace_na(data, as.list(mean))
  return(imputedData)
}

# clustering function 
clusterData = function(data, numClusters = 4) {
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
  dataMissing10[[i]] = ampute(dataList[[i]], prop = 0.1, mech = "MAR")$amp
  dataMissing20[[i]] = ampute(dataList[[i]], prop = 0.2, mech = "MAR")$amp
  dataMissing40[[i]] = ampute(dataList[[i]], prop = 0.4, mech = "MAR")$amp
  # imputations start here. first mean imputation
  dataMean10[[i]] = imputeMean(dataMissing10[[i]])
  dataMean20[[i]] = imputeMean(dataMissing20[[i]])
  dataMean40[[i]] = imputeMean(dataMissing40[[i]])
  # missRanger imputation
  dataRanger10[[i]] = missRanger(dataMissing10[[i]])
  dataRanger20[[i]] = missRanger(dataMissing20[[i]])
  dataRanger40[[i]] = missRanger(dataMissing40[[i]])
  # mice imputation
  dataPMM10[[i]] = complete(mice(dataMissing10[[i]], method = "pmm", m = 1))
  dataPMM20[[i]] = complete(mice(dataMissing20[[i]], method = "pmm", m = 1))
  dataPMM40[[i]] = complete(mice(dataMissing40[[i]], method = "pmm", m = 1))
  # cluster creation starts here. first a baseline cluster without any missing data that the imputed data cluster get compared to
  clusterBaseline[[i]] = clusterData(dataList[[i]])
  # then cluster for mean imputation, missranger imputation and mice imputation
  clusterMean10[[i]] = clusterData(dataMean10[[i]])
  clusterMean20[[i]] = clusterData(dataMean20[[i]])
  clusterMean40[[i]] = clusterData(dataMean40[[i]])
  
  clusterRanger10[[i]] = clusterData(dataRanger10[[i]])
  clusterRanger20[[i]] = clusterData(dataRanger20[[i]])
  clusterRanger40[[i]] = clusterData(dataRanger40[[i]])
  
  clusterPMM10[[i]] = clusterData(dataPMM10[[i]])
  clusterPMM20[[i]] = clusterData(dataPMM20[[i]])
  clusterPMM40[[i]] = clusterData(dataPMM40[[i]])
  
  # calculating the jaccard indices comparing the clusters with the baseline cluster using the clv.Jaccard function
  # of the clv package
  
  
  jaccardMean10[i] = clv.Jaccard(std.ext(clusterBaseline[[i]]$cluster, clusterMean10[[i]]$cluster))
  jaccardMean20[i] = clv.Jaccard(std.ext(clusterBaseline[[i]]$cluster, clusterMean20[[i]]$cluster))
  jaccardMean40[i] = clv.Jaccard(std.ext(clusterBaseline[[i]]$cluster, clusterMean40[[i]]$cluster))
  
  jaccardRanger10[i] = clv.Jaccard(std.ext(clusterBaseline[[i]]$cluster, clusterRanger10[[i]]$cluster))
  jaccardRanger20[i] = clv.Jaccard(std.ext(clusterBaseline[[i]]$cluster, clusterRanger20[[i]]$cluster))
  jaccardRanger40[i] = clv.Jaccard(std.ext(clusterBaseline[[i]]$cluster, clusterRanger40[[i]]$cluster))
  
  jaccardPMM10[i] = clv.Jaccard(std.ext(clusterBaseline[[i]]$cluster, clusterPMM10[[i]]$cluster))
  jaccardPMM20[i] = clv.Jaccard(std.ext(clusterBaseline[[i]]$cluster, clusterPMM20[[i]]$cluster))
  jaccardPMM40[i] = clv.Jaccard(std.ext(clusterBaseline[[i]]$cluster, clusterPMM40[[i]]$cluster))
  
  # calculating the rand index in comparing the clusters with the baseline cluster using the clv.Rand function
  # of the clv package
  
  rMean10[i] = clv.Rand(std.ext(clusterBaseline[[i]]$cluster, clusterMean10[[i]]$cluster))
  rMean20[i] = clv.Rand(std.ext(clusterBaseline[[i]]$cluster, clusterMean20[[i]]$cluster))
  rMean40[i] = clv.Rand(std.ext(clusterBaseline[[i]]$cluster, clusterMean40[[i]]$cluster))
  
  rRanger10[i] = clv.Rand(std.ext(clusterBaseline[[i]]$cluster, clusterRanger10[[i]]$cluster))
  rRanger20[i] = clv.Rand(std.ext(clusterBaseline[[i]]$cluster, clusterRanger20[[i]]$cluster))
  rRanger40[i] = clv.Rand(std.ext(clusterBaseline[[i]]$cluster, clusterRanger40[[i]]$cluster))
  
  rPMM10[i] = clv.Rand(std.ext(clusterBaseline[[i]]$cluster, clusterPMM10[[i]]$cluster))
  rPMM20[i] = clv.Rand(std.ext(clusterBaseline[[i]]$cluster, clusterPMM20[[i]]$cluster))
  rPMM40[i] = clv.Rand(std.ext(clusterBaseline[[i]]$cluster, clusterPMM40[[i]]$cluster))
  
  
}



# turning the data frames of the jaccard indizes into the proper format, a long dataframe with 50 observables of 1 variable as opposed to 50 variables with 1 observation each

# function to pivot_longer dataframes using the tidyr function pivot_longer

pivot = function(data) {
  data = pivot_longer(data, everything(), names_to = "clusterNumber", values_to = "value")
  return(data)
}

jaccardMean10 = pivot(jaccardMean10)
jaccardMean20 = pivot(jaccardMean20)
jaccardMean40 = pivot(jaccardMean40)

jaccardRanger10 = pivot(jaccardRanger10)
jaccardRanger20 = pivot(jaccardRanger20)
jaccardRanger40 = pivot(jaccardRanger40)

jaccardPMM10 = pivot(jaccardPMM10)
jaccardPMM20 = pivot(jaccardPMM20)
jaccardPMM40 = pivot(jaccardPMM40)

rMean10 = pivot(rMean10)
rMean20 = pivot(rMean20)
rMean40 = pivot(rMean40)

rRanger10 = pivot(rRanger10)
rRanger20 = pivot(rRanger20)
rRanger40 = pivot(rRanger40)

rPMM10 = pivot(rPMM10)
rPMM20 = pivot(rPMM20)
rPMM40 = pivot(rPMM40)

# combining the jaccard indizes of each imputation method into one data frame for plotting purposes

jac = function(df1, df2, df3) {
  df1$clusterNumber = "10%"
  df2$clusterNumber = "20%"
  df3$clusterNumber = "40%"
  
  combinedData = bind_rows(df1, df2, df3)
  colnames(combinedData) = c("missing", "value")
  
  return(combinedData)
}

jaccardMean = jac(jaccardMean10, jaccardMean20, jaccardMean40)
jaccardPMM = jac(jaccardPMM10, jaccardPMM20, jaccardPMM40)
jaccardRanger = jac(jaccardRanger10, jaccardRanger20, jaccardRanger40)

randMean = jac(rMean10, rMean20, rMean40)
randPMM = jac(rPMM10, rPMM20, rPMM40)
randRanger = jac(rRanger10, rRanger20, rRanger40)


# function to plot the jaccard indizes and rand numbers in box plots

box = function(data, ylable, title, filename, ylimit = c(0.4, 0.9), color = "bisque2") {
  plot = ggplot(data, aes(x = as.factor(missing), y = value)) + geom_boxplot(fill = color, alpha = 0.3) + xlab("% of missing data") + ylab(ylable) + ggtitle(title) +ylim(ylimit)
  ggsave(filename = filename, path = "./2d4c/")
  return(plot)
}

box(jaccardMean, "Jaccard Index", "Boxplot of the Jaccard Indices for mean imputation", "meanJaccard.jpg")
box(jaccardPMM, "Jaccard Index", "Boxplot of the Jaccard Indices for imputation using mice", "meanPMM.jpg")
box(jaccardRanger, "Jaccard Index", "Boxplot of the Jaccard Indices for imputation using ranger", "meanRanger.jpg")


box(randMean, "Rand Index", "Boxplot of the Rand Indices for mean imputation", "randMean.jpg", c(0.6,1), "cadetblue2")
box(randPMM, "Rand Index", "Boxplot of the Rand Indices for imputation using mice", "randMice.jpg",c(0.6,1), "cadetblue2")
box(randRanger, "Rand Index", "Boxplot of the Rand Indices for imputation using ranger", "randRanger.jpg",c(0.6,1), "cadetblue2")



# calculating the averages of Indices to compare to the baseline cluster 

avgJacMean10 = mean(jaccardMean10$value)
avgJacMean20 = mean(jaccardMean20$value)
avgJacMean40 = mean(jaccardMean40$value)

avgJacPMM10 = mean(jaccardPMM10$value)
avgJacPMM20 = mean(jaccardPMM20$value)
avgJacPMM40 = mean(jaccardPMM40$value)

avgJacRanger10 = mean(jaccardRanger10$value)
avgJacRanger20 = mean(jaccardRanger20$value)
avgJacRanger40 = mean(jaccardRanger40$value)

avgRMean10 = mean(rMean10$value)
avgRMean20 = mean(rMean20$value)
avgRMean40 = mean(rMean40$value)

avgRPMM10 = mean(rPMM10$value)
avgRPMM20 = mean(rPMM20$value)
avgRPMM40 = mean(rPMM40$value)

avgRRanger10 = mean(rRanger10$value)
avgRRanger20 = mean(rRanger20$value)
avgRRanger40 = mean(rRanger40$value)


# plotting of arbitrary example clusters

baselinePlot = ggplot() + geom_point(data = clusterBaseline[[18]], mapping = aes(x = Var1, y = Var2, colour = cluster)) + labs(x = "Variable 1", y = "Variable 2") + xlim(-8,8) + ylim(5, 15) + ggtitle("k-means clustering of the data without any missing values or imputations")
ggsave("Baseline.jpg", path = "./2d4c/")

mean10Plot = ggplot() + geom_point(data = clusterMean10[[2]], mapping = aes(x = Var1, y = Var2, colour = cluster)) + labs(x = "Variable 1", y = "Variable 2") + xlim(-8,8) + ylim(5, 15) + ggtitle("k-means clustering of the dataset missing 10% of the data, using mean imputation")
ggsave("mean10.jpg", path = "./2d4c/")

mean20Plot = ggplot() + geom_point(data = clusterMean20[[41]], mapping = aes(x = Var1, y = Var2, colour = cluster)) + labs(x = "Variable 1", y = "Variable 2") + xlim(-8,8) + ylim(5, 15) + ggtitle("k-means clustering of the dataset missing 20% of the data, using mean imputation")
ggsave("mean20.jpg", path = "./2d4c/")

mean40Plot = ggplot() + geom_point(data = clusterMean40[[9]], mapping = aes(x = Var1, y = Var2, colour = cluster)) + labs(x = "Variable 1", y = "Variable 2") + xlim(-8,8) + ylim(5, 15) + ggtitle("k-means clustering of the dataset missing 40% of the data, using mean imputation")
ggsave("mean40.jpg", path = "./2d4c/")

ranger10Plot = ggplot() + geom_point(data = clusterRanger10[[4]], mapping = aes(x = Var1, y = Var2, colour = cluster)) + labs(x = "Variable 1", y = "Variable 2") + xlim(-8,8) + ylim(5, 15) + ggtitle("k-means clustering of the dataset missing 10% of the data, using mean random forest imputation with missRanger")
ggsave("ranger10.jpg", path = "./2d4c/")

ranger20Plot = ggplot() + geom_point(data = clusterRanger20[[6]], mapping = aes(x = Var1, y = Var2, colour = cluster)) + labs(x = "Variable 1", y = "Variable 2") + xlim(-8,8) + ylim(5, 15) + ggtitle("k-means clustering of the dataset missing 20% of the data, using mean random forest imputation with missRanger")
ggsave("ranger20.jpg", path = "./2d4c/")

ranger40Plot = ggplot() + geom_point(data = clusterRanger40[[2]], mapping = aes(x = Var1, y = Var2, colour = cluster)) + labs(x = "Variable 1", y = "Variable 2") + xlim(-8,8) + ylim(5, 15) + ggtitle("k-means clustering of the dataset missing 40% of the data, using mean random forest imputation with missRanger")
ggsave("ranger40.jpg", path = "./2d4c/")

pmm10plot = ggplot() + geom_point(data = clusterPMM10[[21]], mapping = aes(x = Var1, y = Var2, colour = cluster)) + labs(x = "Variable 1", y = "Variable 2") + xlim(-8,8) + ylim(5, 15) + ggtitle("k-means clustering of the dataset missing 10% of the data, using mice")
ggsave("mice10.jpg", path = "./2d4c/")

pmm20plot = ggplot() + geom_point(data = clusterPMM20[[13]], mapping = aes(x = Var1, y = Var2, colour = cluster)) + labs(x = "Variable 1", y = "Variable 2") + xlim(-8,8) + ylim(5, 15) + ggtitle("k-means clustering of the dataset missing 20% of the data, using mice")
ggsave("mice20.jpg", path = "./2d4c/")

pmm40plot = ggplot() + geom_point(data = clusterPMM10[[21]], mapping = aes(x = Var1, y = Var2, colour = cluster)) + labs(x = "Variable 1", y = "Variable 2") + xlim(-8,8) + ylim(5, 15) + ggtitle("k-means clustering of the dataset missing 40% of the data, using mice")
ggsave("mice40.jpg", path = "./2d4c/")

